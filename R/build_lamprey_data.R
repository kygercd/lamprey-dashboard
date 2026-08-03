# build_lamprey_data.R
# -------------------------------------------------------------
# Transforms the raw PTAGIS pulls (from R/fetch_ptagis.R) into
# the CSVs the Shiny app actually reads:
#   data/releases.csv        <- translocated_lamprey_cth.csv
#   data/detections.csv      <- translocated_lamprey_interrogation.csv
#                                 + data/releases.csv
#   data/juv_releases.csv    <- juv_lamprey_tagging_detail.csv
#   data/juv_detections.csv  <- juv_lamprey_interrogation.csv
#                                 + data/juv_releases.csv
#
# Must run AFTER fetch_ptagis.R and build_site_metadata.R.
#
# Release-site precision: PTAGIS's own "release site" coordinate
# on the Complete Tag History export is the generic site record
# (e.g. the Methow River mouth) rather than the actual translocation
# drop point. config/lamprey_release_site_overrides.csv is a
# static, hand-corrected snapshot (built once from a manually
# QA'd tag history export) that supplies the true release
# site/lat/lon for tags it covers. Tags not in that table (i.e.
# new tags added since the override file was built) fall back to
# PTAGIS's own release-site fields -- less precise, but always
# available with no manual work required. release_date itself is
# NEVER taken from the override file; it always comes from the
# fresh PTAGIS pull.
# -------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(fs)
})

root     <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                                   rprojroot::has_dir(".github"))
data_dir <- path(root, "data")

# ---- Helpers --------------------------------------------------
extract_site_code <- function(x) trimws(str_extract(x, "^[^ ]+"))
extract_site_label_after_dash <- function(x) str_remove(x, "^[^ ]+ - ")

build_sites <- function(meta, supp) {
  bind_rows(supp, meta) |>                 # supplemental first -> wins ties
    filter(!is.na(site_code), nzchar(site_code)) |>
    distinct(site_code, .keep_all = TRUE)
}

parse_ptagis_datetime <- function(x) {
  parsed <- suppressWarnings(lubridate::mdy_hms(x, quiet = TRUE))
  format(parsed, "%Y-%m-%d %H:%M:%S")
}

# ---- Load inputs ------------------------------------------------
cth          <- read_csv(path(data_dir, "translocated_lamprey_cth.csv"),
                          show_col_types = FALSE)
interr_adult <- read_csv(path(data_dir, "translocated_lamprey_interrogation.csv"),
                          show_col_types = FALSE)
juv_tag      <- read_csv(path(data_dir, "juv_lamprey_tagging_detail.csv"),
                          show_col_types = FALSE)
interr_juv   <- read_csv(path(data_dir, "juv_lamprey_interrogation.csv"),
                          show_col_types = FALSE)

site_meta <- read_csv(path(root, "config", "site_metadata.csv"),
                       show_col_types = FALSE)
site_supp <- read_csv(path(root, "config", "site_coords_supplemental.csv"),
                       show_col_types = FALSE)
overrides <- read_csv(path(root, "config", "lamprey_release_site_overrides.csv"),
                       show_col_types = FALSE)

sites <- build_sites(site_meta, site_supp)

# ---- data/releases.csv -------------------------------------------
# NOTE: the live PTAGIS "Complete Tag History" query is currently
# configured with only 8 output columns (tag/event_type/event_site/
# event_date/event_release_site/event_release_date/mark_date/
# cth_count) -- no lat/lon or comment fields. So the fallback for
# tags NOT in the override table can't read lat/lon off the fresh
# pull directly; instead it looks up the release site's code (the
# leading token of event_release_site, e.g. "METHR" out of "METHR -
# Methow River") against the combined PTAGIS site list. This is the
# same generic-but-always-available coordinate the site itself
# would report, just fetched via config/site_metadata.csv instead
# of a query column.
overrides_slim <- overrides |>
  select(tag_code,
         ovr_site = event_release_site_name,
         ovr_lat  = event_release_site_latitude_value,
         ovr_lon  = event_release_site_longitude_value) |>
  distinct(tag_code, .keep_all = TRUE)

sites_lookup <- sites |>
  select(site_code, fallback_lat = latitude, fallback_lon = longitude)

releases <- cth |>
  mutate(tag_code = tag,
         release_site_code_raw = extract_site_code(event_release_site)) |>
  left_join(overrides_slim, by = "tag_code") |>
  left_join(sites_lookup, by = c("release_site_code_raw" = "site_code")) |>
  mutate(
    release_site = if_else(!is.na(ovr_site), ovr_site,
                            extract_site_label_after_dash(event_release_site)),
    release_lat  = if_else(!is.na(ovr_lat), ovr_lat, fallback_lat),
    release_lon  = if_else(!is.na(ovr_lon), ovr_lon, fallback_lon),
    release_date = event_release_date,
    release_year = year(mdy(event_release_date))
  ) |>
  select(tag_code, release_site, release_date, release_year,
         release_lat, release_lon)

write_csv(releases, path(data_dir, "releases.csv"))

# ---- data/detections.csv -----------------------------------------
releases_key <- releases |> distinct(tag_code, .keep_all = TRUE)

unmatched <- setdiff(unique(interr_adult$tag), releases_key$tag_code)
if (length(unmatched) > 0) {
  message(sprintf(
    "!! %d detection tag(s) have no matching release record, dropping: %s",
    length(unmatched),
    paste(head(unmatched, 10), collapse = ", ")))
}

detections <- interr_adult |>
  select(-release_site, -release_date) |>   # PTAGIS's own uncorrected fields
  inner_join(releases_key, by = c("tag" = "tag_code")) |>
  mutate(
    tag_code             = tag,
    detection_site_code  = extract_site_code(site),
    detection_site_label = site,
    first_detection      = parse_ptagis_datetime(first_time),
    last_detection       = parse_ptagis_datetime(last_time)
  ) |>
  select(tag_code, release_site, release_date, release_year,
         release_lat, release_lon, detection_site_code,
         detection_site_label, first_detection, last_detection, count)

write_csv(detections, path(data_dir, "detections.csv"))

# ---- data/juv_releases.csv ---------------------------------------
juv_releases <- juv_tag |>
  mutate(
    tag_code = tag,
    life_stage = case_when(
      str_detect(comment, "AMMOCOETE")     ~ "Larvae",
      str_detect(comment, "MACROPHTHALMIA") ~ "Macropthalmia (eyed juvenile)",
      TRUE                                  ~ "Unknown"
    ),
    release_site_label = release_site,
    release_site_code  = extract_site_code(mark_site),
    release_site_name  = extract_site_label_after_dash(mark_site),
    release_year       = year(mdy(release_date)),
    length_mm          = length
  ) |>
  left_join(sites |> select(site_code, release_lat = latitude,
                            release_lon = longitude),
            by = c("release_site_code" = "site_code")) |>
  select(tag_code, life_stage, release_site_label, release_site_code,
         release_site_name, release_date, release_year,
         release_lat, release_lon, length_mm)

write_csv(juv_releases, path(data_dir, "juv_releases.csv"))

# ---- data/juv_detections.csv --------------------------------------
juv_releases_key <- juv_releases |> distinct(tag_code, .keep_all = TRUE)

unmatched_juv <- setdiff(unique(interr_juv$tag), juv_releases_key$tag_code)
if (length(unmatched_juv) > 0) {
  message(sprintf(
    "!! %d juvenile detection tag(s) have no matching release record, dropping: %s",
    length(unmatched_juv),
    paste(head(unmatched_juv, 10), collapse = ", ")))
}

juv_detections <- interr_juv |>
  select(-release_site, -release_date) |>   # PTAGIS's own uncorrected fields
  inner_join(juv_releases_key, by = c("tag" = "tag_code")) |>
  mutate(
    tag_code              = tag,
    detection_site_code   = extract_site_code(site),
    detection_site_label  = site,
    first_detection       = parse_ptagis_datetime(first_time),
    last_detection        = parse_ptagis_datetime(last_time)
  ) |>
  left_join(sites |> select(site_code, det_lat = latitude, det_lon = longitude),
            by = c("detection_site_code" = "site_code")) |>
  select(tag_code, life_stage, release_site_label, release_year,
         release_date, detection_site_label, detection_site_code,
         det_lat, det_lon, first_detection, last_detection, count)

write_csv(juv_detections, path(data_dir, "juv_detections.csv"))

cat(sprintf("\nWrote releases.csv: %d rows\n", nrow(releases)))
cat(sprintf("Wrote detections.csv: %d rows\n", nrow(detections)))
cat(sprintf("Wrote juv_releases.csv: %d rows\n", nrow(juv_releases)))
cat(sprintf("Wrote juv_detections.csv: %d rows\n", nrow(juv_detections)))
