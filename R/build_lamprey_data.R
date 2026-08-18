# build_lamprey_data.R
# -------------------------------------------------------------
# Transforms the raw PTAGIS pulls (from R/fetch_ptagis.R) into
# the CSVs the Shiny app actually reads:
#   data/releases.csv        <- translocated_lamprey_cth.csv
#                                 + rt_study_tagging.csv
#   data/detections.csv      <- translocated_lamprey_interrogation.csv
#                                 + rt_study_interrogation.csv
#                                 + data/releases.csv
#   data/juv_releases.csv    <- juv_lamprey_tagging_detail.csv
#   data/juv_detections.csv  <- juv_lamprey_interrogation.csv
#                                 + data/juv_releases.csv
#
# 2026 RT (radiotelemetry) study fish: adult Pacific Lamprey
# trapped/released at Wells Hatchery, a subset of which also got an
# external radio tag alongside their PIT tag (rt_study_tagging.csv's
# radio_tag column is a numeric ID for those fish, NA for PIT-only
# fish). They're folded into the regular adult releases.csv /
# detections.csv -- so they show up on the existing Map tab like any
# other adult fish -- with two extra columns, radio_tag and
# tag_type ("PIT+RT" / "PIT only"), that the "2026 Radio Telemetry
# Study" tab uses to tell the two groups apart. Pre-existing
# (non-RT-study) adult rows get radio_tag/tag_type = NA. The
# rt_study_*.csv files are optional -- if they haven't been fetched
# yet, the adult data just builds as before.
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

# 2026 RT study inputs (optional -- see header note above).
# rt_study_interrogation.csv shares the exact same column layout as
# translocated_lamprey_interrogation.csv, so it can just be
# row-bound onto interr_adult before the rest of the (unmodified)
# adult-detections pipeline runs.
rt_tag_fp    <- path(data_dir, "rt_study_tagging.csv")
rt_interr_fp <- path(data_dir, "rt_study_interrogation.csv")
rt_tag       <- if (file_exists(rt_tag_fp))
  read_csv(rt_tag_fp, show_col_types = FALSE) else NULL
if (file_exists(rt_interr_fp))
  interr_adult <- bind_rows(interr_adult,
                             read_csv(rt_interr_fp, show_col_types = FALSE))

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

# ---- 2026 RT study releases (merged into releases.csv) ------------
# rt_study_tagging.csv is a "Tagging Detail" export (mark_site /
# release_site / release_date given directly, no event_* columns),
# so it needs its own -- simpler -- transform rather than reusing
# the CTH one above. There's no hand-corrected override file for
# these brand-new tags, so release coordinates always come from the
# site-metadata fallback (same generic-but-always-available lookup
# the CTH path uses for un-overridden tags).
if (!is.null(rt_tag)) {
  rt_releases <- rt_tag |>
    mutate(tag_code = tag,
           release_site_code_raw = extract_site_code(release_site)) |>
    left_join(sites_lookup, by = c("release_site_code_raw" = "site_code")) |>
    mutate(
      release_site = extract_site_label_after_dash(release_site),
      release_year = year(mdy(release_date)),
      radio_tag    = suppressWarnings(as.numeric(radio_tag)),
      tag_type     = if_else(!is.na(radio_tag), "PIT+RT", "PIT only"),
      release_lat  = fallback_lat,
      release_lon  = fallback_lon
    ) |>
    select(tag_code, release_site, release_date, release_year,
           release_lat, release_lon, radio_tag, tag_type)

  # RT study fish are trapped/marked at Wells Hatchery as part of the
  # same trapping effort the general "Translocated Lamprey" CTH query
  # covers, so every RT tag also shows up as a plain CTH row above
  # with no radio_tag info. Drop those plain duplicates so each tag
  # appears once, with the richer RT record winning.
  releases <- bind_rows(
    releases |> filter(!tag_code %in% rt_releases$tag_code) |>
      mutate(radio_tag = NA_real_, tag_type = NA_character_),
    rt_releases
  )
} else {
  releases <- releases |> mutate(radio_tag = NA_real_, tag_type = NA_character_)
}

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
  distinct(tag, site, first_time, last_time, .keep_all = TRUE) |>
  select(-release_site, -release_date) |>   # PTAGIS's own uncorrected fields
  inner_join(releases_key, by = c("tag" = "tag_code")) |>
  mutate(
    tag_code             = tag,
    detection_site_code  = extract_site_code(site),
    detection_site_label = site,
    first_detection      = parse_ptagis_datetime(first_time),
    last_detection       = parse_ptagis_datetime(last_time),
    last_antenna         = last_antenna_group
  ) |>
  select(tag_code, release_site, release_date, release_year,
         release_lat, release_lon, radio_tag, tag_type,
         detection_site_code, detection_site_label,
         first_detection, last_detection, last_antenna, count)

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
if (!is.null(rt_tag))
  cat(sprintf("  (of which %d are 2026 RT study fish: %d PIT+RT, %d PIT only)\n",
              sum(!is.na(releases$tag_type)),
              sum(releases$tag_type == "PIT+RT", na.rm = TRUE),
              sum(releases$tag_type == "PIT only", na.rm = TRUE)))
