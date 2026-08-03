# app.R
# Lamprey Dashboard
#
# Tabs:
#   Map         – adult releases / last detection, filter by year
#   Juveniles   – larvae & macropthalmia releases / last detection,
#                 filter by life stage + year
#   Wells Dam   – daily Pacific Lamprey passage counts (CBR DART)
#   Data status – file info

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(leaflet)
  library(plotly)
  library(DT)
})

# ---- Config -------------------------------------------------
GH_OWNER  <- "kygercd"
GH_REPO   <- "lamprey-dashboard"
GH_BRANCH <- "main"
GH_RAW    <- sprintf("https://raw.githubusercontent.com/%s/%s/%s",
                     GH_OWNER, GH_REPO, GH_BRANCH)

# ---- Path helpers -------------------------------------------
local_root <- {
  here <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                           rprojroot::has_dir(".github")),
    error = function(e) NULL
  )
  if (is.null(here)) getwd() else here
}

# Reads from data/ or config/ if present locally (dev, or a full
# repo checkout); otherwise falls back to the GitHub raw URLs, so
# the deployed shinyapps.io app (which only bundles app.R) picks
# up new data automatically without a redeploy.
load_csv <- function(rel_path, ...) {
  local_fp <- file.path(local_root, rel_path)
  if (file.exists(local_fp)) {
    return(suppressWarnings(readr::read_csv(local_fp,
                                            show_col_types = FALSE, ...)))
  }
  url <- paste0(GH_RAW, "/", rel_path)
  tryCatch(
    suppressWarnings(readr::read_csv(url, show_col_types = FALSE, ...)),
    error = function(e) {
      warning("Failed to load ", rel_path, ": ", conditionMessage(e))
      NULL
    }
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# ---- Date helpers -------------------------------------------
parse_dt <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  if (inherits(x, "POSIXt")) return(x)
  out <- suppressWarnings(lubridate::mdy_hms(x, quiet = TRUE,
                                             tz = "America/Los_Angeles"))
  miss <- is.na(out)
  if (any(miss)) {
    out2 <- suppressWarnings(lubridate::mdy(x[miss], quiet = TRUE))
    if (all(is.na(out2)))
      out2 <- suppressWarnings(lubridate::ymd_hms(x[miss], quiet = TRUE,
                                                  tz = "America/Los_Angeles"))
    if (all(is.na(out2)))
      out2 <- suppressWarnings(lubridate::ymd(x[miss], quiet = TRUE))
    out[miss] <- as.POSIXct(out2, tz = "America/Los_Angeles")
  }
  out
}

fmt_date <- function(x) {
  d <- suppressWarnings(as.Date(x))
  out <- format(d, "%m-%d-%Y"); out[is.na(d)] <- ""; out
}

# ---- Life stage constants -----------------------------------
JUV_STAGES        <- c("Larvae", "Macropthalmia (eyed juvenile)")
JUV_STAGE_COLORS  <- c("Larvae"                        = "#e6550d",
                        "Macropthalmia (eyed juvenile)" = "#756bb1")

# ---- Load data ----------------------------------------------
load_all <- function() {
  # Adult
  releases   <- load_csv("data/releases.csv")
  detections <- load_csv("data/detections.csv")
  # Juvenile
  juv_rel    <- load_csv("data/juv_releases.csv")
  juv_det    <- load_csv("data/juv_detections.csv")
  # Wells
  wells      <- load_csv("data/wells_lamprey_counts.csv")
  # Site metadata (for adult detection coords). Supplemental wins
  # on conflicts -- it covers a couple of MRR trap sites PTAGIS's
  # sites API doesn't list at all.
  meta      <- load_csv("config/site_metadata.csv")
  meta_supp <- load_csv("config/site_coords_supplemental.csv")
  if (!is.null(meta) || !is.null(meta_supp))
    meta <- bind_rows(meta_supp, meta) |>
      filter(!is.na(site_code), nzchar(site_code)) |>
      distinct(site_code, .keep_all = TRUE)

  # -- Adult releases --
  if (!is.null(releases))
    releases <- releases |>
      mutate(release_date = parse_dt(release_date),
             release_year = as.integer(release_year),
             release_lat  = suppressWarnings(as.numeric(release_lat)),
             release_lon  = suppressWarnings(as.numeric(release_lon)))

  # -- Adult detections (attach coords from metadata) --
  if (!is.null(detections)) {
    detections <- detections |>
      mutate(release_date    = parse_dt(release_date),
             release_year    = as.integer(release_year),
             release_lat     = suppressWarnings(as.numeric(release_lat)),
             release_lon     = suppressWarnings(as.numeric(release_lon)),
             first_detection = parse_dt(first_detection),
             last_detection  = parse_dt(last_detection),
             count           = suppressWarnings(as.integer(count)))
    if (!is.null(meta)) {
      coords <- meta |>
        select(site_code, det_lat = latitude, det_lon = longitude) |>
        filter(!is.na(site_code), nzchar(site_code))
      detections <- detections |>
        left_join(coords, by = c("detection_site_code" = "site_code"))
    }
  }

  # -- Juvenile releases --
  if (!is.null(juv_rel))
    juv_rel <- juv_rel |>
      mutate(release_date = parse_dt(release_date),
             release_year = as.integer(release_year),
             release_lat  = suppressWarnings(as.numeric(release_lat)),
             release_lon  = suppressWarnings(as.numeric(release_lon)),
             length_mm    = suppressWarnings(as.numeric(length_mm)),
             life_stage   = as.character(life_stage))

  # -- Juvenile detections --
  if (!is.null(juv_det)) {
    juv_det <- juv_det |>
      mutate(release_date    = parse_dt(release_date),
             release_year    = as.integer(release_year),
             first_detection = parse_dt(first_detection),
             last_detection  = parse_dt(last_detection),
             count           = suppressWarnings(as.integer(count)),
             det_lat         = suppressWarnings(as.numeric(det_lat)),
             det_lon         = suppressWarnings(as.numeric(det_lon)),
             life_stage      = as.character(life_stage))
  }

  # -- Wells counts --
  if (!is.null(wells))
    wells <- wells |>
      mutate(date           = as.Date(date),
             count          = suppressWarnings(as.integer(count)),
             count_10yr_avg = suppressWarnings(as.numeric(count_10yr_avg)),
             year           = lubridate::year(date),
             md             = format(date, "%m-%d")) |>
      filter(!is.na(date), !is.na(count))

  list(releases   = releases,   detections = detections,
       juv_rel    = juv_rel,    juv_det    = juv_det,
       wells      = wells)
}

raw <- load_all()

# ---- Year range helpers -------------------------------------
adult_years <- sort(unique(raw$releases$release_year), decreasing = TRUE)
juv_years   <- sort(unique(raw$juv_rel$release_year),  decreasing = TRUE)
wells_min   <- min(raw$wells$year, na.rm = TRUE)
wells_max   <- max(raw$wells$year, na.rm = TRUE)

# ---- Site summary helpers -----------------------------------
# Returns tibble with: site_code, site_name, lat, lon, n
site_summary_rel <- function(df) {
  df |>
    filter(!is.na(release_lat), !is.na(release_lon)) |>
    group_by(release_site, release_lat, release_lon) |>
    summarise(n = n_distinct(tag_code), .groups = "drop") |>
    transmute(site_code = release_site, site_name = release_site,
              lat = release_lat, lon = release_lon, n)
}

site_summary_det <- function(df) {
  df |>
    filter(!is.na(det_lat), !is.na(det_lon)) |>
    group_by(detection_site_code, detection_site_label, det_lat, det_lon) |>
    summarise(n = n_distinct(tag_code), .groups = "drop") |>
    rename(site_code = detection_site_code, site_name = detection_site_label,
           lat = det_lat, lon = det_lon)
}

site_summary_juv_rel <- function(df) {
  df |>
    filter(!is.na(release_lat), !is.na(release_lon)) |>
    group_by(release_site_code, release_site_name, release_lat, release_lon) |>
    summarise(n = n_distinct(tag_code), .groups = "drop") |>
    transmute(site_code = release_site_code, site_name = release_site_name,
              lat = release_lat, lon = release_lon, n)
}

site_summary_juv_det <- function(df) {
  df |>
    filter(!is.na(det_lat), !is.na(det_lon)) |>
    group_by(detection_site_code, detection_site_label, det_lat, det_lon) |>
    summarise(n = n_distinct(tag_code), .groups = "drop") |>
    rename(site_code = detection_site_code, site_name = detection_site_label,
           lat = det_lat, lon = det_lon)
}

# ---- Shared map renderer ------------------------------------
render_map_base <- function(map_id) {
  leaflet() |>
    addProviderTiles(providers$Esri.WorldTopoMap,  group = "Topo") |>
    addProviderTiles(providers$Esri.WorldImagery,  group = "Imagery") |>
    addLayersControl(baseGroups = c("Topo", "Imagery"),
                     options = layersControlOptions(collapsed = TRUE)) |>
    setView(lng = -120.0, lat = 48.1, zoom = 8)
}

# ---- UI -----------------------------------------------------
ui <- page_navbar(
  title   = "Lamprey Dashboard",
  theme   = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  # ===================== Adult Map tab =======================
  nav_panel(
    title = "Map",
    layout_sidebar(
      sidebar = sidebar(
        title = "Map options", width = 300,
        radioButtons("map_mode", "Mode:",
                     choices  = c("Last Detection" = "last",
                                  "Releases"        = "releases"),
                     selected = "last"),
        hr(),
        selectInput("year_filter", "Release year:",
                    choices  = c("All", as.character(adult_years)),
                    selected = "All"),
        hr(),
        textOutput("map_summary"),
        hr(),
        helpText("Click a bubble to see individual fish.",
                 "Click a row in the table to see that fish's full detection history.")
      ),
      card(full_screen = TRUE, height = "68vh", min_height = "460px",
           leafletOutput("map", height = "100%")),
      card(height = "30vh", min_height = "200px",
           card_header(textOutput("detail_header")),
           DTOutput("detail_table"))
    )
  ),

  # ===================== Juveniles tab =======================
  nav_panel(
    title = "Juveniles",
    layout_sidebar(
      sidebar = sidebar(
        title = "Juvenile options", width = 300,
        radioButtons("juv_mode", "Mode:",
                     choices  = c("Last Detection" = "last",
                                  "Releases"        = "releases"),
                     selected = "releases"),
        hr(),
        checkboxGroupInput(
          "juv_stages", "Life stage:",
          choices  = JUV_STAGES,
          selected = JUV_STAGES
        ),
        hr(),
        selectInput("juv_year", "Release year:",
                    choices  = c("All", as.character(juv_years)),
                    selected = "All"),
        hr(),
        textOutput("juv_map_summary"),
        hr(),
        helpText("Click a bubble to see individual fish.",
                 "Click a row in the table to see that fish's full detection history.")
      ),
      card(full_screen = TRUE, height = "68vh", min_height = "460px",
           leafletOutput("juv_map", height = "100%")),
      card(height = "30vh", min_height = "200px",
           card_header(textOutput("juv_detail_header")),
           DTOutput("juv_detail_table"))
    )
  ),

  # ===================== Wells Dam tab =======================
  nav_panel(
    title = "Wells Dam Counts",
    layout_sidebar(
      sidebar = sidebar(
        title = "Wells Dam – Pacific Lamprey", width = 280,
        helpText("Daily Pacific Lamprey passage counts at Wells Dam.",
                 "Source: CBR DART (adult_daily, project=WEL, LmpryDay)."),
        sliderInput("wells_year_range", "Year range (plots):",
                    min = wells_min, max = wells_max,
                    value = c(wells_min, wells_max), step = 1, sep = ""),
        hr(),
        selectInput("wells_table_year", "Table year:",
                    choices = rev(wells_min:wells_max), selected = wells_max)
      ),
      card(card_header("Annual totals"),
           plotlyOutput("wells_annual",   height = "280px")),
      card(card_header("Seasonal daily counts (overlay by year)"),
           plotlyOutput("wells_seasonal", height = "280px")),
      card(card_header(textOutput("wells_table_header")),
           DTOutput("wells_table"))
    )
  ),

  # ===================== Data status tab =====================
  nav_panel(
    title = "Data Status",
    card(
      card_header("Data files"),
      tags$ul(
        tags$li(sprintf("Adult releases: %s fish at %s sites",
                        format(nrow(raw$releases), big.mark = ","),
                        n_distinct(raw$releases$release_site))),
        tags$li(sprintf("Adult detections: %s records for %s fish at %s sites",
                        format(nrow(raw$detections), big.mark = ","),
                        n_distinct(raw$detections$tag_code),
                        n_distinct(raw$detections$detection_site_code))),
        tags$li(sprintf("Juvenile releases: %s fish (%s Larvae, %s Macropthalmia)",
                        format(nrow(raw$juv_rel), big.mark = ","),
                        sum(raw$juv_rel$life_stage == "Larvae"),
                        sum(raw$juv_rel$life_stage == "Macropthalmia (eyed juvenile)"))),
        tags$li(sprintf("Juvenile detections: %s records for %s fish",
                        format(nrow(raw$juv_det), big.mark = ","),
                        n_distinct(raw$juv_det$tag_code))),
        tags$li(sprintf("Wells Dam counts: %s days (%s – %s)",
                        format(nrow(raw$wells), big.mark = ","),
                        min(raw$wells$date), max(raw$wells$date)))
      )
    )
  )
)

# ---- Server -------------------------------------------------
server <- function(input, output, session) {

  # ============================================================
  # Adult Map
  # ============================================================

  releases_filt <- reactive({
    df <- raw$releases; if (is.null(df)) return(NULL)
    if (input$year_filter != "All")
      df <- df |> filter(release_year == as.integer(input$year_filter))
    df
  })

  last_det_filt <- reactive({
    df <- raw$detections; if (is.null(df)) return(NULL)
    if (input$year_filter != "All")
      df <- df |> filter(release_year == as.integer(input$year_filter))
    df |> group_by(tag_code) |>
      slice_max(last_detection, n = 1, with_ties = FALSE) |> ungroup()
  })

  adult_active <- reactive({
    if (input$map_mode == "last") last_det_filt() else releases_filt()
  })

  adult_bubbles <- reactive({
    df <- adult_active(); if (is.null(df) || nrow(df) == 0) return(NULL)
    if (input$map_mode == "last") site_summary_det(df)
    else                          site_summary_rel(df)
  })

  output$map_summary <- renderText({
    df <- adult_active(); if (is.null(df) || nrow(df) == 0) return("No data.")
    yr  <- input$year_filter
    lbl <- if (input$map_mode == "last") "fish with detections" else "fish released"
    n_sites <- if (input$map_mode == "last") n_distinct(df$detection_site_code)
               else n_distinct(df$release_site)
    sprintf("%s %s%s at %s site(s).",
            format(n_distinct(df$tag_code), big.mark = ","), lbl,
            if (yr == "All") "" else paste0(" in ", yr), n_sites)
  })

  output$map <- renderLeaflet(render_map_base("map"))
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  observe({
    df  <- adult_bubbles()
    prx <- leafletProxy("map") |> clearMarkers()
    if (is.null(df) || nrow(df) == 0) return()
    r   <- pmax(6, pmin(36, sqrt(df$n) * 3.2))
    col  <- if (input$map_mode == "last") "#2c7bb6" else "#1a9641"
    fill <- if (input$map_mode == "last") "#74c6f0" else "#a6d96a"
    prx |> addCircleMarkers(
      data = df, lng = ~lon, lat = ~lat, radius = r,
      color = col, weight = 1, fillColor = fill, fillOpacity = 0.75,
      layerId = ~site_code,
      label   = ~sprintf("%s – %d fish", site_name, n),
      popup   = ~sprintf("<b>%s</b><br/>%d fish", site_name, n))
  })

  adult_click <- reactiveVal(NULL)
  observeEvent(input$map_marker_click, adult_click(input$map_marker_click$id))
  observe({ input$map_mode; input$year_filter; adult_click(NULL) })

  output$detail_header <- renderText({
    sc <- adult_click()
    if (is.null(sc)) "Click a bubble on the map to see fish details."
    else             sprintf("Fish at: %s", sc)
  })

  adult_detail <- reactive({
    sc <- adult_click(); if (is.null(sc)) return(NULL)
    if (input$map_mode == "last") {
      last_det_filt() |> filter(detection_site_code == sc) |>
        transmute(tag_code, release_site,
                  release_date   = fmt_date(release_date),
                  release_year,
                  detection_site = detection_site_label,
                  last_detection = fmt_date(last_detection))
    } else {
      releases_filt() |> filter(release_site == sc) |>
        transmute(tag_code, release_site,
                  release_date = fmt_date(release_date), release_year)
    }
  })

  output$detail_table <- renderDT({
    df <- adult_detail()
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(), rownames = FALSE, selection = "single"))
    datatable(df, selection = "single", rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE,
                             order = list(list(0, "asc"))))
  })

  observeEvent(input$detail_table_rows_selected, {
    sel <- input$detail_table_rows_selected
    df  <- adult_detail()
    if (is.null(sel) || is.null(df) || sel > nrow(df)) return()
    tag <- df$tag_code[sel]
    hist <- raw$detections |> filter(tag_code == tag) |>
      arrange(first_detection) |>
      transmute(`Detection Site`  = detection_site_label,
                `First Detection` = fmt_date(first_detection),
                `Last Detection`  = fmt_date(last_detection),
                Count = count)
    showModal(modalDialog(
      title = paste("Detection history:", tag),
      DTOutput("adult_history_tbl"),
      easyClose = TRUE, footer = modalButton("Close"), size = "l"))
    output$adult_history_tbl <- renderDT(
      datatable(hist, rownames = FALSE,
                options = list(pageLength = 50, dom = "t", scrollX = TRUE)))
  })

  # ============================================================
  # Juvenile Map
  # ============================================================

  juv_rel_filt <- reactive({
    df <- raw$juv_rel; if (is.null(df)) return(NULL)
    stages <- input$juv_stages %||% JUV_STAGES
    if (input$juv_year != "All")
      df <- df |> filter(release_year == as.integer(input$juv_year))
    df |> filter(life_stage %in% stages)
  })

  juv_last_det_filt <- reactive({
    df <- raw$juv_det; if (is.null(df)) return(NULL)
    stages <- input$juv_stages %||% JUV_STAGES
    if (input$juv_year != "All")
      df <- df |> filter(release_year == as.integer(input$juv_year))
    df |> filter(life_stage %in% stages) |>
      group_by(tag_code) |>
      slice_max(last_detection, n = 1, with_ties = FALSE) |> ungroup()
  })

  juv_active <- reactive({
    if (input$juv_mode == "last") juv_last_det_filt() else juv_rel_filt()
  })

  juv_bubbles <- reactive({
    df <- juv_active(); if (is.null(df) || nrow(df) == 0) return(NULL)
    if (input$juv_mode == "last") site_summary_juv_det(df)
    else                          site_summary_juv_rel(df)
  })

  output$juv_map_summary <- renderText({
    df <- juv_active(); if (is.null(df) || nrow(df) == 0) return("No data.")
    stages <- input$juv_stages %||% JUV_STAGES
    lbl <- if (input$juv_mode == "last") "fish with detections" else "fish released"
    yr  <- input$juv_year
    stage_lbl <- if (setequal(stages, JUV_STAGES)) ""
                 else paste0(" (", paste(stages, collapse = " + "), ")")
    sprintf("%s %s%s%s at %s site(s).",
            format(n_distinct(df$tag_code), big.mark = ","), lbl,
            if (yr == "All") "" else paste0(" in ", yr),
            stage_lbl, n_distinct(df[[
              if (input$juv_mode == "last") "detection_site_code"
              else "release_site_code"]]))
  })

  output$juv_map <- renderLeaflet(render_map_base("juv_map"))
  outputOptions(output, "juv_map", suspendWhenHidden = FALSE)

  observe({
    df  <- juv_bubbles()
    prx <- leafletProxy("juv_map") |> clearMarkers()
    if (is.null(df) || nrow(df) == 0) return()
    r    <- pmax(6, pmin(36, sqrt(df$n) * 3.2))
    col  <- if (input$juv_mode == "last") "#2c7bb6" else "#e6550d"
    fill <- if (input$juv_mode == "last") "#74c6f0" else "#fdae6b"
    prx |> addCircleMarkers(
      data = df, lng = ~lon, lat = ~lat, radius = r,
      color = col, weight = 1, fillColor = fill, fillOpacity = 0.75,
      layerId = ~site_code,
      label   = ~sprintf("%s – %d fish", site_name, n),
      popup   = ~sprintf("<b>%s</b><br/>%d fish", site_name, n))
  })

  juv_click <- reactiveVal(NULL)
  observeEvent(input$juv_map_marker_click, juv_click(input$juv_map_marker_click$id))
  observe({ input$juv_mode; input$juv_stages; input$juv_year; juv_click(NULL) })

  output$juv_detail_header <- renderText({
    sc <- juv_click()
    if (is.null(sc)) "Click a bubble on the map to see fish details."
    else             sprintf("Fish at: %s", sc)
  })

  juv_detail <- reactive({
    sc <- juv_click(); if (is.null(sc)) return(NULL)
    if (input$juv_mode == "last") {
      juv_last_det_filt() |> filter(detection_site_code == sc) |>
        transmute(tag_code, life_stage,
                  release_site   = release_site_label,
                  release_date   = fmt_date(release_date),
                  release_year,
                  detection_site = detection_site_label,
                  last_detection = fmt_date(last_detection))
    } else {
      juv_rel_filt() |> filter(release_site_code == sc) |>
        transmute(tag_code, life_stage,
                  release_site = release_site_name,
                  release_date = fmt_date(release_date),
                  release_year,
                  length_mm)
    }
  })

  output$juv_detail_table <- renderDT({
    df <- juv_detail()
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(), rownames = FALSE, selection = "single"))
    datatable(df, selection = "single", rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE,
                             order = list(list(0, "asc"))))
  })

  observeEvent(input$juv_detail_table_rows_selected, {
    sel <- input$juv_detail_table_rows_selected
    df  <- juv_detail()
    if (is.null(sel) || is.null(df) || sel > nrow(df)) return()
    tag <- df$tag_code[sel]
    hist <- raw$juv_det |> filter(tag_code == tag) |>
      arrange(first_detection) |>
      transmute(
        `Detection Site`  = detection_site_label,
        `First Detection` = fmt_date(first_detection),
        `Last Detection`  = fmt_date(last_detection),
        Count             = count)
    showModal(modalDialog(
      title = paste("Detection history:", tag),
      DTOutput("juv_history_tbl"),
      easyClose = TRUE, footer = modalButton("Close"), size = "l"))
    output$juv_history_tbl <- renderDT(
      datatable(hist, rownames = FALSE,
                options = list(pageLength = 50, dom = "t", scrollX = TRUE)))
  })

  # ============================================================
  # Wells Dam
  # ============================================================

  wells_filt <- reactive({
    df <- raw$wells; if (is.null(df)) return(NULL)
    df |> filter(year >= input$wells_year_range[1],
                 year <= input$wells_year_range[2])
  })

  output$wells_annual <- renderPlotly({
    df <- raw$wells; if (is.null(df)) return(plotly_empty())
    annual_all <- df |>
      group_by(year) |> summarise(total = sum(count, na.rm = TRUE), .groups = "drop") |>
      arrange(year) |>
      mutate(avg10 = sapply(year, function(y) {
        p <- total[year >= y - 10 & year < y]
        if (length(p) == 0) NA_real_ else mean(p, na.rm = TRUE) }))
    annual <- annual_all |>
      filter(year >= input$wells_year_range[1], year <= input$wells_year_range[2])
    plot_ly(annual) |>
      add_bars(x = ~year, y = ~total, name = "Annual total",
               marker = list(color = "#2c7bb6")) |>
      add_trace(x = ~year, y = ~avg10, type = "scatter", mode = "lines+markers",
                name = "Prior 10-yr avg",
                line   = list(color = "#d7191c", width = 2),
                marker = list(color = "#d7191c", size = 7, symbol = "diamond")) |>
      layout(yaxis  = list(title = "Pacific Lamprey (total / year)"),
             xaxis  = list(title = "", dtick = 1),
             legend = list(orientation = "h", x = 0, y = 1.1),
             margin = list(l = 60, r = 20, t = 30, b = 40))
  })

  output$wells_seasonal <- renderPlotly({
    df <- wells_filt(); if (is.null(df) || nrow(df) == 0) return(plotly_empty())
    df <- df |> mutate(season_x = as.Date(paste0("2000-", md))) |> arrange(year, season_x)
    plot_ly(df, x = ~season_x, y = ~count, color = ~as.factor(year),
            type = "scatter", mode = "lines") |>
      layout(yaxis  = list(title = "Pacific Lamprey / day"),
             xaxis  = list(title = "", tickformat = "%b", dtick = "M1"),
             legend = list(title = list(text = "Year")),
             margin = list(l = 60, r = 20, t = 20, b = 40))
  })

  output$wells_table_header <- renderText(
    sprintf("Daily counts – %s", input$wells_table_year))

  output$wells_table <- renderDT({
    yr <- suppressWarnings(as.integer(input$wells_table_year))
    df <- raw$wells
    if (is.null(df) || is.na(yr)) return(datatable(data.frame(), rownames = FALSE))
    prior <- df |> filter(year < yr, year >= yr - 10) |>
      group_by(md) |> summarise(avg10 = mean(count, na.rm = TRUE), .groups = "drop")
    # Cumulative sums are computed over the full season (including
    # zero-count days) before filtering to count >= 1 for display,
    # so a day with no fish this year but a nonzero historical
    # average still contributes to the cumulative 10-yr avg.
    full <- df |> filter(year == yr) |>
      left_join(prior, by = "md") |> arrange(date) |>
      mutate(avg10 = ifelse(is.na(avg10), 0, avg10),
             cum_count = cumsum(count),
             cum_avg10 = cumsum(avg10))
    tbl <- full |> filter(count >= 1) |>
      transmute(Date = fmt_date(date), Count = count,
                `Cumulative Count` = cum_count,
                `10-yr avg` = round(avg10, 1),
                `Cumulative 10-yr avg` = round(cum_avg10, 1))
    if (nrow(tbl) == 0)
      return(datatable(data.frame(message = "No passage recorded this year."),
                       rownames = FALSE))
    datatable(tbl, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE,
                             order = list(list(0, "asc"))))
  })
}

shinyApp(ui, server)
