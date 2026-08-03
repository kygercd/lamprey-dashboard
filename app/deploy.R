# deploy.R
# Deploy Lamprey Dashboard to shinyapps.io.
# Run from anywhere in the project:  source("app/deploy.R")

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
suppressPackageStartupMessages(library(rsconnect))

root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION") |
                               rprojroot::has_dir(".github"))
app_dir <- file.path(root, "app")

# Files inside app_dir to upload. Keep this minimal -- the app
# pulls data from the GitHub raw URLs at runtime, so we don't
# need to bundle CSVs.
app_files <- c("app.R")

rsconnect::deployApp(
  appDir         = app_dir,
  appFiles       = app_files,
  appName        = "Lamprey-Dashboard",
  appTitle       = "Lamprey Dashboard",
  account        = "douglaspud-nr",
  server         = "shinyapps.io",
  forceUpdate    = TRUE,
  launch.browser = TRUE
)
