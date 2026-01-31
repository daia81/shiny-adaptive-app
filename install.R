install.packages(
  c(
    "shiny",
    "shinyjs",
    "readxl",
    "dplyr",
    "stringr",
    "tidyr",
    "tibble",
    "ggplot2",
    "httr",
    "jsonlite"
  ),
  repos = "https://cloud.r-project.org",
  Ncpus = max(1L, parallel::detectCores())
)
