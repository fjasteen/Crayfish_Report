## =========================================================
## config.R – centrale instellingen voor 04_analyze_validated_data.R
## =========================================================

## ---------- Libraries (optioneel hier) ----------
required_packages <- c(
  "here",
  "ggspatial",
  "sf",
  "dplyr",
  "scales",
  "osmdata",
  "tidyr",
  "lubridate",
  "rgbif",
  "readr",
  "stringr",
  "glue"
)

invisible(lapply(required_packages, library, character.only = TRUE))

## ---------- Project roots ----------
# Root van dit rapport-repo (Craywatch-Rapport)
root_rapport <- here::here()

# Parallelle craywatch-app repo (../craywatch)
root_craywatch_app <- file.path(dirname(root_rapport), "craywatch")

## ---------- Directory-structuur ----------
dir_data_input        <- file.path(root_rapport, "data", "input")
dir_data_intermediate <- file.path(root_rapport, "data", "intermediate")
dir_data_output       <- file.path(root_rapport, "data", "output")

dir_gbif_input        <- file.path(dir_data_input, "gbif")
dir_gbif_intermediate <- file.path(dir_data_intermediate, "gbif")

dir_shapefiles        <- file.path(dir_data_input, "shapefiles")

dir_craywatch_output  <- file.path(root_craywatch_app, "R", "data", "output")
dir_craywatch_assets  <- file.path(root_craywatch_app, "assets")


required_dirs <- c(
  dir_data_input,
  dir_data_intermediate,
  dir_data_output,
  dir_gbif_input,
  dir_gbif_intermediate, 
  dir_craywatch_output
)

invisible(lapply(required_dirs, function(x) {
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE)
    message(paste("Map aangemaakt:", x))
  }
}))


## ---------- Bestanden: input ----------
file_craywatch_validated <- file.path(
  dir_data_input,
  "craywatch_data.csv"
)   # 10.5281/zenodo.17639074

file_localities_map <- file.path(
  dir_craywatch_assets,
  "localities.csv"
)

file_gbif_occurrences <- file.path(
  dir_gbif_input,
  "gbif_occ_CF.csv"
)

# Shapefiles
file_watervlakken <- file.path(
  dir_shapefiles,
  "watervlakken.shp"
)

file_vha_catc <- file.path(
  dir_shapefiles,
  "vhaCattraj.shp"
)

file_bekken <- file.path(
  dir_shapefiles,
  "Wsbekken.shp"
)

## ---------- Bestanden: output ----------
file_analyse_dataset_rapport <- file.path(
  dir_data_output,
  "analyse_dataset.csv"
)

file_analyse_dataset_craywatch <- file.path(
  dir_craywatch_output,
  "analyse_dataset.csv"
)

## ---------- GBIF download instellingen ----------
gbif_species <- c(
  "Procambarus clarkii",
  "Procambarus virginalis",
  "Procambarus acutus",
  "Faxonius limosus",
  "Pacifastacus leniusculus",
  "Faxonius virilis",
  "Faxonius immunis",
  "Faxonius juvenilis",
  "Faxonius rusticus",
  "Pontastacus leptodactylus"
)

gbif_country          <- "BE"
gbif_min_year         <- 2010L
gbif_occurrence_state <- "PRESENT"

# GBIF API credentials via environment variables
gbif_user  <- Sys.getenv("GBIF_USER")   # let op: consistente naamgeving
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

# Directory waarin de zip van occ_download wordt bewaard
dir_gbif_download <- dir_gbif_intermediate

## ---------- GBIF filterparameters ----------
gbif_issues_to_discard <- c(
  "ZERO_COORDINATE",
  "COORDINATE_OUT_OF_RANGE",
  "COORDINATE_INVALID",
  "COUNTRY_COORDINATE_MISMATCH"
)

gbif_id_status_to_discard <- c(
  "unverified",
  "not validated",
  "under validation"
)

gbif_max_coordinate_uncertainty_m <- 100  # meter
gbif_required_level1_name         <- "Vlaanderen"

## ---------- Craywatch protocolparameters ----------
# Sessies: nieuwe sessie als er > 7 dagen tussen zit
cray_session_gap_days <- 7L

# Minimale trapdays voor geldige afwezigheid (crayfish indet)
cray_min_trapdays_absence <- 12L

# Drempel voor CPUE-filter: bij traps_used < 12 én CPUE == 0 → NA
cray_min_traps_for_confident_zero <- 12L

## ---------- Ruimtelijke instellingen ----------
crs_wgs84   <- 4326
# Overige CRS wordt overgenomen van shapefiles (st_crs(file_watervlakken))

# Maximale afstand (m) tot dichtstbijzijnde waterloop/watervlak om VHAG/CATC/WVLC toe te kennen
max_link_distance_m <- 10

## ---------- Groeperings-/aggregatieregels ----------
# GBIF: groeperingseenheid
gbif_group_by_fields <- c("year", "month", "day", "latitude", "longitude", "species", "dat.source")

# Craywatch: velden om dagelijkse data tot sessieniveau te aggregeren
cray_group_fields_daily <- c("locID", "session_nr", "date", "soort")

cray_group_fields_session <- c("locID", "session_nr", "soort")

