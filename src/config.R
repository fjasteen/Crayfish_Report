## =========================================================
## config.R – centrale instellingen voor craywatch data pipe
## =========================================================

## ---------- Libraries  ----------
required_packages <- c(
  "here",       # Relatieve paden
  "sf",         # Ruimtelijke data
  "dplyr",      # Data manipulatie
  "tidyr",      # Data reshaping
  "lubridate",  # Datums
  "readr",      # CSV lezen/schrijven
  "stringr",    # String manipulatie
  "rgbif",      # GBIF API
  "leaflet"     # Interactieve kaarten
)


invisible(lapply(required_packages, library, character.only = TRUE))

## ---------- Project paden ----------
# Root van dit rapport-repo
root_rapport <- here::here()

# Parallelle craywatch repo (../craywatch)
root_craywatch_app <- file.path(dirname(root_rapport), "craywatch")

## ---------- Mappenstructuur ----------
dir_data_input        <- file.path(root_rapport, "data", "input")
dir_data_intermediate <- file.path(root_rapport, "data", "intermediate")
dir_data_output       <- file.path(root_rapport, "data", "output")

dir_gbif_input        <- file.path(dir_data_input, "gbif")
dir_gbif_intermediate <- file.path(dir_data_intermediate, "gbif")
dir_shapefiles        <- file.path(dir_data_input, "shapefiles")

dir_craywatch_output  <- file.path(root_craywatch_app, "R", "data", "output")
dir_craywatch_assets  <- file.path(root_craywatch_app, "assets")


required_dirs <- c(
  dir_data_input, dir_data_intermediate, dir_data_output,
  dir_gbif_input, dir_gbif_intermediate, dir_shapefiles,
  dir_craywatch_output
)

invisible(lapply(required_dirs, function(x) {
  if (!dir.exists(x)) dir.create(x, recursive = TRUE)
}))


## ---------- Bestanden: input ----------
# Ruwe data
file_craywatch_validated <- file.path(dir_data_input, "craywatch_data.csv") # 10.5281/zenodo.17639074
file_localities_map      <- file.path(dir_craywatch_assets, "localities.csv")

# Shapefiles (Zorg dat deze namen kloppen met je bestanden op schijf)
file_vlaanderen_grenzen <- file.path(dir_shapefiles, "grenzenvlaanderen.shp")
file_watervlakken       <- file.path(dir_shapefiles, "watervlakken.shp")
file_vha_catc           <- file.path(dir_shapefiles, "vhaCattraj.shp")
file_bekken             <- file.path(dir_shapefiles, "Wsbekken.shp")

## ---------- 4. Bestanden: Intermediate (RDS) ----------
# zorgt dat script 01, 02 en 03 met elkaar kunnen praten via bestanden
# RDS is sneller en behoudt datatypes beter dan CSV.

file_inter_craywatch_clean <- file.path(dir_data_intermediate, "craywatch_clean.rds")
file_inter_gbif_processed  <- file.path(dir_data_intermediate, "gbif_processed.rds")

# GBIF raw output 
file_gbif_occurrences      <- file.path(dir_gbif_input, "gbif_occ_CF.csv")

## ---------- Bestanden: output ----------
## ---------- 5. Bestanden: Output ----------
file_analyse_dataset_rapport   <- file.path(dir_data_output, "analyse_dataset.csv")
file_analyse_dataset_craywatch <- file.path(dir_craywatch_output, "analyse_dataset.csv")

## ---------- GBIF download instellingen ----------
gbif_species <- c(
  "Procambarus clarkii",
  "Procambarus virginalis",
  "Procambarus acutus",
  "Faxonius limosus",
  "Pacifastacus leniusculus",
  "Faxonius virilis",
  #"Faxonius immunis",
  #"Faxonius juvenilis",
  #"Faxonius rusticus",
  "Pontastacus leptodactylus"
)

gbif_country          <- "BE"
gbif_min_year         <- 2010L
gbif_occurrence_state <- "PRESENT"

# GBIF API credentials via environment 
gbif_user  <- Sys.getenv("GBIF_USER")   
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
  "unverified", "not validated","under validation"
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
crs_wgs84   <- 4326 #GPS
crs_lambert <- 1370 # Belgische Lambert 72

# Maximale afstand (m) tot dichtstbijzijnde waterloop/watervlak om VHAG/CATC/WVLC toe te kennen
max_link_distance_m <- 50



