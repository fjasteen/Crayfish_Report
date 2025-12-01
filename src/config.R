## =========================================================
## config.R – centrale instellingen voor craywatch data pipe
## =========================================================

## ---------- Libraries  ----------
required_packages <- c(
  "here",       # Relatieve paden
  "sf",         # Ruimtelijke data
  "dplyr",      
  "tidyr",      
  "lubridate",  # Datums
  "readr",      # CSV lezen/schrijven
  "stringr",    
  "rgbif",      # GBIF API
  "leaflet",    # Interactieve kaarten
  "ggplot2",
  "htmltools"
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
dir_bescherming_output <- file.path(dir_data_output, "maps", "bescherming")

dir_gbif_input        <- file.path(dir_data_input, "gbif")
dir_gbif_intermediate <- file.path(dir_data_intermediate, "gbif")
dir_shapefiles        <- file.path(dir_data_input, "shapefiles")

dir_craywatch_output  <- file.path(root_craywatch_app, "R", "data", "output")
dir_craywatch_assets  <- file.path(root_craywatch_app, "assets")


required_dirs <- c(
  dir_data_input, dir_data_intermediate, dir_data_output,dir_bescherming_output,
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

# Shapefiles 
file_vlaanderen_grenzen <- file.path(dir_shapefiles, "grenzenvlaanderen.shp")
file_watervlakken       <- file.path(dir_shapefiles, "watervlakken.shp")
file_vha_catc           <- file.path(dir_shapefiles, "vhaCattraj.shp")
file_bekken             <- file.path(dir_shapefiles, "Wsbekken.shp")

# Shapefiles gebruikt in ruimtelijke analyse
file_n2000_habitats <- file.path(dir_shapefiles, "BwkHab.shp") # natura2000
file_3260_habitats  <- file.path(dir_shapefiles, "Hab3260.shp") #Ranunculoides hab 3260

file_hoofdrivieren  <- file.path(dir_shapefiles, "hoofdrivieren.shp") # rivieren
file_kanalen        <- file.path(dir_shapefiles, "kanalen.shp") # kanalen
file_gemeenten      <- file.path(dir_shapefiles, "gemeenten.shp") # gemeentegrenzen

## ---------- 4. Bestanden: Intermediate (RDS) ----------
file_inter_craywatch_clean <- file.path(dir_data_intermediate, "craywatch_clean.rds")
file_inter_gbif_processed  <- file.path(dir_data_intermediate, "gbif_processed.rds")

# GBIF raw output 
file_gbif_occurrences      <- file.path(dir_gbif_input, "gbif_occ_CF.csv")

## ---------- 5. Bestanden: Output ----------
file_analyse_dataset_rapport   <- file.path(dir_data_output, "analyse_dataset.csv")
file_analyse_dataset_craywatch <- file.path(dir_craywatch_output, "analyse_dataset.csv")

# Output paden voor kaarten (worden nu in /bescherming geplaatst)
file_map_hbtrl <- file.path(dir_bescherming_output, "map_hbtrl.png")
file_map_sbp <- file.path(dir_bescherming_output, "map_sbp.png")

## ---------- 6. GBIF download instellingen ----------
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
crs_lambert <- 31370 # Belgische Lambert 72

# Maximale afstand (m) tot dichtstbijzijnde waterloop/watervlak om VHAG/CATC/WVLC toe te kennen
max_link_distance_m <- 50 #(Zou 100m moeten zijn omwille van coordinate uncertainty)

# URL voor WFS/API Deelbekken (Watersystemen)
# OGC API Features URL (Geopunt) - Collectie: WsDeelbek
url_wfs_deelbekken <- "https://geo.api.vlaanderen.be/Watersystemen/ogc/features/collections/WsDeelbek/items?f=json&limit=5000"
url_hbrl <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/ps/wfs?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=ps:ps_hbtrl"
url_sbp_pgs <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=lu:lu_sbp_pgs"
url_sbp_pls <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=lu:lu_sbp_pls"

# Filter voor aquatische habitats en soorten
aqua_habcodes <- c('3270', '3260', '3130', '3110', '3140', '3150', '3160', '7230')
aquatische_sbp <- c('Grote modderkruiper', 'Poelkikker', 'Kamsalamander', 'Vroedmeesterpad', 'Heikikker', 'Rugstreeppad', 'Boomkikker', 'Otter', 'Knoflookpad')


# Uniforme kleurenschaal over rapport
species_colors <- c(
  "faxonius limosus"          = "#FFD700",
  "procambarus clarkii"       = "#FF0000", 
  "procambarus virginalis"    = "#FF00FF",
  "faxonius virilis"          = "#FF8C00", 
  "procambarus acutus"        = "#000000", 
  "pontastacus leptodactylus" = "#00FFFF", 
  "pacifastacus leniusculus"  = "#8A2BE2",
  "Afwezigheid"               = "#BEBEBE"
)

species_labels_dutch <- c( 
  "faxonius limosus"          = "Gevlekte Amerikaanse rivierkreeft",
  "procambarus clarkii"       = "Rode Amerikaanse rivierkreeft", 
  "procambarus virginalis"    = "Marmerkreeft", 
  "faxonius virilis"          = "Geknobbelde Amerikaanse rivierkreeft", 
  "procambarus acutus"        = "Gestreepte Amerikaanse rivierkreeft",
  "pontastacus leptodactylus" = "Turkse rivierkreeft",
  "pacifastacus leniusculus"  = "Californische rivierkreeft",
  "absence"                   = "Afwezigheid"
)

color_scale_dutch <- scale_color_manual(values = species_colors, labels = species_labels_dutch)

# baseplot functie

get_baseplot <- function() {
  # Zorg dat libraries geladen zijn (sf, ggplot2)
  
  # Shapefiles inlezen (paden staan al in je config)
  # Gebruik de variabelen die je al hebt gedefinieerd
  vlaanderen    <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% st_transform(31370)
  hoofdrivieren <- st_read(file_hoofdrivieren, quiet = TRUE) %>% st_transform(31370) %>% st_intersection(vlaanderen)
  kanalen       <- st_read(file_kanalen, quiet = TRUE) %>% st_transform(31370) %>% st_intersection(vlaanderen)
  
  # De plot constructie
  ggplot() +
    geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
    geom_sf(data = hoofdrivieren, size=0.3, colour="#6BA1D3") +
    geom_sf(data = kanalen, size=0.3, colour="#6BA1D3") +
    theme_void() +
    theme(legend.title = element_blank(), 
          legend.text = element_text(size=8, face="italic"), 
          legend.key.size = unit(0.2, "cm"),
          legend.position = "bottom",
          plot.title = element_text(face = "italic"))
}