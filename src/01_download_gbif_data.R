# ====================================================
# Scriptnaam: dnwoload_gbif_data
# Auteur: 
# Datum: 09-01-2025 (update 18-09-2025)
# Beschrijving:
# Dit script downloadt nieuwe GBIF-data op basis van instellingen in config.R
# ====================================================

# 0. Instellingen laden
# Zorg dat de working directory correct staat (root van project) of pas het pad aan
source("./src/config.R") 

############### 1. Downloadparameters ###################

# Haal taxon keys op voor de soorten gedefinieerd in config.R
taxonkeys <- gbif_species %>% 
  name_backbone_checklist() %>% 
  filter(matchType == "EXACT") %>% #enkel de exacte matches
  pull(usageKey)

# Start de download request bij GBIF
set <- occ_download(
  pred_in("taxonKey", taxonkeys),
  pred("country", gbif_country),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", gbif_min_year),
  pred("occurrenceStatus", gbif_occurrence_state),
  user = gbif_user,
  pwd = gbif_pwd,
  email = gbif_email,
  curlopts = list(http_version = 2) #ander protocol voor grotere bestanden
)

############### 2. Wacht op verwerking en download ###################

print(paste("Download aangevraagd. ID:", set))

# Loop om te checken of de download klaar is
repeat {
  Sys.sleep(time = 10) # Korte pauze
  meta <- occ_download_meta(set)
  
  if (meta$status == "SUCCEEDED") {
    download_doi <- meta$doi
    print(paste("Download voltooid. DOI:", download_doi))
    
    # Download de zip-file naar de intermediate folder
    occ_download_get(set,
                     path = dir_gbif_download,
                     overwrite = TRUE,
                     curlopts = list(http_version = 2))
    break
  } else if (meta$status %in% c("KILLED", "CANCELLED")) {
    stop("GBIF download is geannuleerd of mislukt.")
  }
  
  print(paste("Status:", meta$status))
}

############### 3. Verwerking en opslag ###################

# Unzip de gedownloade data
zip_path <- file.path(dir_gbif_download, paste0(set, ".zip"))
unzip_dir <- file.path(dir_gbif_download, set)

unzip(zip_path, exdir = unzip_dir)

# Lees de ruwe data (occurrence.txt)
raw_file_path <- file.path(unzip_dir, "occurrence.txt")
cf_occ <- read.delim(raw_file_path, header = TRUE)

# Omzetten naar sf om coördinaten zeker te stellen (optioneel, maar behouden uit origineel script)
# en daarna direct weer plat slaan naar CSV formaat.
occ_gbif <- st_as_sf(cf_occ, 
                     coords = c("decimalLongitude", "decimalLatitude"),
                     crs = crs_wgs84) %>%
  mutate(decimalLongitude = sf::st_coordinates(.)[,1],
         decimalLatitude = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# Opslaan van de verwerkte dataset en de DOI
write_csv(occ_gbif, file_gbif_occurrences)

# Sla de DOI apart op voor referentie (in dezelfde map als de csv)
doi_file <- file.path(dir_gbif_input, "gbif_download_doi.txt")
writeLines(download_doi, con = doi_file)

print("Script voltooid: Data en DOI opgeslagen.")