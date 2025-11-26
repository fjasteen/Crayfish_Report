# ====================================================
# Scriptnaam: 02_construct_data_frame.R
# Auteur: 
# Datum: 01-09-2025 (update 26-11-2025)
# Beschrijving:
# Haalt de bruikbare data uit de gevalideerde dataset (validatiecriteria)
# Combineert data met GBIF data tot analysedataset
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")

# --- 1. Data inlezen ---
# paden uit config.R 
craywatch_raw <- read_csv(file_craywatch_validated, show_col_types = FALSE) #raw CW data
map_data      <- read_csv(file_localities_map, show_col_types = FALSE) #localities.csv
gbif_raw      <- read_csv(file_gbif_occurrences, show_col_types = FALSE)

# Shapefiles laden (quiet = TRUE zorgt voor minder output in je console)
watervlakken  <- st_read(file_watervlakken, quiet = TRUE)
vha_catc      <- st_read(file_vha_catc, quiet = TRUE)
bekken        <- st_read(file_bekken, quiet = TRUE)

# --- 2. verwerking CW data ---

# Sessie nummer creëren per locID (>1 sammpling event)
# Criterion: all samples  with  >7 days sampling interval = seperate sampling session
# Sessies identificeren
craywatch_data <- craywatch_raw %>%
  mutate(date = dmy(date)) %>%
  arrange(locID, date) %>%
  filter(str_detect(locID, "^[A-Z]_[0-9]{4}_[0-9]+$")) %>% 
  group_by(locID) %>%
  mutate(
    date_diff = c(0, diff(date)),
    session_nr = cumsum(date_diff > cray_session_gap_days)
  ) %>%
  ungroup()

# Aggregeer data 
daily_data <- craywatch_data %>%
  group_by(locID, session_nr, date, soort) %>%
  summarize(
    individuals_daily = sum(number.of.individuals, na.rm = TRUE),
    traps_daily = sum(number.of.traps, na.rm = TRUE),
    vrijwillID = first(vrijwillID),
    .groups = 'drop'
  )
         
# Group daily data
# remove all unvalid absences (<12 trapdays in water)
# keep all rows where minimum one specimen is caught
grouped_craywatch_data <- daily_data %>%
  group_by(locID, session_nr, soort) %>%  # Groepeer per locatie, sessie en soort
  summarize(
    individuals_caught = sum(individuals_daily, na.rm = TRUE),
    days_sampled = n_distinct(date),
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    consecutive = {
      date_diff <- diff(sort(unique(date)))
      all(date_diff == 1) #add consecutive data for later analysis (evaluation of protocol execution)
    },
    traps_used = sum(traps_daily, na.rm = TRUE),
    .groups = 'drop',
    vrijwillID = first(vrijwillID),
  ) %>%
  dplyr::filter((soort == "crayfish indet" & traps_used >= 12) | 
                (soort != "crayfish indet")) %>% # Filter rijen die aan het protocol voldoen
  select(-session_nr) # remove session_nr

# Add coordinates from map_data 
grouped_craywatch_data <- grouped_craywatch_data %>%
  left_join(
    map_data %>% select(locID, Latitude, Longitude),
    by = "locID"
  ) %>%
  mutate(
    Latitude  = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# # Maak GIS-laag
# craywatch_sf <- st_as_sf(grouped_craywatch_data, coords = c("longitude", "latitude"), crs = 4326)
# st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results.shp", append=FALSE)
# st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results.gpkg", layer = "craywatch_observations", append=FALSE)





######## 3. Transformeer naar wide formaat #########
# Soorten die in df moeten staan
required_species <- read_csv(file_gbif_occurrences, show_col_types = FALSE) %>%
  distinct(species)
#

craywatch_processed <- grouped_craywatch_data %>%
  mutate(
    dat.source = "craywatch_data",
    year = year(end_date),
    date = end_date,
    CPUE = ifelse(traps_used > 0, individuals_caught / traps_used, 0),
    species_present = if_else(soort != "crayfish indet", 1, 0),
    `CPUE_pontastacus leptodactylus` = 0,
    `CPUE_pacifastacus leniusculus` = 0,
    `pontastacus leptodactylus` = 0,
    `pacifastacus leniusculus` = 0
  ) %>%
  rename(species = soort) %>%
  separate_rows(species, sep = ", ") %>%
  # Gebruik values_from met de juiste namen
  pivot_wider(
    names_from = species,
    values_from = c(CPUE, species_present),
    values_fill = 0,
    names_prefix = "",
    names_sep = "_"
  ) %>%
  # Pas de kolomnamen aan
  rename_with(~ sub("species_present_", "", .), starts_with("species_present_")) %>%
  select(-ends_with("crayfish indet"), -individuals_caught, -start_date, -end_date)

# Pas het filter toe op de craywatch_processed dataset met `mutate()` en `across()`
craywatch_processed <- craywatch_processed %>%
  mutate(
    across(
      .cols = matches("^(CPUE|faxonius|procambarus|pontastacus|pacifastacus)"),
      .fns = ~ if_else(
        dat.source == "craywatch_data" & traps_used < 12 & . == 0,
        NA_real_,
        .
      )
    )
  )

# Filter de te gerbuiken gbif data er uit
issues_to_discard <- c(
  "ZERO_COORDINATE",
  "COORDINATE_OUT_OF_RANGE", 
  "COORDINATE_INVALID",
  "COUNTRY_COORDINATE_MISMATCH"
)

identificationVerificationStatus_to_discard <- c(
  "unverified",
  "not validated", 
  "under validation"
)

craywatch_ids <- unique(craywatch_data$id)

occ_filtered <- gbif_data %>%
  distinct(occurrenceID, .keep_all = TRUE) %>%
  dplyr::filter(!issue %in% issues_to_discard) %>%
  dplyr::filter(!identificationVerificationStatus %in% identificationVerificationStatus_to_discard) %>%
  dplyr::filter(coordinateUncertaintyInMeters <= 100 | is.na(coordinateUncertaintyInMeters)) %>%
  dplyr::filter(level1Name == "Vlaanderen") %>%
  mutate(
    dat.source = "gbif_data",
    species_present = 1,
    species = tolower(species)
  ) %>%
  rename(
    latitude = decimalLatitude,
    longitude = decimalLongitude
  ) %>%
  # Voeg month en day toe aan de groep
  group_by(year, month, day, latitude, longitude, species, dat.source) %>%
  summarise(species_present = max(species_present), .groups = 'drop') %>%
  pivot_wider(names_from = species, values_from = species_present) %>%
  # Voeg de ontbrekende kolommen toe met NA's
  mutate(
    traps_used = NA_real_,
    days_sampled = NA_real_,
    consecutive = NA,
    `CPUE_faxonius limosus` = NA_real_,
    `CPUE_procambarus virginalis` = NA_real_,
    `CPUE_procambarus acutus` = NA_real_,
    `CPUE_faxonius virilis` = NA_real_,
    `CPUE_procambarus clarkii` = NA_real_,
    `CPUE_pontastacus leptodactylus` = NA_real_,
    `CPUE_pacifastacus leniusculus` = NA_real_
  ) %>%
  # Maak de date-kolom aan uit year, month, en day
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  # Verwijder de originele year, month en day kolommen
  select(-month, -day)

# Voeg de datasets samen
final_dataset <- bind_rows(craywatch_processed, occ_filtered)

#### Maak intersect met WVLC, CATC en VHAG ###########

watervlakken <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/watervlakken.shp")
vha_catc <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/vhaCattraj.shp")
bekken <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/Wsbekken.shp")

# transformeer craywatch data
final_dataset_sf <- st_as_sf(final_dataset,
                             coords = c("longitude", "latitude"),
                             crs = 4326)
craywatch_sf <- st_transform(final_dataset_sf, crs = st_crs(watervlakken))
vha_catc <- st_transform(vha_catc, crs = st_crs(watervlakken))
bekken <- st_transform(bekken, crs = st_crs(watervlakken))

craywatch_sf<- craywatch_sf%>%
  mutate(# bepaal dichtstbijzijnste waterloop
    nearest_river_index=st_nearest_feature(., vha_catc),
    # voeg VHAG toe
    VHAG=vha_catc$VHAG[nearest_river_index],
    # voeg CATC toe
    CATC=vha_catc$CATC[nearest_river_index],
    # bepaal afstand tussen waterloop en CF punt
    distances_vhag=st_distance(., vha_catc[nearest_river_index, ], by_element = TRUE),
    # bepaal dichtstbijzijnste watervlak
    nearest_waterbody_index=st_nearest_feature(., watervlakken),
    # voeg WVLC toe
    WVLC=watervlakken$WVLC[nearest_waterbody_index],
    # bepaal afstand tussen watervlak en CF punt
    distances_wv=st_distance(., watervlakken[nearest_waterbody_index, ], by_element = TRUE),
    #voeg een entry ID toe
    ID=1:nrow(craywatch_sf)
  )

# bepaal of watervlak of waterloop het dichtstebij ligt

craywatch_sf <- craywatch_sf%>%
  mutate(VHAG=ifelse(distances_vhag<=distances_wv,VHAG,NA),
         CATC=ifelse(distances_vhag<=distances_wv,CATC,NA),
         WVLC=ifelse(distances_wv<distances_vhag,WVLC,NA),
         distances=ifelse(distances_vhag<=distances_wv,distances_vhag,distances_wv))%>%
  select(-c(distances_vhag,distances_wv))


# check afstanden
summary(craywatch_sf$distances)

CF_dataset <- craywatch_sf %>%
  mutate(
    # Bepaal of de afstand > 10 meter is
    is_too_far = distances > 10,
    
    # Zet WVLC, CATC en VHAG op NA als de afstand te groot is (TRUE)
    WVLC = ifelse(is_too_far, NA, WVLC),
    VHAG = ifelse(is_too_far, NA, VHAG),
    CATC = ifelse(is_too_far, NA, CATC)
  ) %>%
  select(-is_too_far, -traps_used, -days_sampled, -vrijwillID, -locID)

CF_dataset <- CF_dataset %>%
  st_join(
    bekken %>% select(BEKNR, BEKNAAM), # Selecteer alleen de relevante kolommen uit bekken
    join = st_intersects,             # Gebruik st_intersects voor punt-in-polygoon
    left = TRUE                       # Gebruik een left join om alle punten te behouden, zelfs als ze buiten een bekken vallen
  )

# Zet terug om naar lat/long
dataset_analyse <- st_transform(CF_dataset, crs = 4326)

# Extraheer de X- en Y-coördinaten
coordinaten_matrix <- st_coordinates(dataset_analyse)

# Voeg de coördinaten toe aan je data frame als nieuwe kolommen
dataset_analyse <- dataset_analyse %>%
  mutate(longitude = coordinaten_matrix[,1],
         latitude = coordinaten_matrix[,2]) %>%
  st_drop_geometry()

craywatch <- dataset_analyse %>%
  dplyr::filter(dat.source == "craywatch_data")


write.csv(dataset_analyse, file = "~/GitHub/Craywatch-Rapport/R/data/output/analyse_dataset.csv", quote = TRUE, row.names = FALSE)
write.csv(dataset_analyse, file = "~/GitHub/craywatch/R/data/output/analyse_dataset.csv", row.names = FALSE, quote = TRUE)