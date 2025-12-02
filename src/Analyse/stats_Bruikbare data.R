library(dplyr)
library(lubridate)
library(tidyr)
library(sf)

# Laad volledige craywatch data in
craywatch_data <- read.csv("~/GitHub/Craywatch-Rapport/R/data/input/craywatch_data.csv")
map_data <- read.csv("~/GitHub/craywatch/assets/localities.csv")

# Creeer een unieke sessie ID per locatie (voor locaties die meer dan 1 keer bemonsterd zijn)
craywatch_data$date <- dmy(craywatch_data$date) # Converteer naar datum
craywatch_data <- craywatch_data %>%
  arrange(locID, date) %>%
  group_by(locID) %>%
  mutate(
    date_diff = c(0, diff(date)),
    session_id = cumsum(date_diff > 7)
  ) %>%
  ungroup()

# som van vallen en individuen per dag, locatie, soort en sessie
daily_data <- craywatch_data %>%
  group_by(locID, session_id, date, soort) %>%
  summarize(
    individuals_daily = sum(number.of.individuals, na.rm = TRUE),
    traps_daily = sum(number.of.traps, na.rm = TRUE),
    vrijwillID = first(vrijwillID),
  )

# groepeer de data en filter de bruikbare data er uit
daily_data <- daily_data %>%
  mutate(soort = gsub("\\s", " ", soort))


species_per_session <- daily_data %>%
  group_by(locID, session_id, soort) %>%  # Groepeer per locatie, sessie en soort
  summarize(
    individuals_caught = sum(individuals_daily, na.rm = TRUE),
    .groups = 'drop'
  )

processed_craywatch_data <- daily_data %>%
  group_by(locID, session_id) %>%
  summarize(
    days_sampled = n_distinct(date),
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    consecutive = {
      date_diff <- diff(sort(unique(date)))
      all(date_diff == 1)
    },
    traps_used = sum(traps_daily, na.rm = TRUE),
    vrijwillID = first(vrijwillID),
    year = year(end_date),
    .groups = 'drop')

# voeg de 2 samen
grouped_craywatch_data <- left_join(species_per_session, processed_craywatch_data, by = c("locID", "session_id")) %>%
  mutate(
    # Bepaal 'by_protocol' en CPUE
    by_protocol = (soort == "crayfish indet" & traps_used >= 12) | (soort != "crayfish indet"),
    CPUE = ifelse(traps_used > 0, individuals_caught / traps_used, 0),
  )
    
# Selecteer de kolommen 'locID', 'Latitude', and 'Longitude' van localities
localities_selected <- map_data %>%
  dplyr::select(locID, Latitude, Longitude)

# voeg data samen met localities om Latitude en Longitude toe te voegen obv locID
grouped_craywatch_data <- left_join(grouped_craywatch_data, localities_selected, by = "locID")

# Ontbrekende locIDs en coördinaten er uit halen
grouped_craywatch_data <- grouped_craywatch_data %>%
  mutate(
    longitude = as.numeric(Longitude),
    latitude = as.numeric(Latitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  select(-Latitude, -Longitude)

# Filter de 'crayfish indet' rijen eruit als er ook andere soorten zijn gevangen
grouped_craywatch_data <- grouped_craywatch_data %>%
  # Stap 1: Groepeer per sessie om te bepalen of er meerdere soorten zijn
  group_by(locID, session_id) %>%
  # Stap 2: Voeg de filterlogica toe
  filter(
    n_distinct(soort) == 1 | (n_distinct(soort) > 1 & soort != "crayfish indet")
  ) %>%
  ungroup() %>%
  select(-session_id)


n_distinct(grouped_craywatch_data$locID)

locations_per_jaar <- grouped_craywatch_data %>%
  group_by(year) %>%
  summarize(
    aantal_unieke = n_distinct(locID),
    .groups = 'drop'
  )
print(locations_per_jaar)

# Hoeveel bruikbare en niet-bruikbare data
niet_bruikbaar <- grouped_craywatch_data %>%
  filter(by_protocol == FALSE)
n_distinct(niet_bruikbaar$locID)

bruikbaar <- grouped_craywatch_data %>%
  filter(by_protocol == TRUE)
n_distinct(bruikbaar$locID)

# door craywatch team
sum(bruikbaar$vrijwillID == "odcxnmmx")

# Per jaar
aantal_2024_nb <- niet_bruikbaar %>%
  filter(year == 2024) %>%
  summarize(aantal_unieke = n_distinct(locID))

aantal_2025_nb <- niet_bruikbaar %>%
  filter(year == 2025) %>%
  summarize(aantal_unieke = n_distinct(locID))

locIDs_2024_b <- bruikbaar %>%
  filter(year == 2024) %>%
  pull(locID) %>%
  unique()
aantal_2024_b <- length(locIDs_2024_b)
print(paste("Aantal unieke bruikbare locIDs in 2024:", aantal_2024_b))

locIDs_2025_b <- bruikbaar %>%
  filter(year == 2025) %>%
  pull(locID) %>%
  unique()
aantal_2025_b <- length(locIDs_2025_b)
print(paste("Aantal unieke bruikbare locIDs in 2025:", aantal_2025_b))

locIDs_beide_jaren <- intersect(locIDs_2024_b, locIDs_2025_b)
aantal_locIDs_beide_jaren <- length(locIDs_beide_jaren)
print("LocIDs die zowel in 2024 als 2025 bruikbaar waren:")
print(locIDs_beide_jaren)

# aanwezigheden vs afwezigheden
aanwezigheid <- bruikbaar %>%
  filter(individuals_caught > 0)
n_distinct(aanwezigheid$locID)

afwezigheid <- bruikbaar %>%
  filter(individuals_caught == 0)
n_distinct(afwezigheid$locID)

# Per jaar aan- en afwezigheid
aantal_2024_aanwezig <- aanwezigheid %>%
  filter(year == 2024) %>%
  summarize(aantal_unieke = n_distinct(locID))

aantal_2024_afwezig <- afwezigheid %>%
  filter(year == 2024) %>%
  summarize(aantal_unieke = n_distinct(locID))

aantal_2025_aanwezig <- aanwezigheid %>%
  filter(year == 2025) %>%
  summarize(aantal_unieke = n_distinct(locID))

aantal_2025_afwezig <- afwezigheid %>%
  filter(year == 2025) %>%
  summarize(aantal_unieke = n_distinct(locID))

# Aantal gevangen individuen
vangst_per_jaar_en_soort <- bruikbaar %>%
  group_by(year, soort) %>%
  summarize(
    totaal_gevangen_individuen = sum(individuals_caught, na.rm = TRUE),
    .groups = 'drop'
  )

# Print het resultaat
print(vangst_per_jaar_en_soort)

# Op hoeveel locaties werd elke soort gevangen
unieke_locaties_per_jaar_en_soort <- bruikbaar %>%
  # Groepeer op jaartal en soort
  group_by(year, soort) %>%
  summarize(
    # Tel het aantal unieke locaties
    aantal_unieke_locaties = n_distinct(locID),
    .groups = 'drop'
  )

# Print het resultaat
print(unieke_locaties_per_jaar_en_soort)

# Aantal vrijwilligers per jaar
bruikbare_vrijwilligers_2024 <- bruikbaar %>%
  filter(year == 2024) %>%
  summarize(aantal_unieke = n_distinct(vrijwillID))

bruikbare_vrijwilligers_2025 <- bruikbaar %>%
  filter(year == 2025) %>%
  summarize(aantal_unieke = n_distinct(vrijwillID))

# Aantal vrijwilligers per jaar
vrijwilligers_2024 <- grouped_craywatch_data %>%
  filter(year == 2024) %>%
  summarize(aantal_unieke = n_distinct(vrijwillID))

vrijwilligers_2025 <- grouped_craywatch_data %>%
  filter(year == 2025) %>%
  summarize(aantal_unieke = n_distinct(vrijwillID))

# Waarnemingen per provincie
provincies <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/provincies.shp")
provincies <- st_transform(provincies, crs = st_crs(provincies))
craywatch_sf <- st_as_sf(bruikbaar, coords = c("longitude", "latitude"), crs = 4326)
craywatch_sf <- st_transform(craywatch_sf, crs = st_crs(provincies))

load("~/GitHub/Craywatch-Rapport/R/data/input/fc_data/analyses_dataset_VHAG_WVLC_CATC.Rdata")
watertype_data <- data

watertype_data_craywatch <- watertype_data %>%
  filter(dat.source == "craywatch_data") %>%
  select(longitude, latitude, VHAG, WVLC, CATC) 

watertype_data_craywatch <- watertype_data_craywatch

watertype_data_craywatch <- st_as_sf(watertype_data_craywatch, coords = c("longitude", "latitude"), crs = 4326)

# 1. Find the index of the nearest feature in watertype_data_craywatch for each feature in craywatch_sf
nearest_index <- st_nearest_feature(craywatch_sf, watertype_data_craywatch)

# 2. Use the index to select the corresponding rows and bind the columns to craywatch_sf
# We use st_drop_geometry() to ensure we only bind the data columns and not a new geometry column.
craywatch_sf_with_watertype <- cbind(
  craywatch_sf,
  st_drop_geometry(watertype_data_craywatch[nearest_index, c("VHAG", "WVLC", "CATC")])
)

# Ruimtelijke join: koppel craywatch-punten aan provincies
punten_in_provincies <- st_join(craywatch_sf_with_watertype, provincies, join = st_within)

punt_zonder_provincie <- punten_in_provincies %>%
  filter(is.na(PROVNAAM))

punten_met_provincie <- punten_in_provincies %>%
  filter(!is.na(PROVNAAM))

# Voer de correctie alleen uit als er NA's zijn
if(nrow(punt_zonder_provincie) > 0) {
  nearest_prov_index <- st_nearest_feature(punt_zonder_provincie, provincies)
  punt_zonder_provincie$PROVNAAM <- provincies$PROVNAAM[nearest_prov_index]
}

# Voeg de data weer samen
punten_in_provincies_opgelost <- bind_rows(punten_met_provincie, punt_zonder_provincie)

# Tel unieke locID's per provincie
punten_per_provincie <- punten_in_provincies_opgelost %>%
  group_by(PROVNAAM) %>%
  summarise(aantal_unieke_locaties = n_distinct(locID))

# Bekijk resultaten
print(punten_per_provincie)


punten_per_provincie_water <- punten_in_provincies_opgelost %>%
  filter(!(is.na(WVLC) & is.na(VHAG))) %>%
  mutate(
    type = if_else(!is.na(WVLC), "stilstaand", "stromend"))


# Tel unieke locID's per provincie
telling_per_provincie <- punten_per_provincie_water %>%
  group_by(PROVNAAM, type) %>%
  summarise(totaal_aantal_rijen = n_distinct(locID))

# Bekijk resultaten
print(telling_per_provincie)

# Aantal locaties per categorie waterloop
# Tel unieke locID's gegroepeerd op Provincie en CATC
telling_per_catc <- punten_in_provincies_opgelost %>%
  # Zorg ervoor dat we alleen de observaties meenemen waar CATC niet NA is
  filter(!is.na(CATC)) %>%
  mutate(
    Aanwezigheid = if_else(individuals_caught > 0, 1, 0)
  ) %>%
  
  # Groepeer op zowel de Provincie Naam als de Categorie
  group_by(CATC, Aanwezigheid) %>%
  
  # Tel het aantal unieke locID's per groep
  summarise(
    aantal_unieke_locaties = n_distinct(locID),
    .groups = "drop" # Drop de groepering na het samenvatten
  )

# Bekijk de resultaten
print(telling_per_catc)
