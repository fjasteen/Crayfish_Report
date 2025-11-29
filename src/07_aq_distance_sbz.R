# ====================================================
# Scriptnaam: 06_sbz_analyse_afstanden.R
# Beschrijving: 
# - DEBUG-VERSIE: Voert de analyse enkel uit voor Natura 2000 (HAB1).
# - De code is lineair gemaakt om stap-voor-stap te kunnen debuggen.
# ====================================================

# --- 0. Instellingen & Data laden ---
source("./src/config.R")
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)

# Laadt alle beschermingslagen en de gefilterde kreeftendata (CF_presence)
# Hierdoor zijn de variabelen 'natura_2000_aq' en 'CF_presence' beschikbaar.
source("./src/05_load_aq_sbz.R")

message("Start ruimtelijke analyse (Wolken & Afstanden) voor Natura 2000...")

# --- Instellen van de Laag ---
CRS_PROJECTED <- 31370
unieke_soorten <- unique(CF_presence$species)
beschermde_laag <- natura_2000_aq
label_kolom <- "HAB1"
gebied_naam <- 
laag_naam <- "Natura 2000"

# Zorg dat de label-kolom beschikbaar is als generieke 'gebied_label'
beschermde_laag <- beschermde_laag %>%
  mutate(gebied_label = as.character(.data[[label_kolom]]))

# HERNOEMD: Duidelijkere namen voor de verzamellijsten
alle_wolken_per_soort_lijst <- list()
aanwezig_in_sbz_lijst <- list()

message("Stap 1: Wolken Vormen en Intersecties Bepalen...")

# --- 1. Wolken vormen & ntersectie voor aanwezigheid---
# De wolk is een populatiecluster.
for (soort in unieke_soorten) {
  species_data <- CF_presence %>% filter(species == soort)
  
  # Buffer 100m rond punten om 'wolken' te maken
  occ_buf_100 <- species_data %>% st_buffer(dist = 100)
  
  if (nrow(occ_buf_100) > 0) {
    # Samenvoegen tot één wolk van occurrences
    wolk_data_union <- occ_buf_100 %>% st_union() %>% st_cast('POLYGON') %>% st_sf()
    
    # Datum bepalen (min date per wolk)
    intersects <- st_intersects(wolk_data_union, species_data)
    wolk_data_union$first_date_cloud <- sapply(intersects, function(idx) {
      if(length(idx) > 0) min(species_data$date[idx], na.rm = TRUE) else NA
    })
    
    wolk_data_union$species <- soort
    alle_wolken_per_soort_lijst[[soort]] <- wolk_data_union # Lijst van alle gevormde wolken
    
    # Intersectie met beschermde laag om wol
    wolk_data_intersect <- wolk_data_union %>% st_intersection(beschermde_laag)
    
    if (nrow(wolk_data_intersect) > 0) {
      aanwezig_in_sbz_lijst[[soort]] <- wolk_data_intersect %>% # Lijst van overlappende wolken
        mutate(species = soort) %>%
        select(species, habitat_id, gebied_label, first_date_cloud)
    }
  } 
}

# Lijsten samenvoegen
all_occ_union <- do.call(rbind, alle_wolken_per_soort_lijst) # Alle wolken van alle soorten
if(!is.null(all_occ_union)) all_occ_union <- all_occ_union %>% mutate(wolk_id = row_number())

intersect_sf <- do.call(rbind, aanwezig_in_sbz_lijst)
# Resultaat Aanwezig: intersect_sf (Bevat de geometrie van de intersectie)

# --- 2. Afstanden berekenen (Buiten het gebied) ---
message("Stap 2: Afstanden Bepalen voor Niet-Overlappende Wolken...")

if(is.null(all_occ_union)) stop("Geen populatie-wolken gevormd, analyse gestopt.")

# Bepaal welke wolken NIET overlappen
layer_union <- st_union(beschermde_laag)
all_occ_union$intersects_layer <- lengths(st_intersects(all_occ_union, layer_union)) > 0

# Dit zijn de wolken die gemeten moeten worden voor nabijheid
wolken_buiten_sbz <- all_occ_union %>% 
  filter(!intersects_layer) %>%
  select(wolk_id, species)
# wolken_buiten_sbz bevat nu de Wolk ID en soort van de wolken die buiten het gebied liggen

if (nrow(wolken_buiten_sbz) > 0) {
  # Bereken afstand van buiten-wolken tot de beschermde elementen
  distances <- st_distance(wolken_buiten_sbz, beschermde_laag)
  
  # Afstandsmatrix naar dataframe (long format)
  dist_df <- as.data.frame(distances) %>%
    mutate(wolk_id = wolken_buiten_sbz$wolk_id) %>%
    pivot_longer(cols = starts_with("V"), names_to = "col_idx", values_to = "dist_m") %>%
    mutate(habitat_idx = as.integer(gsub("V", "", col_idx)))
  
  # Metadata koppelen
  layer_meta <- beschermde_laag %>% st_drop_geometry() %>% mutate(habitat_idx = row_number())
  
  final_dist <- dist_df %>%
    left_join(wolken_buiten_sbz %>% st_drop_geometry(), by = "wolk_id") %>%
    left_join(layer_meta, by = "habitat_idx") %>%
    mutate(dist_m = as.numeric(dist_m)) # Units strippen (meters)
  
  # Filter: <1000m en niet limosus (invasive)
  nearby_pops <- final_dist %>%
    filter(dist_m < 1000) %>%
    filter(species != "faxonius limosus")
  
  # Anti-join: Verwijder 'Nearby' als de soort al IN dat specifieke habitat zit
  if (!is.null(intersect_sf) && nrow(intersect_sf) > 0) {
    already <- intersect_sf %>% st_drop_geometry() %>% select(species, habitat_id) %>% distinct()
    nearby_pops <- nearby_pops %>% anti_join(already, by = c("species", "habitat_id"))
  }
  
  # Geometrie terugkoppelen
  nearby_sf <- nearby_pops %>%
    left_join(all_occ_union %>% select(wolk_id, geometry), by = "wolk_id") %>%
    st_as_sf()
  # Resultaat Nabij: nearby_sf (Bevat de geometrie van de originele wolk)
  
} else {
  nearby_sf <- NULL
  message("Geen wolken buiten het Natura 2000 gebied gevonden om de afstand van te meten.")
}


# --- 3. Leaflet Interactieve Kaart (Natura 2000) ---
message("Stap 3: Leaflet Kaart Genereren...")

# Palette voor alle mogelijke soorten
all_species <- unique(CF_presence$species)
pal <- colorFactor(palette = species_colors, domain = all_species)

# Basis kaart
map <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")

# 1. Achtergrondlaag (Natura 2000 gebied zelf)
map <- map %>%
  addPolygons(data = natura_2000_aq %>% st_transform(4326),
              color = "darkgreen", weight = 1, fillOpacity = 0.2,
              label = ~HAB1, group = laag_naam)

# 2. Nabije populaties (Rood omrand)
if (!is.null(nearby_sf) && nrow(nearby_sf) > 0) {
  for (sp in unique(nearby_sf$species)) {
    dat <- nearby_sf %>% filter(species == sp) %>% st_transform(4326)
    
    popup_content <- paste0(
      "<strong>Soort:</strong> ", sp, "<br>",
      "<strong>Nabij:</strong> ", dat$gebied_label, "<br>",
      "<strong>Afstand:</strong> ", round(dat$dist_m), " m"
    )
    
    map <- map %>%
      addPolygons(data = dat, 
                  color = "red", weight = 2, fillOpacity = 0, 
                  popup = popup_content,
                  group = paste(laag_naam, "Nabij:", sp))
  }
}

# 3. Aanwezige populaties (Blauw omrand + vulling)
if (!is.null(intersect_sf) && nrow(intersect_sf) > 0) {
  for (sp in unique(intersect_sf$species)) {
    dat <- intersect_sf %>% filter(species == sp) %>% st_transform(4326)
    
    popup_content <- paste0(
      "<strong>Soort:</strong> ", sp, "<br>",
      "<strong>In gebied:</strong> ", dat$gebied_label, "<br>",
      "<strong>Datum:</strong> ", dat$first_date_cloud
    )
    
    map <- map %>%
      addPolygons(data = dat, 
                  color = "blue", weight = 2, fillOpacity = 0.4,
                  fillColor = ~pal(species),
                  popup = popup_content,
                  group = paste(laag_naam, "Aanwezig:", sp))
  }
}

# 4. Alle waarnemingspunten (als referentie)
map <- map %>%
  addCircleMarkers(data = CF_presence %>% st_transform(4326),
                   radius = 3, stroke = FALSE, fillOpacity = 0.8,
                   fillColor = ~pal(species),
                   popup = ~paste(species, date),
                   group = "Alle Punten")

# 5. Layer Control
# We verzamelen alle groepen die we nu hebben
map_groups <- c(laag_naam, "Alle Punten")
if (!is.null(intersect_sf)) map_groups <- c(map_groups, unique(paste(laag_naam, "Aanwezig:", intersect_sf$species)))
if (!is.null(nearby_sf)) map_groups <- c(map_groups, unique(paste(laag_naam, "Nabij:", nearby_sf$species)))

map <- map %>%
  addLayersControl(
    baseGroups = c("OSM", "CartoDB"),
    overlayGroups = map_groups,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(pal = pal, values = all_species, title = "Soort", position = "bottomright")

# Toon kaart in viewer 
map