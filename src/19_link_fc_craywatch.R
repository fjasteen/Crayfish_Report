# ====================================================
# Scriptnaam: 19_Spatial_Context_Refined.R
# Auteur: Stien Mertens
# Refactored door: Frédérique Steen
# Datum laatste wijziging: 05-12-2025
# Beschrijving: 
# - Laadt de gevalideerde onderzoekslocaties
# - Voert complexe hydrologische analyse uit (VHAG2 identificatie via QGIS)
# - Koppelt VHAG- en WVLC-data met Fysicochemische (FC) metingen
# - Past een driejarig lag-model toe en aggregeert tot zomermediaan
# ====================================================

# --- 0. Instellingen en Libraries laden ---
library(qgisprocess)
library(purrr)
library(sf)
library(dplyr)
library(here) # Toegevoegd voor consistentie met gebruik verderop

# --- 1. Data Inladen ---
# Opmerking: Deze objecten moeten in het geheugen geladen zijn via voorgaande stappen.
# 'file_vha_catc': Bronbestand voor de waterlopen
# 'file_analyse_dataset_rapport': Output van stap 04 (gekoppelde locaties)

# Inlezen waterloopnetwerk en transformeren naar Lambert72
waterloop <- read_sf(file_vha_catc) %>%
  st_transform(31370)

# Masterdataset inlezen en omzetten naar SF-object (Lambert72)
data <- read_csv(file_analyse_dataset_rapport) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(31370)

# --- 2. Ruimtelijke Voorbewerking (QGIS Process) ---
# Doel: Creëren van een topologisch correct waterloopnetwerk.
# Vereist: 'qgisprocess' package en een geconfigureerde QGIS-installatie.

# Stap 1: Snap polylines om kleine gaten te dichten (threshold 0.01)
waterloop_merge <- qgis_run_algorithm(
  "grass:v.clean",
  type = "line",
  input = waterloop,
  tool = "snap",
  threshold = 0.01
)
waterloop_merge <- sf::st_as_sf(waterloop_merge)

# Stap 2: Samenvoegen (dissolve) op basis van VHAG-code
waterloop_merge <- qgis_run_algorithm(
  "native:dissolve",
  INPUT = waterloop_merge,
  SEPARATE_DISJOINT = TRUE,
  FIELD = "VHAG"
)

waterloop_merge <- sf::st_as_sf(waterloop_merge) 

# --- 3. Functie: Identificatie Aangrenzende VHAG's (VHAG2) ---
# Helper functie: Bepaalt of een meetpunt nabij een segment-einde ligt (< 2km).
# Indien ja, zoekt het naar aansluitende VHAG's om discontinuïteiten te corrigeren.

dist_end_point <- function(x, data_point = data, polyline = waterloop_merge){
  
  # Controleer of primaire VHAG bekend is
  if(!is.na(data_point$VHAG[x])){
    
    # Subsets maken voor het specifieke punt en bijbehorende waterloop
    data_sub <- data_point%>%
      dplyr::filter(row_number() == x)
    
    waterloop_sub <- polyline%>%
      dplyr::filter(VHAG == data_sub$VHAG)
    
    # Omzetten naar sfc formaat voor geometrische operaties
    data_sub_sfc <- data_sub%>%
      st_as_sfc()
    
    waterloop_sub_sfc <- waterloop_sub%>%
      st_as_sfc()
    
    # Bereken projectie op de lijn en totale lijnlengte
    measure_data <- st_line_project(waterloop_sub_sfc, data_sub_sfc)
    waterloop_length <- st_length(waterloop_sub_sfc)
    
    # Filter irrelevante segmenten (waar punt niet op projecteert)
    waterloop_length <- waterloop_length[measure_data != 0]
    measure_data <- measure_data[measure_data != 0]
    
    # Bereken afstand tot beide uiteinden (start en eind)
    measure_data_two_direct <- c(measure_data, as.numeric(waterloop_length) - measure_data)
    
    # Als punt binnen 2km van een uiteinde ligt: zoek aangrenzende VHAG
    if(any(measure_data_two_direct < 2000)){
      
      # Buffer van 1km rondom het punt
      buffer_point <- qgis_run_algorithm(
        "native:buffer",
        INPUT = data_sub,
        DISTANCE = 1000
      )
      buffer_point <- sf::st_as_sf(buffer_point) 
      
      # Snijd de huidige VHAG met deze buffer
      waterloop_sub <- st_intersection(waterloop_sub, buffer_point)
      
      # Kleine buffer (2m) toevoegen om 'touching' joins met andere lijnen te garanderen
      waterloop_sub <- qgis_run_algorithm(
        "native:buffer",
        INPUT = waterloop_sub,
        DISTANCE = 2
      )
      waterloop_sub <- sf::st_as_sf(waterloop_sub) 
      
      # Identificeer kruisende (aangrenzende) VHAG's uit de volledige dataset
      waterloop_sub_extra <- qgis_run_algorithm(
        "native:joinattributesbylocation",
        INPUT = waterloop_sub,
        PREDICATE = "cross",
        JOIN = waterloop, # Gebruikt de originele ongemerged set
        JOIN_FIELDS = "VHAG"
      )
      
      waterloop_sub_extra <- sf::st_as_sf(waterloop_sub_extra) 
      
      # Filter de geometrie van deze gevonden 'VHAG_2' segmenten
      waterloop_sub_extra <- waterloop%>%
        dplyr::filter(VHAG %in% waterloop_sub_extra$VHAG_2)
      
      # Koppel VHAG's die binnen 1km van het punt liggen
      data_buffer <- data_sub%>%
        st_join(., select(waterloop_sub_extra, VHAG), join = st_is_within_distance,
                dist = 1000, suffix = c("","_2"))
      
      # Resultaat opslaan
      VHAG = list(data_buffer$VHAG_2)
      
    }else{
      VHAG = NA
    }
    
  }else{
    VHAG = NA
  } 
  return(VHAG)
}

# Toepassen van de functie op de dataset (creëert kolom VHAG2)
data <- data %>%
  mutate(VHAG2 = map(1:nrow(.), dist_end_point))

# Opslaan tussenresultaat
save(data, file = here("data", "intermediate", "analysis_dataset_vhag2.Rdata"))

# --- 4. Temporele Replicatie (Time Lag) ---
# Creëer records voor het huidige jaar en de twee voorgaande jaren (t, t-1, t-2)
# voor koppeling met FC-data over een langere periode.

data <- data%>%
  mutate(yearGroup = year)

data <- data%>%
  bind_rows(data%>%
              mutate(yearGroup = yearGroup - 1))%>%
  bind_rows(data%>%
              mutate(yearGroup = yearGroup - 2))

# --- 5. Voorbereiding Fysicochemische (FC) Data ---
# Zorg voor consistente projectie tussen kreeftendata en FC-data
st_crs(data) <- 31370
st_crs(fc_aggregated) <- 31370

# Hernoem geometry om conflicten tijdens join te voorkomen en afstand te kunnen berekenen
fc_prep <- fc_aggregated %>%
  dplyr::rename(geom_fc = geometry) %>%
  as.data.frame() # Tijdelijk naar dataframe, behoudt geometry kolom

# --- 6. Koppeling WVLC (Gesloten Wateren) ---
# Join op basis van WVLC-code en Jaar

data_fc_wvlc <- data %>%
  dplyr::filter(!is.na(WVLC)) %>%
  # Inner join: behoud alleen matches
  inner_join(fc_prep %>% dplyr::filter(!is.na(WVLC)), 
             by = c("WVLC" = "WVLC", "yearGroup" = "year"), 
             suffix = c("", "_fc")) %>%
  # Element-wise afstandsberekening tussen meetpunt en FC-punt
  mutate(dist_calc = st_distance(geometry, geom_fc, by_element = TRUE),
         distance_cray_FC = as.numeric(dist_calc)) %>%
  # Opkuis: verwijder hulpkolommen
  select(-geom_fc, -dist_calc)

# --- 7. Koppeling VHAG (Open Wateren) ---
# Complexere spatial join: zoekt dichtstbijzijnde punt binnen VHAG (of VHAG2)

# Selecteer subset met VHAG
data_vhag_subset <- data %>%
  dplyr::filter(!is.na(VHAG))

# Functie: Vind dichtstbijzijnde FC-punt binnen correcte VHAG(s) en Jaar
find_nearest_vhag <- function(curr_idx, data_sf, fc_sf) {
  
  # Huidige kreeft locatie
  current_feat <- data_sf[curr_idx, ]
  
  # Verzamel VHAG en eventuele VHAG2 (aangrenzende segmenten)
  target_vhags <- c(current_feat$VHAG)
  if("VHAG2" %in% names(current_feat) && !is.null(current_feat$VHAG2[[1]])) {
    target_vhags <- c(target_vhags, unlist(current_feat$VHAG2))
  }
  
  # Filter FC dataset op doelvlakken en jaar
  fc_sub <- fc_sf %>%
    dplyr::filter(VHAG %in% target_vhags,
                  year == current_feat$yearGroup)
  
  # Return NA indien geen match
  if (nrow(fc_sub) == 0) {
    return(list(sample_point = NA_character_, distance = NA_real_))
  }
  
  # Zoek dichtstbijzijnde feature
  nearest_idx <- st_nearest_feature(current_feat, fc_sub)
  dist <- st_distance(current_feat, fc_sub[nearest_idx, ], by_element = TRUE)
  
  return(list(sample_point = fc_sub$sample_point[nearest_idx], distance = as.numeric(dist)))
}

# Pas functie toe per rij (map)
results_vhag <- map(1:nrow(data_vhag_subset), 
                    ~find_nearest_vhag(.x, data_vhag_subset, fc_aggregated))

# Resultaten verwerken in de dataset
data_fc_vhag_linked <- data_vhag_subset %>%
  mutate(
    sample_point = map_chr(results_vhag, "sample_point"),
    distance_cray_FC = map_dbl(results_vhag, "distance")
  ) %>%
  # Filter op matches binnen 1000m en valide resultaten
  dplyr::filter(!is.na(sample_point) & distance_cray_FC < 1000)

# Fysicochemische data bijvoegen op basis van gevonden sample_point
data_fc_vhag <- data_fc_vhag_linked %>%
  left_join(st_drop_geometry(fc_aggregated), 
            by = c("sample_point", "yearGroup" = "year", "VHAG"),
            suffix = c("", "_fc"))

# --- 8. Samenvoegen en Export Ruwe Data ---
# Combineer WVLC en VHAG resultaten
data_fc_cray_combined <- bind_rows(data_fc_wvlc, data_fc_vhag) %>%
  st_drop_geometry()

# Export dataset
write.table(data_fc_cray_combined, 
            file = here("data", "output", "data_fc_cray_linked.txt"), 
            sep = "\t", row.names = FALSE)

