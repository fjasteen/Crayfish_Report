# Laad libraries
library(sf)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggspatial)
library(osmdata)
library(ggplot2)
library(tidyr)
library(lubridate)
library(leaflet)
library(htmltools)

# lees shapefiles
natura_2000 <- st_read("~/SWO craywatch/R/input/shapefiles/BwkHab.shp")
sbp_pgs <- st_read("~/SWO craywatch/R/input/shapefiles/lu_sbp_pgs.shp")
sbp_vissen <- st_read("~/SWO craywatch/R/input/shapefiles/lu_sbp_pls.shp")
hbtrl <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/ps_hbtrl_deel.shp")

aquatische_hab_codes <- c('3270', '3260', '3130', '3110', '3140', '3150', '3160', '7230')

# filter de aquatische habitattypes er uit 
natura_2000_aq <- natura_2000 |>
  filter(stringr::str_detect(HAB1, paste(aquatische_hab_codes, collapse = '|')))

# Toon de eerste paar rijen van de gefilterde data
print(head(natura_2000_aq))

aquatische_sbp <- c('Grote modderkruiper', 'Poelkikker', 'Kamsalamander', 'Vroedmeesterpad', 'Heikikker', 'Rugstreeppad', 'Boomkikker', 'Otter', 'Knoflookpad')

# filter de aquatische sbp's er uit 
sbp_pgs_aq <- sbp_pgs |>
  filter(stringr::str_detect(sbp, paste(aquatische_sbp, collapse = '|')))

# Read shapefiles
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
hoofdrivieren <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/hoofdrivieren.shp")
kanalen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/kanalen.shp")
gemeenten <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/gemeenten.shp")

target_crs <- st_crs(vlaanderen)

# lees craywatch data
CF_data <- read.csv("~/GitHub/Craywatch-Rapport/R/data/output/analyse_dataset.csv")
CF_data_sf <- st_as_sf(CF_data, coords = c("longitude", "latitude"), crs = 4326)
CF_data_sf <- st_transform(CF_data_sf, target_crs)

# Zorg dat alle shapefiles dezelfde CRS hebben
vlaanderen <- st_transform(vlaanderen, target_crs)
hoofdrivieren <- st_transform(hoofdrivieren, target_crs)
kanalen <- st_transform(kanalen, target_crs)
gemeenten <- st_transform(gemeenten, target_crs)
sbp_pgs_aq <- st_transform(sbp_pgs_aq, target_crs)
sbp_vissen <- st_transform(sbp_vissen, target_crs)
natura_2000_aq <- st_transform(natura_2000_aq, target_crs)

# Clip de shapefiles tot de grenzen van Vlaanderen
hoofdrivieren_in_vlaanderen <- st_intersection(hoofdrivieren, vlaanderen)
kanalen_in_vlaanderen <- st_intersection(kanalen, vlaanderen)
gemeenten_in_vlaanderen <- st_intersection(gemeenten, vlaanderen)
sbp_pgs_aq <- st_intersection(sbp_pgs_aq, vlaanderen)
sbp_vissen <- st_intersection(sbp_vissen, vlaanderen)

sf_use_s2(FALSE)

# Haal de hbtrl gebieden er uit die aquatische N2000 habitats hebben
hbtrl <- hbtrl %>%
  st_filter(natura_2000_aq, .predicate = st_intersects)

# Make plot
base_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.3, colour="#6BA1D3")+
  geom_sf(data = kanalen_in_vlaanderen, size=0.3, colour="#6BA1D3")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf()


# Define a color palette for species
species_colors <- c("faxonius.limosus" = "#FFD700",
                    "procambarus.clarkii" = "#FF0000", "procambarus.virginalis" = "#FF00FF",
                    "faxonius.virilis" = "#FF8C00", "procambarus.acutus" = "#000000", "Afwezigheid" = "#BEBEBE", "pontastacus.leptodactylus" = "#00FFFF", "pacifastacus.leniusculus" = "#8A2BE2")

# Update the legend labels for species in Dutch 
species_labels_dutch <- c( "faxonius.limosus" = expression("gevlekte Amerikaanse rivierkreeft"),
                           "procambarus.clarkii" = expression("rode Amerikaanse rivierkreeft"), 
                           "procambarus.virginalis" = expression("marmerkreeft"), 
                           "faxonius.virilis" = expression("geknobbelde Amerikaanse rivierkreeft"), 
                           "procambarus.acutus" = expression("gestreepte Amerikaanse rivierkreeft"),
                           "pontastacus.leptodactylus" = expression("Turkse rivierkreeft"),
                           "pacifastacus.leniusculus" = expression("Californische rivierkreeft"),
                           "Afwezigheid" = expression("afwezigheid"))

# Create a color scale with updated labels in Dutch
color_scale_dutch <- scale_color_manual(values = species_colors, labels = species_labels_dutch)

# Haal de aan- en afwezigheden er uit
CF_data_df <- CF_data_sf %>%
  st_drop_geometry() %>% # Verwijder de geometrie-kolom (de lijst)
  as_tibble() # Maak er een reguliere data frame van

# identificeer de soortkolommen
species_columns <- c("faxonius.limosus", "procambarus.clarkii", "procambarus.acutus", "procambarus.virginalis", "faxonius.virilis", "pontastacus.leptodactylus", "pacifastacus.leniusculus")

Cf_df_analysed <- CF_data_df %>%
  mutate(
    across(all_of(species_columns), ~as.numeric(as.character(.)))
  ) %>%
  mutate(
    species_matrix = as.matrix(.[, species_columns]), 
    is_aanwezig = rowSums(species_matrix == 1, na.rm = TRUE) > 0,
    status = case_when(
      is_aanwezig == TRUE ~ "Aanwezigheid",
      TRUE ~ "Afwezigheid"
    )
  ) %>%
  select(-species_matrix, -is_aanwezig) # Verwijder de hulpkolommen

CF_sf_analysed <- CF_data_sf %>%
  mutate(status = Cf_df_analysed$status)

sf_afwezigheid <- CF_sf_analysed %>%
  filter(status == "Afwezigheid") %>%
  # Geef ze de status 'Afwezigheid' voor de kleur
  mutate(species = "Afwezigheid") %>%
  select(geometry, species)

sf_aanwezigheid <- CF_sf_analysed %>%
  filter(status == "Aanwezigheid") %>%
  # Converteer van breed naar lang formaat: elke aanwezigheid krijgt een eigen rij
  pivot_longer(
    cols = all_of(species_columns),
    names_to = "species",
    values_to = "is_present"
  ) %>%
  # Filter alleen de waarnemingen waar de soort aanwezig is (is_present == 1)
  filter(is_present == 1) %>%
  select(geometry, species, date) 

# Plot natura 2000 map
species_plot_dutch <- base_plot +
  geom_sf(data = natura_2000_aq, fill = "darkgreen", color = "darkgreen", size = 0.5, alpha = 1) +
#  geom_sf(data = sf_afwezigheid, aes(color = species), size = 0.5) +  # lightgrey (absence) points
  geom_sf(data = sf_aanwezigheid, aes(color = species), size = 0.4) +   # other species points
  color_scale_dutch + # Apply the color scale based on species
  guides(
    color = guide_legend(
      override.aes = list(size = 1.2), # Grotere bolletjes
      ncol = 3 # FORCEREN VAN TWEE KOLOMMEN
    ))

# Save the plot (Dutch)
ggsave(species_plot_dutch, file = "~/SWO craywatch/R/output/maps/map_natura_2000.png", 
       width = 15, height = 8, units = "cm", dpi = 400)

# Plot habitatrichtlijn map
species_plot_hbtrl <- base_plot +
  geom_sf(data = hbtrl, fill = "darkgreen", color = "darkgreen", size = 0.5, alpha = 1) +
  #  geom_sf(data = sf_afwezigheid, aes(color = species), size = 0.5) +  # lightgrey (absence) points
  geom_sf(data = sf_aanwezigheid, aes(color = species), size = 0.4) +   # other species points
  color_scale_dutch + # Apply the color scale based on species
  guides(
    color = guide_legend(
      override.aes = list(size = 1.2), # Grotere bolletjes
      ncol = 3 # FORCEREN VAN TWEE KOLOMMEN
    ))

# Save the plot (Dutch)
ggsave(species_plot_hbtrl, file = "~/SWO craywatch/R/output/maps/map_hbtrl.png", 
       width = 15, height = 8, units = "cm", dpi = 400)

# Plot sbp map
species_plot_dutch <- base_plot +
  geom_sf(data = sbp_pgs_aq, fill = "darkgreen", color = "darkgreen", alpha = 0.8) +
  geom_sf(data = sbp_vissen, fill = "green", color = "green", alpha = 1) +
#  geom_sf(data = sf_afwezigheid, aes(color = species), size = 0.5) +  # lightgrey (absence) points
  geom_sf(data = sf_aanwezigheid, aes(color = species), size = 0.4) +   # other species points
  color_scale_dutch + # Apply the color scale based on species
  guides(
    color = guide_legend(
      override.aes = list(size = 1.2), # Grotere bolletjes
      ncol = 3 # FORCEREN VAN TWEE KOLOMMEN
    ))

# Save the plot (Dutch)
ggsave(species_plot_dutch, file = "~/SWO craywatch/R/output/maps/map_sbp.png", 
       width = 15, height = 8, units = "cm", dpi = 400)

#------------------ rivierkreeften in NAtura 2000 natuurgebieden berekenen -------------
# Definieer projectie CRS
CRS_PROJECTED <- 31370 # Lambert 72 (EPSG: 31370)

# STAP 1: BEREKENING VAN WOLKEN EN INTERSECTIE (100m buffer)


# 1.1 Voorbereiding van Natura 2000 en unieke soorten
unieke_soorten <- unique(sf_aanwezigheid$species)

# Zorg dat Natura 2000 een unieke ID heeft (enkel de benodigde kolommen behouden)
natura_2000_aq <- natura_2000_aq %>%
  mutate(habitat_id = row_number()) %>%
  select(habitat_id, HAB1, geometry)

# Initialiseer lijsten voor de resultaten
occ_union_list <- list()
occ_intersect_list <- list()

# 1.2 Loop en berekening (100m buffer)
for (soort in unieke_soorten) {
  species_data <- sf_aanwezigheid %>%
    filter(species == soort)
  
  # Buffer en projecteer de data
  occ_buf_100 <- species_data %>%
    st_transform(CRS_PROJECTED) %>%
    st_buffer(dist = 100)
  
  if (nrow(occ_buf_100) > 0) {
    # Combineer overlappende buffers tot 'wolken'
    occ_data_union <- occ_buf_100 %>%
      st_union() %>%
      st_cast('POLYGON') %>%
      st_sf()
    
    species_points_in_union <- species_data %>% st_transform(CRS_PROJECTED) %>% st_intersects(occ_data_union, sparse = FALSE)
    earliest_date_in_cloud <- min(species_data$date[species_points_in_union], na.rm = TRUE)
    
    occ_data_union$species <- soort
    occ_data_union$first_date_cloud <- earliest_date_in_cloud
    occ_union_list[[soort]] <- occ_data_union
    
    # Intersectie met Natura 2000-gebieden (die al habitat_id bevatten)
    occ_data_intersect <- occ_data_union %>%
      st_intersection(natura_2000_aq %>% st_transform(CRS_PROJECTED))
    
    if (nrow(occ_data_intersect) > 0) {
      occ_intersect_list[[soort]] <- occ_data_intersect %>%
        mutate(species = soort) %>%
        select(species, habitat_id, HAB1, first_date_cloud)
    }
  }
}

# datum eerste waarneming bepalen
first_date_in_N2000 <- sf_aanwezigheid %>%
  # Projecteer naar hetzelfde CRS als N2000
  st_transform(4326) %>% 
  # Join de waarnemingen met de N2000-gebieden. 
  st_join(natura_2000_aq %>% st_transform(4326) %>% select(habitat_id, HAB1)) %>%
  st_drop_geometry() %>%
  # Filter waarnemingen die buiten alle N2000-gebieden liggen (NA in habitat_id)
  filter(!is.na(habitat_id)) %>% 
  # Groepeer per combinatie van soort en habitat
  group_by(species, habitat_id) %>%
  # Vind de vroegste datum in die groep
  summarise(
    first_observation_date = min(date, na.rm = TRUE),
    .groups = 'drop'
  )

# 1.3 Resultaten samenvoegen
all_occ_union <- do.call(rbind, occ_union_list) %>%
  mutate(wolk_id = row_number()) # Voeg wolk_id nu toe voor de afstandsberekening

all_occ_intersect <- do.call(rbind, occ_intersect_list)
all_occ_intersect <- all_occ_intersect %>%
  dplyr::left_join(first_date_in_N2000, by = c("species", "habitat_id"))

# Voeg coördinaten van het zwaartepunt toe (opgeschoond)
all_occ_intersect <- all_occ_intersect %>%
  st_transform(4326) %>%
  mutate(
    lon = st_coordinates(st_centroid(.))[, "X"],
    lat = st_coordinates(st_centroid(.))[, "Y"]
  ) %>%
  st_transform(target_crs)

# Diagnostische controle
print("Diagnostische controle 'wolken':")
print(paste("Aantal polygonen:", nrow(all_occ_union)))
print(paste("Totale oppervlakte (m²):", sum(st_area(all_occ_union))))

  
## 2. AFSTAND TOT NATURA 2000 BEREKENEN EN FILTEREN
  
# 2.1 Splitsing van wolken
natura_2000_union <- st_union(natura_2000_aq %>% st_transform(CRS_PROJECTED))

# Bepaal welke wolken overlappen met het beschermde gebied
all_occ_union$intersects_natura_2000 <- lengths(st_intersects(all_occ_union, natura_2000_union)) > 0

# Splits de data
occ_outside_natura <- all_occ_union %>%
  filter(!intersects_natura_2000) %>%
  select(wolk_id, species) # Behoud enkel wolk_id en species

# 2.2 Afstandsberekening tot individuele Natura 2000-gebieden
# Bereken de afstand tot ELK individueel Natura 2000-gebied (projecteer indien nodig)
distances_to_all_natura <- st_distance(
  occ_outside_natura %>% st_transform(CRS_PROJECTED),
  natura_2000_aq %>% st_transform(CRS_PROJECTED)
)

# 2.3 Data opschonen en koppelen
# Haal de niet-geometrie data van Natura 2000 op (habitat_id is al aanwezig)
natura_2000_data_df <- natura_2000_aq %>%
  st_drop_geometry() %>%
  select(habitat_id, HAB1)

# Converteer de afstandsmarix naar tidy format
distance_tidy <- as.data.frame(distances_to_all_natura) %>%
  mutate(wolk_id = occ_outside_natura$wolk_id) %>% # Voeg wolk_id toe
  tidyr::pivot_longer(
    cols = starts_with("V"),
    names_to = "habitat_column",
    values_to = "afstand_in_meter"
  ) %>%
  mutate(habitat_id = as.integer(gsub("V", "", habitat_column))) %>%
  select(-habitat_column)

# Koppel alle attributen
final_distances_df <- distance_tidy %>%
  left_join(occ_outside_natura %>% st_drop_geometry(), by = "wolk_id") %>%
  left_join(natura_2000_data_df, by = "habitat_id") %>%
  select(wolk_id, habitat_id, species, HAB1, afstand_in_meter)

# 2.4 Filteren
dichtsbijzijnde <- final_distances_df %>%
  filter(afstand_in_meter < units::set_units(1000, "m"))

dichtsbijzijnde_zonder_limosus <- dichtsbijzijnde %>%
  filter(species != "faxonius.limosus")

# **Filter de nabije populaties er uit die reeds in het gebied aanwezig zijn**
aanwezig_in_gebied_ID <- all_occ_intersect %>%
  st_drop_geometry() %>%
  select(species, habitat_id) %>%
  distinct()

aanwezigheid_zonder_limosus <- all_occ_intersect %>%
  filter(species != "faxonius.limosus")

dichtsbijzijnde_gefilterd <- dichtsbijzijnde_zonder_limosus %>%
  anti_join(aanwezig_in_gebied_ID, by = c("species", "habitat_id"))

# 2.5 Geometrie koppelen en finale SF maken
wolken_geometrie_sf <- all_occ_union %>%
  select(wolk_id, geometry)

dichtsbijzijnde_sf <- dichtsbijzijnde_gefilterd %>%
  dplyr::left_join(wolken_geometrie_sf, by = "wolk_id") %>%
  st_as_sf()

# Maak kaart

unieke_soorten <- unique(dichtsbijzijnde_sf$species)
soorten_intersect <- unique(aanwezigheid_zonder_limosus$species)
alle_unieke_soorten <- unique(c(unieke_soorten, soorten_intersect))

# Definieer een Leaflet kleurenfunctie
species_pal <- leaflet::colorFactor(palette = species_colors, 
                                    domain = alle_unieke_soorten)
# plot



leaflet() %>%
  addTiles(data = "OSM") %>%
  
  # 1. Natura 2000-gebieden (Laag 1)
  addPolygons(data = natura_2000_aq %>% st_transform(4326), 
              color = "darkgreen", 
              fill = "darkgreen", 
              fillOpacity = 0.8, weight = 1,
              # ⬅️ OPLOSSING: Converteer de labelkolom naar een karaktervector
              label = ~as.character(HAB1), 
              group = "Natura 2000 Gebieden") %>%
  
  # 2. Polygons per Soort (Laag 2a, 2b, etc.)
  {
    map <- .
    for (soort in unieke_soorten) {
      map <- map %>%
        addPolygons(data = dichtsbijzijnde_sf %>% 
                      dplyr::filter(species == soort) %>% 
                      st_transform(4326),
                    weight = 4,
                    fillOpacity = 1,
                    # ⬅️ OPLOSSING: Converteer de labelkolom naar een karaktervector
                    label = ~as.character(wolk_id),
                    fillColor = ~species_pal(species),
                    color = "red",
                    group = paste("Nabije populatie:", soort)) 
    }
    map
  } %>%
  
  {
    map <- .
    for (soort in soorten_intersect) {
      map <- map %>%
        addPolygons(data = aanwezigheid_zonder_limosus %>% 
                      dplyr::filter(species == soort) %>% 
                      st_transform(4326),
                    weight = 4,
                    fillOpacity = 0.9,
                    label = ~paste0("Soort: ", species, 
                                    "<br>Gebied: ", HAB1,
                                    "<br>Eerste Waarneming: ", 
                                    first_observation_date),
                    fillColor = ~species_pal(species),
                    color = "blue", # Gebruik een andere kleur voor de rand dan de nabije populaties
                    group = paste("Aanwezig in N2000:", soort))
    }
    map
  } %>%
  # 3. CircleMarkers (Laag 3)
  addCircleMarkers(data = sf_aanwezigheid %>% st_transform(4326),
                   radius = 5,
                   stroke = FALSE,
                   fillColor = ~species_pal(species),
                   fillOpacity = 0.9,
                   # ⬅️ OPLOSSING: Converteer de labelkolom naar een karaktervector
                   label = ~as.character(species),
                   group = "Alle Waarnemingspunten") %>% 
  
  fitBounds(
    lng1 = 5.466, lat1 = 50.933,
    lng2 = 5.540, lat2 = 50.985
  ) %>%
  
  # 4. Layer Control en Legend blijven ongewijzigd
  addLayersControl(
    overlayGroups = c("Natura 2000 Gebieden", 
                      paste("Aanwezig in N2000:", soorten_intersect),
                      paste("Nabije populatie:", unieke_soorten), 
                      "Alle Waarnemingspunten"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(position = "bottomright", 
            pal = species_pal, 
            values = alle_unieke_soorten, 
            title = "Soort")

# ------------------ Voor SBP's -----------------------------------------------

sbp_pgs_aq <- sbp_pgs_aq %>%
  # Creëer de unieke ID op de SF-laag zelf
  mutate(habitat_id = row_number(),
         sbp_soort = soort)

sbp_vissen <- sbp_vissen %>%
  # Creëer de unieke ID op de SF-laag zelf
  mutate(habitat_id = row_number(),
         sbp_soort = soort) 

# Initialiseer lijsten om de resultaten op te slaan
occ_intersect_pgs <- list()
occ_intersect_vis <- list()
occ_intersect_vis_results <- list()
occ_intersect_pgs_results <- list()

# Maak een buffer rond de lijnvormige element bij vissen
sbp_vissen_buffer <- sbp_vissen %>%
  st_transform(crs = 31370) %>%
  st_buffer(dist = 10)
  
# Loop door elke unieke soort om een intersect te maken voor de vissen
for (soort in unieke_soorten) {
  species_clouds <- all_occ_union %>%
    filter(species == soort) %>%
    st_transform(31370)
  # Voer de intersectie uit met SBP vissen
  occ_intersect_vis <- species_clouds %>%
      st_intersection(sbp_vissen_buffer)
  # Controleer of de intersectie daadwerkelijk een resultaat opleverde
  if (nrow(occ_intersect_vis) > 0) {
      # Voeg de intersectie toe aan de lijst
      occ_intersect_vis$species_kreeft <- soort
      occ_intersect_vis_results[[soort]] <- occ_intersect_vis %>%
        select(species_kreeft, habitat_id, sbp_soort, wolk_id)
    }
  }

# Combineer alle 'wolken' in de lijst tot één sf object
all_intersect_vis <- do.call(rbind, occ_intersect_vis_results)

# Voer een diagnostische controle uit
print("Diagnostische controle 'wolken':")
print(paste("Aantal polygonen:", nrow(all_intersect_vis)))
print(paste("Totale oppervlakte (m²):", sum(st_area(all_intersect_vis))))

# Voor de andere soorten
sbp_pgs_aq <- sbp_pgs_aq %>%
  st_transform(31370)

# Loop door elke unieke soort om een intersect te maken voor de andere soorten
for (soort in unieke_soorten) {
  species_clouds <- all_occ_union %>%
    filter(species == soort) %>%
    st_transform(31370)
  # Voer de intersectie uit met SBP vissen
  occ_intersect_pgs <- species_clouds %>%
    st_intersection(sbp_pgs_aq)
  # Controleer of de intersectie daadwerkelijk een resultaat opleverde
  if (nrow(occ_intersect_pgs) > 0) {
    # Voeg de intersectie toe aan de lijst
    occ_intersect_pgs$species_kreeft <- soort
    occ_intersect_pgs_results[[soort]] <- occ_intersect_pgs %>%
      select(species_kreeft, habitat_id, sbp_soort, wolk_id)
  }
}

# Combineer alle 'wolken' in de lijst tot één sf object
all_intersect_pgs <- do.call(rbind, occ_intersect_pgs_results)
all_intersect_sbp <- bind_rows(all_intersect_vis, all_intersect_pgs)

# Maak één samengevoegde geometrie van ALLE SBP-gebieden (Vissen en PGS)
sbp_vissen_proj <- sbp_vissen_buffer
sbp_pgs_proj <- sbp_pgs_aq %>% st_transform(CRS_PROJECTED)
sbp_all_individual <- bind_rows(sbp_vissen_proj, sbp_pgs_proj)

sbp_all_geometries <- sbp_all_individual %>% st_geometry()

# 2.2 Bepaal Overlap voor het splitsen van de wolken
all_occ_union$intersects_sbp <- lengths(st_intersects(
  all_occ_union %>% st_transform(CRS_PROJECTED),
  sbp_all_individual
)) > 0

# Splits de data: wolken die buiten SBP-gebieden liggen
occ_outside_sbp <- all_occ_union %>%
  filter(!intersects_sbp) %>%
  select(wolk_id, species)

distances_to_all_sbp <- st_distance(
  occ_outside_sbp %>% st_transform(CRS_PROJECTED),
  sbp_all_individual
)

# 2.4 Data opschonen en koppelen
# SBP attributen (zonder geometrie) voor de join
sbp_data_df <- sbp_all_individual %>%
  st_drop_geometry() %>%
  select(habitat_id, sbp_soort)

# Converteer de afstandsmarix naar tidy format en koppel attributen
distance_tidy_sbp <- as.data.frame(distances_to_all_sbp) %>%
  mutate(wolk_id = occ_outside_sbp$wolk_id) %>%
  tidyr::pivot_longer(
    cols = starts_with("V"),
    names_to = "habitat_column",
    values_to = "afstand_in_meter"
  ) %>%
  # Gebruik row_number om te koppelen aan de juiste habitat_id
  mutate(habitat_id_index = as.integer(gsub("V", "", habitat_column))) %>%
  left_join(sbp_data_df %>% mutate(habitat_id_index = row_number()),
            by = "habitat_id_index") %>%
  select(-habitat_column, -habitat_id_index)

# Koppel de soortsinformatie van de wolk en finaliseer de dataframe
final_distances_sbp_df <- distance_tidy_sbp %>%
  left_join(occ_outside_sbp %>% st_drop_geometry(), by = "wolk_id") %>%
  rename(species_sbp = sbp_soort) %>%
  select(wolk_id, habitat_id, species, species_sbp, afstand_in_meter)


# 2.5 Filteren (Afstand en Aanwezigheid)
dichtsbijzijnde_sbp <- final_distances_sbp_df %>%
  filter(afstand_in_meter < units::set_units(1000, "m")) %>%
  filter(species != "faxonius.limosus")

aanwezigheid_sbp <- bind_rows(all_intersect_vis, all_intersect_pgs) %>%
  filter(species_kreeft != "faxonius.limosus")

# Identificeer BINNEN-populaties (gebruik de gecombineerde intersecties)
aanwezig_in_sbp_ID <- bind_rows(all_intersect_vis, all_intersect_pgs) %>%
  st_drop_geometry() %>%
  select(species_kreeft, habitat_id) %>%
  distinct() %>%
  rename(species = species_kreeft)

# Filter de nabije populaties die reeds binnen het gebied aanwezig zijn
dichtsbijzijnde_sbp_gefilterd <- dichtsbijzijnde_sbp %>%
  anti_join(aanwezig_in_sbp_ID, by = c("species", "habitat_id"))

# 2.6 Geometrie koppelen en finale SF maken
wolken_geometrie_sf <- all_occ_union %>%
  select(wolk_id, geometry)

dichtsbijzijnde_sbp_sf <- dichtsbijzijnde_sbp_gefilterd %>%
  # Join het SF-object (met geometrie) op de wolk_id
  dplyr::left_join(wolken_geometrie_sf, by = "wolk_id") %>%
  st_as_sf()

unieke_soorten_sbp <- unique(dichtsbijzijnde_sbp_sf$species)
soorten_in_sbp <- unique(aanwezigheid_sbp$species)
alle_sbp_soorten_legend <- unique(c(unieke_soorten_sbp, soorten_in_sbp))
unieke_sbp_soorten <- unique(sbp_all_individual$sbp_soort)

# Haal de data voor de labels één keer expliciet op en forceer de conversie:
sbp_labels <- sbp_all_individual %>% pull(sbp_soort) %>% as.character()
marker_labels <- sf_aanwezigheid %>% pull(species) %>% as.character()

# Definieer een Leaflet kleurenfunctie
species_pal_sbp <- leaflet::colorFactor(palette = species_colors, 
                                    domain = alle_sbp_soorten_legend)

# zorg dat kreeftenpopulaties die met meerdere SBP's overlappen elke SBP vermelden
# Aggregeer de SBP details per unieke kreeftenwolk
aanwezigheid_geaggregeerd <- aanwezigheid_sbp %>%
  st_drop_geometry() %>% 
  group_by(wolk_id, species_kreeft) %>% 
  summarise(
    sbp_details_label = paste0(
      "SBP ", sbp_soort, " (ID: ", habitat_id, ")",
      collapse = "<br>"),
    .groups = 'drop'
  )

# Selecteer de unieke geometrie en voeg de geaggregeerde details toe
aanwezigheid_uniek_sf <- aanwezigheid_sbp %>%
  # Selecteer slechts één rij per unieke geometrie/wolk
  group_by(wolk_id) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>%
  select(-sbp_soort, -habitat_id) %>% 
  # Join de geaggregeerde details terug op de unieke geometrieën
  left_join(aanwezigheid_geaggregeerd, by = c("wolk_id", "species_kreeft")) %>%
  st_transform(4326)

# Converteer de sbp_details_label kolom naar HTML-objecten
aanwezigheid_uniek_sf <- aanwezigheid_uniek_sf %>%
  mutate(
    # BELANGRIJK: Maak van de string een HTML-object zodat <br> werkt
    sbp_details_html = lapply(sbp_details_label, HTML)
  )

# Ook voor de nabije populaties
nabijheid_geaggregeerd <- dichtsbijzijnde_sbp_sf %>%
  st_drop_geometry() %>% 
  group_by(wolk_id, species) %>% 
  summarise(
    # Combineer alle nabije SBP-namen, ID's en afstand in een tekststring
    nabijheid_details_label = paste0(
      "Nabij SBP: ", species_sbp, 
      " (ID: ", habitat_id, ")",
      " - Afstand: ", round(afstand_in_meter, 0), "m",
      collapse = "<br>"),
    .groups = 'drop'
  )

# 2. Selecteer de unieke geometrie en voeg de geaggregeerde details toe
nabijheid_uniek_sf <- dichtsbijzijnde_sbp_sf %>%
  # Selecteer slechts één rij per unieke geometrie/wolk (behoudt de geometrie)
  group_by(wolk_id) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>%
  # Verwijder de oude SBP-kolommen die nu misleidend zijn
  select(-species_sbp, -habitat_id, -afstand_in_meter) %>% 
  # Join de geaggregeerde details terug op de unieke geometrieën
  left_join(nabijheid_geaggregeerd, by = c("wolk_id", "species")) %>%
  st_transform(4326)

# 3. Converteer de labeltekst naar HTML
nabijheid_uniek_sf <- nabijheid_uniek_sf %>%
  mutate(
    # Maak van de string een HTML-object zodat <br> werkt in de popup
    nabijheid_details_html = lapply(nabijheid_details_label, HTML)
  )

# Zorg ervoor dat de 'species' kolom als factor wordt behandeld voor de pal
nabijheid_uniek_sf$species <- as.factor(nabijheid_uniek_sf$species)

# Plot

leaflet() %>%
  addTiles(data = "OSM") %>%
  
  # 1. SBP-gebieden (Laag 1)
  
  addPolygons(data = sbp_all_individual %>% st_transform(4326), 
              color = "darkgreen", 
              fill = "darkgreen", 
              fillOpacity = 0.8, weight = 1,
              label = sbp_labels, 
              group = "SBP Gebieden") %>%
  
  # 2. Polygons per Soort (Laag 2a, 2b, etc.)
  {
    map <- .
    for (soort in unieke_soorten_sbp) {
      
      # Gebruik de geaggregeerde dataset
      data_subset_unique_nabij <- nabijheid_uniek_sf %>% 
        dplyr::filter(species == soort)
      
      if (nrow(data_subset_unique_nabij) > 0) {
        wolk_labels <- data_subset_unique_nabij %>% pull(wolk_id) %>% as.character()
        
        map <- map %>%
          addPolygons(data = data_subset_unique_nabij, # Gebruik de unieke data
                      weight = 4,
                      fillOpacity = 1,
                      label = wolk_labels, 
                      popup = ~nabijheid_details_html, 
                      fillColor = ~species_pal(species),
                      color = "red",
                      group = paste("Soort Polygons:", soort)) 
      }
    }
    map
  } %>%
  
  {
    map <- .
    for (soort in soorten_in_sbp) {
      # Gebruik de geaggregeerde en unieke data
      data_subset_unique <- aanwezigheid_uniek_sf %>% 
        dplyr::filter(species_kreeft == soort)
      
      if (nrow(data_subset_unique) > 0) {
        map <- map %>%
          addPolygons(data = data_subset_unique,
                      weight = 4,
                      fillOpacity = 0.9,
                      label = ~species_kreeft,
                      popup = ~sbp_details_html,
                      fillColor = ~species_pal(soort),
                      color = "blue",
                      group = paste("Aanwezig in SBP:", soort))
      }
    }
    map
  } %>%
  
  # 3. CircleMarkers (Laag 3)
  addCircleMarkers(data = sf_aanwezigheid %>% st_transform(4326),
                   radius = 5,
                   stroke = FALSE,
                   fillColor = ~species_pal(soort),
                   fillOpacity = 0.9,
                   label = marker_labels,
                   group = "Alle Waarnemingspunten") %>% 
  
  fitBounds(
    lng1 = 5.466, lat1 = 50.933,
    lng2 = 5.540, lat2 = 50.985
  ) %>%
  
  # 4. Layer Control en Legend 
  addLayersControl(
    overlayGroups = c("SBP Gebieden", 
                      paste("Soort Polygons:", unieke_soorten), 
                      paste("Aanwezig in SBP:", soorten_in_sbp), 
                      "Alle Waarnemingspunten"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(position = "bottomright",
            pal = species_pal_sbp, 
            values = alle_sbp_soorten_legend, 
            title = "Soort")

# ----------------- Voor HBTRL ---------------------------------------------------

hbtrl$gebcode <- iconv(hbtrl$gebcode, from = "latin1", to = "UTF-8", sub = "byte")
hbtrl$naam <- iconv(hbtrl$naam, from = "latin1", to = "UTF-8", sub = "byte")
hbtrl$deelgebied <- iconv(hbtrl$deelgebied, from = "latin1", to = "UTF-8", sub = "byte")

# Initialiseer de lijst om de resultaten op te slaan
occ_intersect_hbtrl_results <- list()

# Transformeer
hbtrl_gebieden <- hbtrl %>%
  st_transform(31370)

# Loop door elke unieke soort om een intersectie met HRG te maken
for (soort in unieke_soorten) {
  species_clouds <- all_occ_union %>%
    filter(species == soort) %>%
    st_transform(31370)
  
  # Voer de intersectie uit met hbtrl
  occ_intersect_hbtrl <- species_clouds %>%
    st_intersection(hbtrl_gebieden)
  
  # Controleer of de intersectie daadwerkelijk een resultaat opleverde
  if (nrow(occ_intersect_hbtrl) > 0) {
    # Voeg de soortsinformatie toe aan de intersectie
    occ_intersect_hbtrl$species_kreeft <- soort
    # Voeg de intersectie toe aan de lijst, met de benodigde kolommen
    occ_intersect_hbtrl_results[[soort]] <- occ_intersect_hbtrl %>%
      select(
        wolk_id,          # Unieke ID van de kreeftenwolk
        species_kreeft,   # Soort van de kreeftenwolk
        gebcode,          # HRG attribuut
        naam,             # HRG attribuut
        id,
        deelgebied 
      )
  }
}

# Combineer alle 'wolken' in de lijst tot één sf object
all_intersect_hbtrl <- do.call(rbind, occ_intersect_hbtrl_results)

aanwezigheid_hbtrl <- all_intersect_hbtrl %>% 
  filter(species_kreeft != "faxonius.limosus")

# Voer een diagnostische controle uit
print("Diagnostische controle 'Hbtrl wolken':")
print(paste("Aantal overlappende polygonen:", nrow(all_intersect_hbtrl)))

# Bepaal overlap voor het splitsen van de wolken
all_occ_union$intersects_hbtrl <- lengths(st_intersects(
  all_occ_union %>% st_transform(31370), hbtrl_gebieden
)) > 0

occ_outside_hbtrl <- all_occ_union %>% 
  filter(!intersects_hbtrl) %>% 
  select(wolk_id, species)

# Bereken de afstand tot HRG-gebieden
distances_to_all_hbtrl <- st_distance(occ_outside_hbtrl %>% st_transform(31370), hbtrl_gebieden)

# HRG attributen voor de join
hbtrl_data_df <- hbtrl_gebieden %>% 
  st_drop_geometry() %>% 
  select(id, gebcode, naam, deelgebied)

# Converteer de afstandsmarix en koppel HRG attributen
distance_tidy_hbtrl <- as.data.frame(distances_to_all_hbtrl) %>%
  mutate(wolk_id = occ_outside_hbtrl$wolk_id) %>%
  tidyr::pivot_longer(cols = starts_with("V"), names_to = "habitat_column", values_to = "afstand_in_meter") %>%
  mutate(habitat_id_index = as.integer(gsub("V", "", habitat_column))) %>%
  left_join(hbtrl_data_df %>% mutate(habitat_id_index = row_number()), by = "habitat_id_index") %>%
  select(-habitat_column, -habitat_id_index)

# Koppel soortsinformatie en filter
final_distances_hbtrl_df <- distance_tidy_hbtrl %>%
  left_join(occ_outside_hbtrl %>% st_drop_geometry(), by = "wolk_id") %>%
  rename(species_hbtrl = gebcode)

# Aanwezige populaties (voor anti_join)
aanwezig_in_hbtrl_ID <- aanwezigheid_hbtrl %>% 
  st_drop_geometry() %>% 
  select(species_kreeft, wolk_id) %>% 
  distinct() %>% 
  rename(species = species_kreeft)

# Nabije populaties (max 1000m, minus reeds aanwezige)
dichtsbijzijnde_hbtrl <- final_distances_hbtrl_df %>%
  filter(afstand_in_meter < units::set_units(1000, "m"), species != "faxonius.limosus") %>%
  anti_join(aanwezig_in_hbtrl_ID, by = c("species", "wolk_id"))

# Geometrie koppelen en SF maken voor Nabije populaties
dichtsbijzijnde_hbtrl_sf <- dichtsbijzijnde_hbtrl %>% 
  dplyr::left_join(all_occ_union %>% select(wolk_id, geometry), by = "wolk_id") %>% 
  st_as_sf()

# 1. AGGREGATIE HRG Aanwezig
hbtrl_geaggregeerd <- aanwezigheid_hbtrl %>%
  st_drop_geometry() %>% 
  group_by(wolk_id, species_kreeft) %>% 
  summarise(
    hbtrl_details_label = paste0("HRG Code: ", gebcode, " (Naam: ", naam, ")", " (ID: ", id, ")", " - Deelgebied: ", deelgebied, collapse = "<br>"),
    .groups = 'drop')
hbtrl_uniek_sf <- aanwezigheid_hbtrl %>%
  group_by(wolk_id) %>% filter(row_number() == 1) %>% ungroup() %>%
  select(-gebcode, -naam, -id) %>% 
  left_join(hbtrl_geaggregeerd, by = c("wolk_id", "species_kreeft")) %>%
  st_transform(4326) %>%
  mutate(hbtrl_details_html = lapply(hbtrl_details_label, HTML))
soorten_in_hbtrl <- unique(hbtrl_uniek_sf$species_kreeft)

# 2. AGGREGATIE HRG Nabij
nabijheid_hbtrl_geaggregeerd <- dichtsbijzijnde_hbtrl_sf %>%
  st_drop_geometry() %>% 
  group_by(wolk_id, species) %>% 
  summarise(
    nabijheid_hbtrl_label = paste0("Nabij HRG: ", naam, " (Code: ", species_hbtrl, ") - Afstand: ", round(afstand_in_meter, 0), "m", collapse = "<br>"),
    .groups = 'drop')
nabijheid_hbtrl_uniek_sf <- dichtsbijzijnde_hbtrl_sf %>%
  group_by(wolk_id) %>% filter(row_number() == 1) %>% ungroup() %>%
  select(-species_hbtrl, -id, -naam, -afstand_in_meter) %>% 
  left_join(nabijheid_hbtrl_geaggregeerd, by = c("wolk_id", "species")) %>%
  st_transform(4326) %>%
  mutate(nabijheid_hbtrl_details_html = lapply(nabijheid_hbtrl_label, HTML))
unieke_soorten_nabij_hbtrl <- unique(nabijheid_hbtrl_uniek_sf$species)

# Projecteer HRG data
hbtrl_gebieden <- hbtrl %>% st_transform(31370)

# Transformeer HBTRL-gebieden naar WGS84 (4326) en maak een label
hbtrl_gebieden_4326 <- hbtrl_gebieden %>% 
  st_transform(4326) %>%
  # Maak een label
  mutate(label_hbtrl = paste0(gebcode, ": ", naam, " (", deelgebied, ")"))

# Haal de labels op voor de vaste layer control
hbtrl_labels <- hbtrl_gebieden_4326 %>% pull(label_hbtrl) %>% as.character()
alle_hrg_soorten_legend <- unique(c(soorten_in_hbtrl, unieke_soorten_nabij_hbtrl))

hrg_bbox <- st_bbox(hbtrl_uniek_sf)

# Plot
leaflet() %>%
  addTiles(data = "OSM") %>%
  
  # 1. SBP-gebieden (Laag 1)
  
  addPolygons(data = hbtrl_gebieden_4326, 
              color = "darkgreen",           
              fill = "darkgreen", 
              fillOpacity = 0.6,          
              weight = 1,
              label = hbtrl_labels,       
              group = "HBTRL Gebieden") %>%
  
  # 2. Polygons per Soort (AANWEZIG in HRG)
  {
    map <- .
    for (soort in soorten_in_hbtrl) {
      data_subset_hrg <- hbtrl_uniek_sf %>% 
        dplyr::filter(species_kreeft == soort)
      
      if (nrow(data_subset_hrg) > 0) {
        map <- map %>%
          addPolygons(data = data_subset_hrg,
                      weight = 6, 
                      fillOpacity = 0.9,
                      label = ~species_kreeft, 
                      popup = ~hbtrl_details_html, 
                      fillColor = ~species_pal(soort), 
                      color = "darkblue", 
                      group = paste("Aanwezig in HRG:", soort))
      }
    }
    map
  } %>%
  
  # 3. Polygons per Soort (NABIJ HRG)
  {
    map <- .
    for (soort in unieke_soorten_nabij_hbtrl) {
      data_subset_nabij_hrg <- nabijheid_hbtrl_uniek_sf %>% 
        dplyr::filter(species == soort)
      
      if (nrow(data_subset_nabij_hrg) > 0) {
        map <- map %>%
          addPolygons(data = data_subset_nabij_hrg,
                      weight = 6, 
                      fillOpacity = 0.5,
                      label = ~wolk_id, 
                      popup = ~nabijheid_hbtrl_details_html, 
                      fillColor = ~species_pal(species), 
                      color = "red",
                      group = paste("Nabij HRG:", soort))
      }
    }
    map
  } %>%
  
  # 3. CircleMarkers (Laag 3)
  addCircleMarkers(data = sf_aanwezigheid %>% st_transform(4326),
                   radius = 5,
                   stroke = FALSE,
                   fillColor = ~species_pal(soort),
                   fillOpacity = 0.9,
                   label = marker_labels,
                   group = "Alle Waarnemingspunten") %>% 
  
  fitBounds(
    lng1 = 5.466, lat1 = 50.933,
    lng2 = 5.540, lat2 = 50.985
  ) %>%
  
  # 4. Layer Control en Legend 
  addLayersControl(
    overlayGroups = c("HRG Gebieden",
                      paste("Aanwezig in HRG:", soorten_in_hbtrl),
                      paste("Nabij HRG:", unieke_soorten_nabij_hbtrl),
                      "Alle Waarnemingspunten"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(position = "bottomright",
            pal = species_pal, 
            values = alle_hrg_soorten_legend, 
            title = "Soort")