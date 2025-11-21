# ====================================================
# Scriptnaam: create_craywatch_maps.R
# Auteur: Margot Vermeylen
# Datum: 09-01-2025
# Beschrijving: 
# Dit script genereert twee Craywatch kaarten met de absence/presence waarnemingen van 2024
# en een kaart met de Craywatch punten bemonsterd in cat 0 en 1 waterlopen
# Het script genereert ook de input voor Select_municipalities
# ====================================================

#R - libraries
library(ggspatial)
library(sf)
library(tidyverse)
library(dplyr)
library(scales)
library(osmdata)
library(ggplot2)
library(tidyr)
library(lubridate)

# read craywatch data
full_data <- read_csv("~/GitHub/Craywatch-Rapport/R/data/output/analyse_dataset.csv")
craywatch_data <- full_data %>%
  filter(dat.source == "craywatch_data")

craywatch_sf <- st_as_sf(craywatch_data, coords = c("longitude", "latitude"), crs = 4326)
  
# Read shapefiles
vlaanderen <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/grenzenvlaanderen.shp")
hoofdrivieren <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/hoofdrivieren.shp")
kanalen <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/kanalen.shp")
gemeenten <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/gemeenten.shp")

# Zorg dat alle shapefiles dezelfde CRS hebben
vlaanderen <- st_transform(vlaanderen, st_crs(hoofdrivieren))
hoofdrivieren <- st_transform(hoofdrivieren, st_crs(vlaanderen))
kanalen <- st_transform(kanalen, st_crs(vlaanderen))
gemeenten <- st_transform(gemeenten, st_crs(vlaanderen))

# Clip de shapefiles tot de grenzen van Vlaanderen
hoofdrivieren_in_vlaanderen <- st_intersection(hoofdrivieren, vlaanderen)
kanalen_in_vlaanderen <- st_intersection(kanalen, vlaanderen)
gemeenten_in_vlaanderen <- st_intersection(gemeenten, vlaanderen)

sf_use_s2(FALSE)

# Make plot
base_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.4, colour="#6BA1D3")+
  geom_sf(data = kanalen_in_vlaanderen, size=0.4, colour="#6BA1D3")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf()
  # annotation_scale(location = "tl", style = "ticks", width_hint = 0.15) +
  # annotation_north_arrow(
  #   location = "tr",
  #   which_north = "true",
  #   pad_x = unit(0.5, "cm"),
  #   pad_y = unit(0.1, "cm"),
  #   style = north_arrow_fancy_orienteering(fill = c("black", "black"), line_width = 0.3),
  #   scale = 0.1 # Maak de noordpijl kleiner
  #   )

# Make plot
gemeente_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
  geom_sf(data = gemeenten_in_vlaanderen, size=0.1, colour="lightgrey", fill="#EEEEEE")+
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.4, colour="#6BA1D3")+
  geom_sf(data = kanalen_in_vlaanderen, size=0.4, colour="#6BA1D3")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf()

# Define a color palette for species
species_colors <- c("Afwezigheid" = "#BEBEBE", "faxonius limosus" = "#FFD700",
                    "procambarus clarkii" = "#FF0000", "procambarus virginalis" = "#FF00FF",
                    "faxonius virilis" = "#FF8C00", "procambarus acutus" = "#000000")

# Update the legend labels for species with italic formatting
species_labels <- c( "faxonius limosus" = expression(italic("Faxonius limosus")),
                     "procambarus clarkii" = expression(italic("Procambarus clarkii")), 
                     "procambarus virginalis" = expression(italic("Procambarus virginalis")), 
                     "faxonius virilis" = expression(italic("Faxonius virilis")), 
                     "procambarus acutus" = expression(italic("Procambarus acutus")), 
                     "Afwezigheid" = expression(italic("Absence")))

# Update the legend labels for species in Dutch 
species_labels_dutch <- c( "faxonius limosus" = expression("gevlekte Amerikaanse rivierkreeft"),
                     "procambarus clarkii" = expression("rode Amerikaanse rivierkreeft"), 
                     "procambarus virginalis" = expression("marmerkreeft"), 
                     "faxonius virilis" = expression("geknobbelde Amerikaanse rivierkreeft"), 
                     "procambarus acutus" = expression("gestreepte Amerikaanse rivierkreeft"), 
                     "Afwezigheid" = expression("afwezigheid"))

# # Create a color scale with updated labels
# color_scale <- scale_color_manual(values = species_colors, labels = species_labels)
# 
# # Create a color scale with updated labels in Dutch
# color_scale_dutch <- scale_color_manual(values = species_colors, labels = species_labels_dutch)

craywatch_df_clean <- craywatch_sf %>%
  st_drop_geometry() %>% # Verwijder de geometrie-kolom (de lijst)
  as_tibble() # Maak er een reguliere data frame van

# identificeer de soortkolommen
species_columns <- c("faxonius limosus", "procambarus clarkii", "procambarus acutus", "procambarus virginalis", "faxonius virilis")

df_geanalyseerd <- craywatch_df_clean %>%
  mutate(
    across(all_of(species_columns), ~as.numeric(as.character(.)))
  ) %>%
  mutate(
    # Nu werkt as.matrix() en rowSums() probleemloos
    species_matrix = as.matrix(.[, species_columns]), 
    is_aanwezig = rowSums(species_matrix == 1, na.rm = TRUE) > 0,
    status = case_when(
      is_aanwezig == TRUE ~ "Aanwezigheid",
      TRUE ~ "Afwezigheid"
    )
  ) %>%
  select(-species_matrix, -is_aanwezig) # Verwijder de hulpkolommen

sf_geanalyseerd <- craywatch_sf %>%
  mutate(status = df_geanalyseerd$status)

sf_afwezigheid <- sf_geanalyseerd %>%
  filter(status == "Afwezigheid") %>%
  # Geef ze de status 'Afwezigheid' voor de kleur
  mutate(species = "Afwezigheid") %>%
  select(geometry, species)

sf_aanwezigheid_gekleurd <- sf_geanalyseerd %>%
  filter(status == "Aanwezigheid") %>%
  # Converteer van breed naar lang formaat: elke aanwezigheid krijgt een eigen rij
  pivot_longer(
    cols = all_of(species_columns),
    names_to = "species",
    values_to = "is_present"
  ) %>%
  # Filter alleen de waarnemingen waar de soort aanwezig is (is_present == 1)
  filter(is_present == 1) %>%
  select(geometry, species) 


# Plot 'crayfish.indet' (absence) points first, then other species
species_plot <- base_plot +
  geom_sf(data = sf_afwezigheid, aes(color = species), size = 1) +  
  geom_sf(data = sf_aanwezigheid_gekleurd, aes(color = species), size = 1) +   
  scale_color_manual(
    values = species_colors,
    labels = species_labels_dutch,
    breaks = names(species_colors)) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5), # Grotere bolletjes
      ncol = 2 # FORCEREN VAN TWEE KOLOMMEN
    ))

# Plot craywatch map with municipalities
species_plot_gemeente <- gemeente_plot +
  geom_sf(data = sf_afwezigheid, aes(color = species), size = 1) +  
  geom_sf(data = sf_aanwezigheid_gekleurd, aes(color = species), size = 1) +   
  scale_color_manual(
    values = species_colors,
    labels = species_labels,
    breaks = names(species_colors)) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5), # Grotere bolletjes
      ncol = 2 # FORCEREN VAN TWEE KOLOMMEN
    ))

# Plot craywatch map with municipalities
species_plot_dutch <- gemeente_plot +
  geom_sf(data = sf_afwezigheid, aes(color = species), size = 1) +  
  geom_sf(data = sf_aanwezigheid_gekleurd, aes(color = species), size = 1) +   
  scale_color_manual(
    values = species_colors,
    labels = species_labels_dutch) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5), # Grotere bolletjes
      ncol = 2 # FORCEREN VAN TWEE KOLOMMEN
    ))

# Save the plot
ggsave(species_plot, file = "~/GitHub/Craywatch-Rapport/R/data/output/craywatch_maps/total_craywatch_map.png", 
       width = 15, height = 8, units = "cm", dpi = 400)

# Save the plot (gemeente)
ggsave(species_plot_gemeente, file = "~/GitHub/Craywatch-Rapport/R/data/output/craywatch_maps/validated_craywatch_map_gemeenten.png", 
       width = 15, height = 8, units = "cm", dpi = 400)

# Save the plot (Dutch)
ggsave(species_plot_dutch, file = "~/GitHub/Craywatch-Rapport/R/data/output/craywatch_maps/validated_craywatch_map_dutch.png", 
       width = 15, height = 8, units = "cm", dpi = 400)


# ===================================================================================================================
## Make plot of observations in cat 1 waterways
# shapefile inlezen

waterlopen <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/vhaCattraj.shp")
load("~/GitHub/Craywatch-Rapport/R/data/input/fc_data/analyses_dataset_VHAG_WVLC_CATC_rivier_breedte.Rdata")
CF <- data

CW <- CF %>%
  filter(dat.source == "craywatch_data")

# cat1 waterlopen er uit halen
cat1_waterlopen <- waterlopen %>%
  filter(CATC == 1)

# cat0 waterlopen er uit halen
cat0_waterlopen <- waterlopen %>%
  filter(CATC == 0)

cat1_waterlopen <- st_transform(cat1_waterlopen, st_crs(vlaanderen))
cat0_waterlopen <- st_transform(cat0_waterlopen, st_crs(vlaanderen))
cat1_waterlopen_vlaanderen <- st_intersection(cat1_waterlopen, vlaanderen)
cat0_waterlopen_vlaanderen <- st_intersection(cat0_waterlopen, vlaanderen)

target_crs <- 3857 
jitter_afstand_meters <- 3000 # Jitter

CW_sf <- st_as_sf(CW, coords = c("longitude", "latitude"), crs = 4326)

CW_df_clean <- CW_sf %>%
  st_drop_geometry() %>% # Verwijder de geometrie-kolom (de lijst)
  as_tibble() # Maak er een reguliere data frame van

# identificeer de soortkolommen
species_columns <- c("faxonius.limosus", "procambarus.clarkii", "procambarus.acutus", "procambarus.virginalis", "faxonius.virilis")

CW_df_geanalyseerd <- CW_df_clean %>%
  mutate(
    across(all_of(species_columns), ~as.numeric(as.character(.)))
  ) %>%
  mutate(
    # Nu werkt as.matrix() en rowSums() probleemloos
    species_matrix = as.matrix(.[, species_columns]), 
    is_aanwezig = rowSums(species_matrix == 1, na.rm = TRUE) > 0,
    status = case_when(
      is_aanwezig == TRUE ~ "Aanwezigheid",
      TRUE ~ "Afwezigheid"
    )
  ) %>%
  select(-species_matrix, -is_aanwezig) # Verwijder de hulpkolommen

CW_sf_geanalyseerd <- CW_sf %>%
  mutate(status = CW_df_geanalyseerd$status) %>%
  filter(!is.na(CATC)) %>%
  filter(CATC == 0 | CATC == 1)

afwezigheid_CATC <- CW_sf_geanalyseerd %>%
  filter(status == "Afwezigheid") %>%
  # Geef ze de status 'Afwezigheid' voor de kleur
  mutate(species = "Afwezigheid") %>%
  select(geometry, species, CATC)

aanwezigheid_CATC <- CW_sf_geanalyseerd %>%
  filter(status == "Aanwezigheid") %>%
  # Converteer van breed naar lang formaat: elke aanwezigheid krijgt een eigen rij
  pivot_longer(
    cols = all_of(species_columns),
    names_to = "species",
    values_to = "is_present"
  ) %>%
  # Filter alleen de waarnemingen waar de soort aanwezig is (is_present == 1)
  filter(is_present == 1) %>%
  select(geometry, species, CATC)

# --- Transformeer ALLE achtergrond shapefiles ---
vlaanderen_proj <- st_transform(vlaanderen, target_crs)
gemeenten_in_vlaanderen_proj <- st_transform(gemeenten_in_vlaanderen, target_crs)
cat1_waterlopen_vlaanderen_proj <- st_transform(cat1_waterlopen_vlaanderen, target_crs)
cat0_waterlopen_vlaanderen_proj <- st_transform(cat0_waterlopen_vlaanderen, target_crs)

species_colors_catc <- c("Afwezigheid" = "#BEBEBE", "faxonius.limosus" = "#FFD700",
                    "procambarus.clarkii" = "#FF0000", "procambarus.virginalis" = "#FF00FF",
                    "faxonius.virilis" = "#FF8C00", "procambarus.acutus" = "#000000")

species_labels_catc <- c( "faxonius.limosus" = expression("gevlekte Amerikaanse rivierkreeft"),
                           "procambarus.clarkii" = expression("rode Amerikaanse rivierkreeft"), 
                           "procambarus.virginalis" = expression("marmerkreeft"), 
                           "faxonius.virilis" = expression("geknobbelde Amerikaanse rivierkreeft"), 
                           "procambarus.acutus" = expression("gestreepte Amerikaanse rivierkreeft"), 
                           "Afwezigheid" = expression("afwezigheid"))

# Make plot
catc_plot <- ggplot() +
  geom_sf(data = vlaanderen_proj, fill= "#EEEEEE", size=0.2, colour= "black") +
  geom_sf(data = gemeenten_in_vlaanderen_proj, size=0.1, colour="lightgrey", fill="#EEEEEE")+
  geom_sf(data = cat1_waterlopen_vlaanderen_proj, size=0.4, colour="#99CCFF")+
  geom_sf(data = cat0_waterlopen_vlaanderen_proj, size=0.3, colour="#004C99")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf(crs = target_crs)


# --- Transformeer de Punten Data ---
sf_afwezigheid_cat_proj <- st_transform(afwezigheid_CATC, target_crs)
sf_aanwezigheid_cat_proj <- st_transform(aanwezigheid_CATC, target_crs)

# --- Extraheer de geprojecteerde coördinaten voor geom_point ---
sf_aanwezigheid_cat_proj <- sf_aanwezigheid_cat_proj %>%
  mutate(
    X_proj = st_coordinates(.)[, 1],
    Y_proj = st_coordinates(.)[, 2]
  )

# Voeg clarkii in de ijzer manueel toe (niet automatisch toegekend naar stomend systeem)
punt_clarkii_ijzer <- data.frame(
  longitude = c(2.81524, 2.83965),
  latitude = c(50.98940, 51.00636),
  species = c("procambarus.clarkii", "procambarus.clarkii")
)

# Zet om naar sf
punt_clarkii_ijzer_sf <- st_as_sf(punt_clarkii_ijzer, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326)
# Transformeer
punt_clarkii_ijzer_sf <- st_transform(punt_clarkii_ijzer_sf, target_crs)

# Plot on the gemeente map
species_plot_catc <- catc_plot +
  geom_sf(data = sf_afwezigheid_cat_proj, aes(color = species), size = 0.8, alpha = 0.8) +  
  geom_point(data = sf_aanwezigheid_cat_proj, 
             aes(x = X_proj, y = Y_proj, color = species),
             size = 1, 
             # Jitter-afstand in meters
             position = position_jitter(width = jitter_afstand_meters, height = jitter_afstand_meters)) +  
  geom_sf(data = punt_clarkii_ijzer_sf, 
          aes(color = species), 
          size = 1) +
  scale_color_manual(
    values = species_colors_catc,
    labels = species_labels_catc,
    breaks = names(species_colors_catc)) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5), # Grotere bolletjes
      ncol = 2 # FORCEREN VAN TWEE KOLOMMEN
    ))


# Save the plot
ggsave(species_plot_catc, file = "~/GitHub/Craywatch-Rapport/R/data/output/craywatch_maps/cat0+1_craywatch_map.png", 
       width = 15, height = 8, units = "cm", dpi = 400)


