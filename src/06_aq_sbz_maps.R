# ====================================================
# Scriptnaam: 06_aq_sbz_analyse.R
# Auteur: Frédérique Steen
# Datum: 27-11-2025 (Updated)
# Beschrijving: 
# - Genereert statische kaarten voor het rapport
# - Gebruikt SBZ-H en SBP 
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")

# Libraries zijn al geladen in config, maar voor zekerheid:
library(tidyverse)
library(sf)
library(leaflet)
library(htmltools) # voor HTML() in labels

# --- 1. Ruimtelijke lagen opladen ---
message("Ruimtelijke lagen inladen...")
# Laadt alle beschermingslagen en de gefilterde kreeftendata (CF_presence)
# Hierdoor zijn de variabelen 'natura_2000_aq' en 'CF_presence' beschikbaar.
source("./src/05_load_aq_sbz.R")

# Vlaanderen (Referentie)
vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>%
  st_transform(crs_lambert)

# Helper functie: Clip lagen naar Vlaanderen
clip_to_vlaanderen <- function(x) {
  x %>%
    st_transform(crs_lambert) %>%
    st_intersection(vlaanderen)
}

# Basislagen
hoofdrivieren <- st_read(file_hoofdrivieren, quiet = TRUE) %>% clip_to_vlaanderen()
kanalen       <- st_read(file_kanalen, quiet = TRUE) %>% clip_to_vlaanderen()
# gemeenten     <- st_read(file_gemeenten, quiet = TRUE) %>% clip_to_vlaanderen() # Niet gebruikt in plot?

# --- 2. Statische Kaarten (baseplot) ---
base_plot <- get_baseplot()

# Functie om kaarten te plotten en op te slaan
plot_protected_map <- function(base_layer, points_layer, outfile, 
                               line_layer = NULL,     # Optionele laag voor lijnen
                               fill_col = "darkgreen", 
                               line_col = "blue",     # Kleur voor de lijnen
                               alpha = 0.8) {
  
  # Start met de basislaag (polygonen/vlakken)
  p <- base_plot +
    geom_sf(data = base_layer, 
            fill = fill_col, 
            colour = NA,       
            alpha = alpha)
  
  # Als er een lijnlaag is (zoals sbp_vissen), voeg deze toe
  if (!is.null(line_layer)) {
    p <- p + 
      geom_sf(data = line_layer, 
              colour = line_col, 
              size = 0.8,      
              fill = NA)
  }
  
  # Voeg de punten toe en de styling
  p <- p +
    geom_sf(data = points_layer, aes(color = species), size = 0.4) +
    color_scale_dutch +
    guides(
      color = guide_legend(
        override.aes = list(size = 3),
        ncol = 2
      )
    ) +
    coord_sf() 
  
  ggsave(p, file = outfile, width = 15, height = 8, units = "cm", dpi = 300)
  message(paste("Kaart opgeslagen:", outfile))
}


# 1. aquatische natura2000 
plot_protected_map(
  base_layer = natura_2000_aq,
  points_layer = CF_presence,
  outfile = file.path(dir_bescherming_output, "map_natura_2000.png")
)

# 2. habitatrichtlijn 
plot_protected_map(
  base_layer = hbtrl,
  points_layer = CF_presence,
  outfile = file.path(dir_bescherming_output, "map_hbtrl.png")
)

# 3. SBP 
# Hier splitsen we de lagen op in base_layer & line_layer (vis)
plot_protected_map(
  base_layer   = sbp_pgs_aq,     # De groene vlakken
  line_layer   = sbp_vissen %>% st_cast("MULTILINESTRING"),     # De blauwe lijnen
  points_layer = CF_presence,
  outfile      = file.path(dir_bescherming_output, "map_sbp.png"),
  fill_col     = "darkgreen",
  line_col     = "lightgreen",         # Blauw voor de waterlopen
  alpha        = 0.8
)


