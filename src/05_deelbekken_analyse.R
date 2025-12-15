# ====================================================
# Scriptnaam: 05_deelbekken_analyse.R
# Auteur: Frédérique Steen
# Datum: 27-11-2025
# Beschrijving:
# - Genereert jpg kaarten per soort gebruikmakend van de project baseplot.
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")
library(ggplot2) 
library(rlang)   
library(sf)

# --- 1. Data inlezen ---

# A. Analyse dataset laden
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# B. Kaartlagen voorbereiden
message("Kaartlagen laden...")

# 1. Deelbekken via API
deelbekkens_sf <- st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  st_make_valid() 

# 2. Baseplot genereren
message("Baseplot genereren...")
base_map <- get_baseplot() 

# 3. Grenzen ophalen uit de base_map
# In config.R is 'vlaanderen' de eerste laag (layers[[1]])
vlaanderen_sf <- base_map$layers[[1]]$data

# 4. Transformatie deelbekkens naar Lambert 72 (EPSG:31370)
target_crs <- 31370
deelbekkens_sf <- st_transform(deelbekkens_sf, target_crs)

# Map voor output
dir_maps <- file.path(dir_data_output, "maps", "deelbekken")
if (!dir.exists(dir_maps)) dir.create(dir_maps, recursive = TRUE)

# --- 2. Loop over elke soort ---
message("Start genereren van kaarten...")

for (species_name in gbif_species) {
  
  # Kolomnaam bepalen
  sp_col <- tolower(species_name)
  
  if (!sp_col %in% names(df_analyse)) {
    message(paste("LET OP: Kolom", sp_col, "niet gevonden. Sla over."))
    next
  }
  
  # --- 3. Data prepareren ---
  df_sp <- df_analyse %>%
    filter(.data[[sp_col]] == 1) %>%
    mutate(
      has_vhag   = !is.na(VHAG),
      # String exact gelijk aan scale_color_manual
      point_type = if_else(has_vhag, "open systeem", "gesloten systeem - niet gekoppeld")
    )
  
  n_obs <- nrow(df_sp)
  
  if (n_obs == 0) {
    message(paste("  - Geen waarnemingen voor", species_name))
    next
  }
  
  message(paste("Bezig met:", species_name, "(", n_obs, "waarnemingen )"))
  
  # --- 4. Spatial Join ---
  points_sf <- st_as_sf(df_sp, coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(target_crs)
  
  points_joined <- st_join(points_sf, deelbekkens_sf, join = st_intersects)
  
  poly_id_col <- "DEELBID" 
  if (!poly_id_col %in% names(deelbekkens_sf)) poly_id_col <- names(deelbekkens_sf)[1]
  
  counts_per_poly <- points_joined %>%
    st_drop_geometry() %>%
    filter(has_vhag == TRUE) %>%
    filter(!is.na(.data[[poly_id_col]])) %>%
    count(.data[[poly_id_col]], name = "n_obs_open")
  
  map_polygons_colored <- deelbekkens_sf %>%
    inner_join(counts_per_poly, by = poly_id_col) %>% 
    filter(n_obs_open > 0)
  
  # --- 5. Plotten ---
  
  p <- base_map +
    
    # Laag 1: Specifieke deelbekken inkleuren
    geom_sf(
      data = map_polygons_colored, 
      aes(fill = "Aanwezigheid in open systeem"), # Let op: exacte match met scale_fill
      color = NA,       
      linewidth = 0,
      alpha = 0.5 
    ) +
    
    # Laag 2: Alle deelbekken contouren
    geom_sf(data = deelbekkens_sf, fill = NA, color = "grey60", linewidth = 0.2) +
    
    # Laag 3: Punten
    geom_sf(data = points_sf, aes(color = point_type), size = 1.5, alpha = 0.8) +
    
    # Kleurschalen
    scale_fill_manual(
      values = c("Aanwezigheid in open systeem" = "#3182bd"), 
      name = "Status van het deelbekken"
    ) +
    
    scale_color_manual(
      values = c("open systeem" = "red", "gesloten systeem - niet gekoppeld" = "orange"), 
      name = "Waarneming in:"
    ) +
    
    # Zorg dat de legende voor de polygonen zichtbaar is
    guides(
      fill = guide_legend(
        override.aes = list(color = "black", linewidth = 0.2, alpha = 1)
      )
    ) +
    
    # Focus op Vlaanderen
    coord_sf(
      xlim = st_bbox(vlaanderen_sf)[c(1,3)], 
      ylim = st_bbox(vlaanderen_sf)[c(2,4)],
      expand = FALSE
    ) +
    
    # Layout
    labs(
      title = paste0("Waarnemingen van ", species_name, " op deelbekkenniveau"),
      subtitle = paste("Aantal waarnemingen:", n_obs),
      x = NULL, y = NULL
    ) +
    
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold.italic", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10, t=10)),
      legend.title = element_text(face="bold", size=10),
      legend.text = element_text(size=9)
    )
  
  # --- 6. Opslaan ---
  file_name <- paste0("map_", gsub(" ", "_", species_name), "_deelbekken.jpg")
  output_path <- file.path(dir_maps, file_name)
  
  ggsave(output_path, plot = p, width = 10, height = 8, dpi = 300)
}

message("Klaar! JPG kaarten opgeslagen in: ", dir_maps)