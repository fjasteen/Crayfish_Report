# ====================================================
# Scriptnaam: 04_deelbekken_analyse.R
# Auteur: Frédérique Steen
# Datum: 27-11-2025
# Beschrijving:
# - Genereert jpg kaarten per soort.
# - Doel: Visualiseren van de ruimtelijke distributie t.o.v. de deelbekkens.
# - Bekkenstatus: rood gekleurd als er waarnemingen in het open systeem (VHAG) zijn.
# - Punten: rood (VHAG-gekoppeld) vs. oranje (WVLC/Niet-gekoppeld).
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")
library(ggplot2) # Nodig voor statische plots
library(rlang)   # Voor !!sym() / .data[[]]
library(sf)

# --- 1. Data inlezen ---

# A. Analyse dataset laden
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# B. Shapefiles
message("Kaartlagen laden...")

# 1. Deelbekken via API
subbekkens_sf <- st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  st_make_valid() 

# 2. Vlaanderen grenzen (voor de uitsnede en achtergrond)
if (!file.exists(file_vlaanderen_grenzen)) stop("Shapefile Vlaanderen niet gevonden!")
vlaanderen_sf <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>%
  st_make_valid()

# --- Transformatie naar Lambert 72 (EPSG:31370) ---
# Voor statische kaarten van Vlaanderen is dit mooier dan WGS84 (minder vervorming)
target_crs <- 31370

message("Transformeren naar Lambert 72...")
subbekkens_sf <- st_transform(subbekkens_sf, target_crs)
vlaanderen_sf <- st_transform(vlaanderen_sf, target_crs)

# Map voor output aanpassen naar ./maps/deelbekken
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
  
  # Filter data voor deze soort
  df_sp <- df_analyse %>%
    filter(.data[[sp_col]] == 1) %>%
    mutate(
      has_vhag   = !is.na(VHAG),
      point_type = if_else(has_vhag, "VHAG (Open)", "WVLC/Niet gekoppeld")
    )
  
  n_obs <- nrow(df_sp)
  
  if (n_obs == 0) {
    message(paste("  - Geen waarnemingen voor", species_name))
    next
  }
  
  message(paste("Bezig met:", species_name, "(", n_obs, "waarnemingen )"))
  
  # --- 4. Spatial Join ---
  
  # Punten naar sf en transformeren naar Lambert
  points_sf <- st_as_sf(df_sp, coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(target_crs)
  
  # Join punten met deelbekken
  points_joined <- st_join(points_sf, subbekkens_sf, join = st_intersects)
  
  # Bepaal ID kolom
  poly_id_col <- "DEELBID" 
  if (!poly_id_col %in% names(subbekkens_sf)) poly_id_col <- names(subbekkens_sf)[1]
  
  # Tel waarnemingen in open systemen per bekken
  counts_per_poly <- points_joined %>%
    st_drop_geometry() %>%
    filter(has_vhag == TRUE) %>%
    filter(!is.na(.data[[poly_id_col]])) %>%
    count(.data[[poly_id_col]], name = "n_obs_open")
  
  # Koppel terug aan polygonen en filter meteen
  # We houden enkel de polygonen over die we willen kleuren (n_obs_open > 0)
  map_polygons_colored <- subbekkens_sf %>%
    inner_join(counts_per_poly, by = poly_id_col) %>% # behoudt enkel matches
    filter(n_obs_open > 0)
  
  # --- 5. Plotten met GGPLOT2 ---
  
  p <- ggplot() +
    # Laag 1: Vlaanderen als grijze achtergrond/border
    geom_sf(data = vlaanderen_sf, fill = "grey95", color = "black", linewidth = 0.5) +
    
    # Laag 2: ALLE deelbekken contouren 
    geom_sf(data = subbekkens_sf, fill = NA, color = "grey70", linewidth = 0.2) +
    
    # Laag 3: Specifieke deelbekken inkleuren 
    geom_sf(
      data = map_polygons_colored, 
      aes(fill = "Aanwezig in Open Systeem"), 
      color = "grey50", # Iets donkerder randje voor de actieve bekkens
      linewidth = 0.2,
      alpha = 0.7
    ) +
    
    # Laag 4: Punten
    geom_sf(data = points_sf, aes(color = point_type), size = 1.5, alpha = 0.8) +
    
    # Kleurschalen
    scale_fill_manual(
      values = c("Aanwezig in open systeem" = "#3182bd"), # Mooi blauw
      name = "Status van het deelbekken"
    ) +
    
    scale_color_manual(
      values = c("VHAG (Open)" = "red", "WVLC & niet gekoppeld" = "orange"),
      name = "Type Waarneming"
    ) +
    
    # Focus op Vlaanderen (BBox)
    coord_sf(
      xlim = st_bbox(vlaanderen_sf)[c(1,3)], 
      ylim = st_bbox(vlaanderen_sf)[c(2,4)],
      expand = FALSE
    ) +
    
    # Layout en titels
    labs(
      title = species_name,
      subtitle = paste("Totaal waarnemingen:", n_obs),
      x = NULL, y = NULL
    ) +
    theme_void() + 
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # --- 6. Opslaan als JPG ---
  file_name <- paste0("kaart_", gsub(" ", "_", species_name), ".jpg")
  output_path <- file.path(dir_maps, file_name)
  
  ggsave(output_path, plot = p, width = 10, height = 8, dpi = 300)
}

message("Klaar! JPG kaarten opgeslagen in: ", dir_maps)