# ====================================================
# Scriptnaam: 06_deelbekken_leaflet.R
# Project: Craywatch
# Datum: 27-11-2025
# Beschrijving:
# - Genereert leaflet kaarten per soort.
# - Doel: Visualiseren van de ruimtelijke distributie t.o.v. de deelbekkens.
# - Bekkenstatus: rood gekleurd als er waarnemingen in het open systeem (VHAG) zijn.
# - Punten: rood (VHAG-gekoppeld) vs. oranje (WVLC/Niet-gekoppeld).
# ====================================================

# --- 0. Instellingen laden ---
source("./src/config.R")
library(htmlwidgets) # Nodig om kaarten op te slaan
library(RColorBrewer)
library(rlang)

# --- 1. Data inlezen ---

# A. Analyse dataset laden
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# B. Shapefiles (deelbekkens) - Via API
message("Deelbekken ophalen via API...")

# Deelbekken inlezen, valide maken en naar wgs84 transformeren voor leaflet
subbekkens_sf <- st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_wgs84) 

# Output map
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
      # Kleur logica voor punten
      has_vhag   = !is.na(VHAG),
      point_type = if_else(has_vhag, "VHAG (Open)", "WVLC/Niet gekoppeld"),
      point_color = if_else(has_vhag, "#cb181d", "orange") # Donkerrood voor VHAG punten
    )
  
  n_obs <- nrow(df_sp)
  
  if (n_obs == 0) {
    message(paste("  - Geen waarnemingen voor", species_name))
    next
  }
  
  message(paste("Bezig met:", species_name, "(", n_obs, "waarnemingen )"))
  
  # --- 4. Spatial join & aggregatie ---
  
  # Punten naar sf - wgs84(!)
  points_sf <- st_as_sf(df_sp, coords = c("Longitude", "Latitude"), crs = crs_wgs84, remove = FALSE)
  
  # Voeg info van deelbekken toe aan de punten
  points_joined <- st_join(points_sf, subbekkens_sf, join = st_intersects)
  
  # Bepaal ID kolom
  poly_id_col <- "DEELBID" 
  if (!poly_id_col %in% names(subbekkens_sf)) poly_id_col <- names(subbekkens_sf)[1]
  
  # Tel 'open' waarnemingen per bekken
  counts_per_poly <- points_joined %>%
    st_drop_geometry() %>%
    filter(has_vhag == TRUE) %>%
    filter(!is.na(.data[[poly_id_col]])) %>%
    count(.data[[poly_id_col]], name = "n_obs_open")
  
  # Koppel terug aan polygonen en filter/kleur
  map_polygons <- subbekkens_sf %>%
    left_join(counts_per_poly, by = poly_id_col) %>% 
    mutate(
      n_obs_open = replace_na(n_obs_open, 0),
      is_active = n_obs_open > 0,
      # Binaire kleuring: Actief (Rood-tint) of Inactief (Transparant)
      fill_color = if_else(is_active, "#fb6a4a", "transparent") 
    )
  
  # --- 5. leaflet maken ---
  
  # Popup tekst voor polygonen
  map_polygons$poly_popup <- paste0(
    "<b>Deelbekken:</b> ", if("DEELBEKNM" %in% names(map_polygons)) map_polygons$DEELBEKNM else "Onbekend", "<br>",
    "<b>ID:</b> ", map_polygons[[poly_id_col]], "<br>",
    "<b>Open Systeem (VHAG) waarnemingen:</b> ", map_polygons$n_obs_open
  )
  
  # Popup tekst voor punten
  points_sf$popup_text <- paste0(
    "<b>Soort:</b> ", species_name, "<br>",
    "<b>Status:</b> ", points_sf$point_type, "<br>",
    "<b>Bron:</b> ", points_sf$dat.source, "<br>",
    "<b>Datum:</b> ", points_sf$date, "<br>",
    "<hr>",
    "<b>VHAG:</b> ", ifelse(is.na(points_sf$VHAG), "-", points_sf$VHAG), "<br>",
    "<b>WVLC:</b> ", ifelse(is.na(points_sf$WVLC), "-", points_sf$WVLC)
  )
  
  m <- leaflet(options = leafletOptions(minZoom = 7)) %>% # minZoom om op Vlaanderen te blijven
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Alle deelbekken grenzen als achtergrond 
    addPolygons(
      data = subbekkens_sf,
      fill = FALSE, 
      color = "red", 
      weight = 0.5, 
      group = "Alle Grenzen"
    ) %>%
    
    # Laag 1: Deelbekken gekleurd indien actief
    addPolygons(
      data = map_polygons,
      fillColor = ~fill_color, 
      fillOpacity = ~if_else(is_active, 0.6, 0.0), # Actieve bekkens half-transparant
      color = "red", # randkleur (Rood voor actieve bekkens, om de rode vulling te omsluiten)
      weight = 0.5, 
      popup = ~poly_popup, 
      group = "Deelbekken"
    ) %>%
    
    # Laag 2: Waarnemingen 
    addCircleMarkers(
      data = points_sf, 
      lng = ~Longitude, 
      lat = ~Latitude,
      color = ~point_color, 
      fillColor = ~point_color, 
      fillOpacity = 0.8,
      radius = 3, 
      weight = 1, 
      popup = ~popup_text,
      group = "Waarnemingen"
    ) %>%
    
    # Legende punten & polygonen 
    addControl(
      html = paste0(
        # Container style:
        # - width: 250px (zorgt dat de tekst ruimte heeft en niet raar afbreekt)
        # - color: black (forceert zwarte tekst)
        "<div style='background:white; padding:8px; border:1px solid #ccc; font-size:12px; line-height:1.5; width:250px; color:black;'>",
        
        # Titel:
        "<b>Waarnemingen van <span style='font-style:italic;'>", species_name, "</span></b><br>",
        "<b>op deelbekkenniveau</b>",
        
        "<hr style='margin: 6px 0;'>",
        
        # Polygoon legende
        "<div style='margin-bottom: 5px;'>",
        "<p style='margin: 0; font-weight:bold;'>Status van deelbekken</p>",
        "<i style='background:#fb6a4a; width:12px; height:12px; display:inline-block; opacity:0.6; border:1px solid red; margin-right:5px; vertical-align:middle;'></i>",
        "<span style='vertical-align:middle;'>aanwezig in open systeem</span>",
        "</div>",
        
        # Punten legende
        "<div>",
        "<p style='margin: 0 0 4px 0; font-weight:bold;'>Waarneming in:</p>",
        
        # Item 1
        "<div style='margin-bottom:2px;'>",
        "<i style='background:#cb181d; width:12px; height:12px; display:inline-block; border-radius:50%; margin-right:5px; vertical-align:middle;'></i>",
        "<span style='vertical-align:middle;'>open systeem</span>",
        "</div>",
        
        # Item 2
        "<div>",
        "<i style='background:orange; width:12px; height:12px; display:inline-block; border-radius:50%; margin-right:5px; vertical-align:middle;'></i>",
        "<span style='vertical-align:middle;'>gesloten systeem</span>",
        "</div>",
        
        "</div>",
        "</div>"
      ),
      position = "bottomright"
    )
  
  # --- 6. Opslaan als HTML ---
  file_name <- paste0("kaart_", gsub(" ", "_", species_name), ".html")
  output_path <- file.path(dir_maps, file_name)
  
  saveWidget(m, file = output_path, selfcontained = TRUE)
}

message("Klaar! Leaflets opgeslagen in: ", dir_maps)