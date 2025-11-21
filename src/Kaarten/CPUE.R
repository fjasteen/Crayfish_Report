#R - libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(readr)

# lees craywatch + gbif dataset in
occurrences <- read_csv("~/GitHub/Craywatch-Rapport/R/data/output/analyse_dataset.csv")

# Read individual shapefiles
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
hoofdrivieren <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/hoofdrivieren.shp")
kanalen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/kanalen.shp")
provincies <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/provincies.shp")

# Zorg dat alle shapefiles dezelfde CRS hebben
vlaanderen <- st_transform(vlaanderen, st_crs(hoofdrivieren))
hoofdrivieren <- st_transform(hoofdrivieren, st_crs(vlaanderen))
kanalen <- st_transform(kanalen, st_crs(vlaanderen))
provincies <- st_transform(provincies, st_crs(vlaanderen))

# Clip de shapefiles tot de grenzen van Vlaanderen
hoofdrivieren_in_vlaanderen <- st_intersection(hoofdrivieren, vlaanderen)
kanalen_in_vlaanderen <- st_intersection(kanalen, vlaanderen)
provincies_in_vlaanderen <- st_intersection(provincies, vlaanderen)

sf_use_s2(FALSE)

# Filter the provincies shapefile for Flanders (assuming the province names are in Dutch)
flanders_provincies <- provincies %>%
  filter(PROVNAAM %in% c("Antwerpen", "Limburg", "Oost-Vlaanderen", "Vlaams-Brabant", "West-Vlaanderen"))

# Make plot
base_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
  geom_sf(data = provincies_in_vlaanderen, size=0.2, fill = "#EEEEEE", colour="grey")+
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.3, colour="#6BA1D3", alpha = 0.8)+
  geom_sf(data = kanalen_in_vlaanderen, size=0.3, colour="#6BA1D3", alpha = 0.8)+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.4, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf()

# Selecteer alleen de aanwezigheidskolommen
aanwezigheidskolommen <- c(
  "faxonius limosus", 
  "procambarus virginalis", 
  "procambarus acutus", 
  "faxonius virilis", 
  "procambarus clarkii", 
  "pontastacus leptodactylus", 
  "pacifastacus leniusculus"
)

# Bereken de aanwezigheidssom voor alle soorten
occurrences <- occurrences %>%
  rowwise() %>%
  # Gebruik `na.rm = TRUE` omdat we willen weten of *iets* gevonden is,
  # zelfs als sommige soorten onbekend zijn (NA).
  mutate(totaal_aanwezig = sum(c_across(all_of(aanwezigheidskolommen)), na.rm = TRUE)) %>%
  ungroup()

for (current_species in aanwezigheidskolommen) {
  
  # Bepaal de afwezigheden voor DEZE SPECIFIEKE SOORT
  # Dit zijn alle locaties waar de kolom van de huidige soort (current_species) 0 is (geen NA)
  afwezigheden_sf <- occurrences %>%
    filter(
      # De waarde van de huidige soort is een bevestigde 0 (geen NA)
      !!sym(current_species) == 0 
    ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Filter de data voor de HUIDIGE SOORT (waarbij de waarde 1 is)
  species_data_filtered <- occurrences %>%
    filter(!!sym(current_species) == 1)
  
  # Splits de data in craywatch en gbif (blijft hetzelfde)
  craywatch_sf <- species_data_filtered %>%
    filter(dat.source == "craywatch_data") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  gbif_sf <- species_data_filtered %>%
    filter(dat.source == "gbif_data") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Genereer de plot voor de huidige soort
  species_plot <- base_plot +
    # Voeg de afwezigheden toe als grijze kruisjes
    geom_sf(data = afwezigheden_sf, shape = 4, size = 1, color = "darkgrey", alpha = 0.5) +
    # Voeg de gbif-punten toe als zwarte punten 
    geom_sf(data = gbif_sf, size = 0.5, color = "#696969", show.legend = FALSE) +
    # Voeg de craywatch-punten toe met een kleurgradiënt 
    geom_sf(data = craywatch_sf, aes(color = !!sym(paste0("CPUE_", current_species))), size = 1, show.legend = TRUE) +
    scale_color_gradient(
      low = "yellow", 
      high = "red", 
      limits = c(0, 5),
      oob = scales::squish,
      name = "CPUE"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text()
    )
  # Sla de plot op voor de huidige soort
  ggsave(species_plot, file = paste0("~/SWO craywatch/R/output/maps/maps_species/maps_", current_species, ".png"),
         width = 15, height = 6.4, units = "cm", dpi = 400)
}

###################### Toename in km-hokken #############################
library(ggpattern)
install.packages("remotes")
library(remotes)

# Converteer de 'date' kolom naar een datumobject
occurrences$date <- as.Date(occurrences$date, format = "%Y-%m-%d")

# Maak een raster (grid) over Vlaanderen
grid <- st_make_grid(vlaanderen, cellsize = 1000, square = TRUE) %>%
  st_sf() %>%
  mutate(id = 1:n())

# Deel 2: Definieer de periodes
start_date_pre <- as.Date("2010-01-01")
end_date_pre <- as.Date("2024-05-31")

start_date_post <- as.Date("2010-01-01")
end_date_post <- Sys.Date()

# Maak een leeg dataframe om de resultaten op te slaan
resultaten_df <- data.frame(
  soort = character(),
  hokken_pre = integer(),
  hokken_post = integer(),
  toename_aantal = integer(),
  toename_procent = numeric(),
  stringsAsFactors = FALSE
)

for (current_species in aanwezigheidskolommen) {
  
  # Filter de data voor de huidige soort
  species_data <- occurrences %>%
    filter(!!sym(current_species) == 1)
  
  craywatch_species_data <- species_data %>%
    filter(dat.source == "craywatch_data")
  
  # We transformeren de coördinaten (WGS84, 4326) naar het CRS van Vlaanderen.
  occ_craywatch_species <- st_as_sf(
    craywatch_species_data,
    coords = c("longitude", "latitude"),
    crs = st_crs(4326)
  )
  
  # Transformeer de CRS en snij de data op de grenzen van Vlaanderen
  occ_craywatch_species <- st_transform(occ_craywatch_species, st_crs(vlaanderen))
  occ_craywatch_species <- st_intersection(occ_craywatch_species, st_geometry(vlaanderen))
  
  # Koppel de waarnemingen aan de rasterhokken en tel het aantal unieke hokken
  bezette_km_hokken_craywatch_species <- st_intersection(grid, occ_craywatch_species) %>%
    distinct(id) %>% # Tel unieke km-hok ID's
    nrow()
  
  # Filter de data voor de pre-Craywatch periode
  data_pre <- species_data %>%
    filter(date >= start_date_pre & date <= end_date_pre)
  
  # Filter de data voor de post-Craywatch periode
  data_post <- species_data %>%
    filter(date >= start_date_post & date <= end_date_post)
  
  # Converteer de gefilterde data naar een sf-object
  occ_pre <- st_as_sf(data_pre, coords = c("longitude", "latitude"), crs = st_crs(4326))
  occ_post <- st_as_sf(data_post, coords = c("longitude", "latitude"), crs = st_crs(4326))
  
  # Transformeer de CRS naar die van Vlaanderen en snij de data op de grenzen van Vlaanderen
  occ_pre <- st_transform(occ_pre, st_crs(vlaanderen))
  occ_pre <- st_intersection(occ_pre, vlaanderen)
  
  occ_post <- st_transform(occ_post, st_crs(vlaanderen))
  occ_post <- st_intersection(occ_post, vlaanderen)
  
  # Koppel de waarnemingen aan de rasterhokken en tel het aantal unieke hokken
  hokken_pre_craywatch <- st_intersection(grid, occ_pre) %>%
    distinct(id) %>%
    nrow()
  
  hokken_post_craywatch <- st_intersection(grid, occ_post) %>%
    distinct(id) %>%
    nrow()
  
  # Bereken de toename en print de resultaten
  toename_aantal <- hokken_post_craywatch - hokken_pre_craywatch
  toename_procent <- if (hokken_pre_craywatch > 0) {
    ((toename_aantal) / hokken_pre_craywatch) * 100
  } else {
    Inf # Oneindig als er geen hokken waren in de pre-periode
  }
  
  # Voeg de resultaten toe aan het dataframe
  nieuwe_rij <- data.frame(
    soort = current_species,
    hokken_pre = hokken_pre_craywatch,
    hokken_post = hokken_post_craywatch,
    toename_aantal = toename_aantal,
    toename_procent = toename_procent,
    craywatch_hokken = bezette_km_hokken_craywatch_species,
    stringsAsFactors = FALSE
  )
  
  resultaten_df <- bind_rows(resultaten_df, nieuwe_rij)
  
  # Print de resultaten voor de huidige soort
  cat("---------------------------------------------------\n")
  cat("Soort: ", current_species, "\n")
  cat("Aantal bezette hokken pre-Craywatch: ", hokken_pre_craywatch, "\n")
  cat("Aantal bezette hokken post-Craywatch: ", hokken_post_craywatch, "\n")
  cat("--> Aantal bezette km-hokken ENKEL door Craywatch observaties: ", bezette_km_hokken_craywatch_species, "\n")
  cat("Toename in aantal hokken: ", toename_aantal, "\n")
  cat("Procentuele toename: ", round(toename_procent, 2), "%\n")
  
}

# Maak grafiek
# 1. Sorteren
resultaten_df <- resultaten_df %>% 
  mutate(toename_aantal_gesorteerd = toename_aantal) %>%
  arrange(toename_aantal_gesorteerd) %>%
  mutate(soort = factor(soort, levels = soort))

# 2. Bereken de drie stapelcomponenten
plot_data_driedelig <- resultaten_df %>%
  mutate(
    # 1. COMPONENTEN BEREKENEN (De waarden blijven hetzelfde)
    Comp_Pre_Basis = hokken_pre,
    Comp_Craywatch_Top = craywatch_hokken,
    Comp_Toename_Zonder_Craywatch = pmax(0, hokken_post - hokken_pre - Comp_Craywatch_Top)
  ) %>%
  select(soort, Comp_Pre_Basis, Comp_Toename_Zonder_Craywatch, Comp_Craywatch_Top, hokken_post) %>%
  
  # Pivot naar een lange indeling
  tidyr::pivot_longer(
    cols = starts_with("Comp_"),
    names_to = "Component",
    values_to = "Aantal"
  ) %>%
  
  # 3. Maak de Fill_factor en Arcering_Factor
  mutate(
    # *** AANGEPASTE STAPELVOLGORDE (om de omgekeerde rendering te corrigeren) ***
    # Dit dwingt de Craywatch (Rood) visueel naar de top in de omgekeerde rendering.
    Fill_factor = factor(Component,
                         levels = c("Comp_Craywatch_Top",                      
                                    "Comp_Toename_Zonder_Craywatch",
                                    "Comp_Pre_Basis"
                                    ),                 
                         labels = c("Door Craywatch", 
                                    "Na Craywatch", 
                                    "Voor Craywatch" 
                                    )),
    
    # DEFINIEERT DE ARCERING
    Arcering_Factor = ifelse(Component == "Comp_Craywatch_Top", "Craywatch", "Basis")
  )

# Bereken de totale hoogte voor de labels
label_data <- plot_data_driedelig %>%
  group_by(soort) %>%
  summarise(Totaal_Aantal = sum(Aantal), .groups = 'drop')

label_data_procentueel <- resultaten_df %>%
  mutate(
    # Format percentage, rond af op 1 decimaal
    percentage_label = case_when(
      is.infinite(toename_procent) ~ "Nieuw", # Voor soorten die pre-periode 0 waren
      toename_procent >= 0 ~ paste0("+", round(toename_procent, 1), "%"),
      TRUE ~ paste0(round(toename_procent, 1), "%")
    ),
    # De labelpositie blijft de totale hoogte (hokken_post)
    label_y = hokken_post
  )

# ###################### 3. Plot met Arcering (ggpattern) ######################

grafiek_drie_stapel_patroon <- ggplot(plot_data_driedelig, aes(x = soort, y = Aantal)) +
  
  # GEOM_COL_PATTERN voor de Arcering
  geom_col_pattern(
    aes(fill = Fill_factor, 
        pattern = Arcering_Factor),
    pattern_fill = "black",       
    pattern_alpha = 0.5,
    pattern_density = 0.01,
    pattern_spacing = 0.02,
    position = position_stack(), 
    color = "black", linewidth = 0.2,
    show.legend = c(pattern = FALSE, fill = TRUE)
  ) +
  
  # *** AANGEPASTE KLEURVOLGORDE ***
  # De kleuren moeten nu in de volgorde van de LEVELS staan om de juiste kleur aan het juiste deel te geven.
  scale_fill_manual(
    values = c("Voor Craywatch" = "#6BA1D3",           # BLUW (voor Pre-Basis)
               "Na Craywatch" = "#B2DF8A",        # GROEN (voor Toename zonder CW)
               "Door Craywatch" = "#B2DF8A"),       # ROOD/ORANJE (voor Craywatch)
    name = NULL
  ) +
  
  # Arcering instellen (alleen de Craywatch-laag krijgt een streep)
  scale_pattern_manual(
    values = c("Craywatch" = "stripe", 
               "Basis" = "none")
  ) + 
  
  guides(
    fill = guide_legend(
      # We negeren de pattern aesthetic voor de legenda-blokjes, behalve voor de Craywatch-component.
      # Hiervoor maken we een lege patroon-vector.
      override.aes = list(
        pattern = c("stripe", "none", "none") # Alleen het derde element (Craywatch) krijgt de arcering
      )
    )
  ) +
  
  # Voeg de totale waarde toe (bovenop de staaf)
  geom_text(
    data = label_data_procentueel, 
    aes(x = soort, y = label_y, label = percentage_label, fill = NULL), 
    vjust = -0.5, 
    size = 3.5,
    inherit.aes = FALSE
  ) +
  
  labs(
    title = NULL,
    x = NULL,
    y = "Totaal aantal bezette km-hokken"
  ) +
  
  INBOtheme::inbo_donkerblauw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
    legend.position = "bottom"
  )

# Toon de plot
print(grafiek_drie_stapel_patroon)
