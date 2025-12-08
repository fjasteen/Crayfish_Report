# ====================================================
# Scriptnaam: 21_Spatial_Context_Refined.R
# Auteur: Stien Mertens
# Refactored door: Frédérique Steen
# Datum laatste wijziging: 05-12-2025
# Beschrijving: 

# ====================================================
load(file.path(dir_data_intermediate, "fysicochemie", "fc_data_raw.Rdata")) 
# (Zorg dat dit dataframe 'fc_raw' heet en kolommen 'sample_point' en 'year' heeft)

# 2. Maak de sleutel uit je reeds gekoppelde dataset
# We hebben enkel de ID, het meetpunt en het jaar nodig om de brug te slaan
link_keys <- data_fc_cray_combined %>%
  select(sample_point, yearGroup, VHAG, WVLC) %>%
  distinct() 

# 3. Trek de ruwe data eruit via een INNER JOIN
# Dit doet 3 dingen tegelijk:
#   a. Filtert de ruwe data (gooit jaren/locaties weg die je niet nodig hebt)
#   b. Koppelt de juiste 'ID' aan de meting (cruciaal voor compare_means!)
#   c. Behoudt de exacte datums (nodig voor notAvg)

data_fc_cray_notAvg <- link_keys %>%
  inner_join(fc_breed, 
             by = c("sample_point" = "sample_point", 
                    "yearGroup" = "year")) # Zorg dat de jaarkolom in fc_raw correct heet

# 4. Opslaan voor compare_means.R
write.table(data_fc_cray_notAvg, 
            file = file.path(dir_data_output, "cray_fc_linked_notAvg.txt"), 
            sep = "\t", row.names = FALSE)
# Pas dit pad aan naar waar jouw "cleane" maar "niet-geaggregeerde" metingen staan
# In het oude script heette dit 'fc_data_cleaner.rdata' of 'fc_breed_sub'
file_fc_raw <- file.path(dir_data_intermediate, "fysicochemie", "fc_data_raw.Rdata") # CHECK DIT PAD!

if (file.exists(file_fc_raw)) {
  load(file_fc_raw) # Zorg dat dit object 'fc_raw' heet of hernoem het hieronder
  
  # Zorg voor projectie en tijdsformat
  fc_raw_prep <- fc_raw %>% # Vervang 'fc_raw' door de naam van het geladen object (bv. fc_data_clean)
    st_as_sf(coords = c("lambert_x", "lambert_y"), crs = 31370, remove = FALSE) %>%
    mutate(year = as.numeric(format(sample_datum_monstername, "%Y")),
           maand = as.numeric(format(sample_datum_monstername, "%m"))) %>%
    dplyr::rename(geom_fc = geometry)
  
  # 2. Koppeling WVLC (Gesloten wateren) met RUWE data
  data_raw_wvlc <- data %>%
    dplyr::filter(!is.na(WVLC)) %>%
    inner_join(fc_raw_prep %>% as.data.frame() %>% dplyr::filter(!is.na(WVLC)), 
               by = c("WVLC" = "WVLC", "yearGroup" = "year"), 
               suffix = c("", "_fc")) %>%
    select(-geom_fc)
  
  # 3. Koppeling VHAG (Open wateren) met RUWE data
  # We hergebruiken de logica van de 'find_nearest_vhag' functie, maar nu op de ruwe set
  # Let op: dit kan traag zijn omdat de ruwe set veel groter is!
  
  # Filter eerst de ruwe set op relevante jaren en VHAGs om het te versnellen
  relevant_years <- unique(data$yearGroup)
  relevant_vhags <- unique(c(data$VHAG, unlist(data$VHAG2)))
  
  fc_raw_subset <- fc_raw_prep %>%
    dplyr::filter(year %in% relevant_years,
                  VHAG %in% relevant_vhags)
  
  # Pas de zoekfunctie toe (deze functie stond al in Script B)
  results_vhag_raw <- map(1:nrow(data_vhag_subset), 
                          ~find_nearest_vhag(.x, data_vhag_subset, fc_raw_subset))
  
  # Verwerk resultaten
  data_raw_vhag_linked <- data_vhag_subset %>%
    mutate(
      sample_point = map_chr(results_vhag_raw, "sample_point"),
      distance_cray_FC = map_dbl(results_vhag_raw, "distance")
    ) %>%
    dplyr::filter(!is.na(sample_point) & distance_cray_FC < 1000)
  
  # Join de daadwerkelijke metingen erbij
  data_raw_vhag <- data_raw_vhag_linked %>%
    left_join(st_drop_geometry(fc_raw_prep), 
              by = c("sample_point", "yearGroup" = "year", "VHAG"),
              suffix = c("", "_fc"))
  
  # 4. Samenvoegen en opslaan
  data_fc_cray_notAvg <- bind_rows(data_raw_wvlc, data_raw_vhag) %>%
    st_drop_geometry() %>%
    select(-VHAG2)
  
  # Opslaan voor gebruik in compare_means.R
  write.table(data_fc_cray_notAvg, 
              file = file.path(dir_data_output, "cray_fc_linked_notAvg.txt"), # Dit is je 'input' voor compare_means
              sep = "\t", row.names = FALSE)
  
  message("Ruwe dataset 'cray_fc_linked_notAvg.txt' is aangemaakt.")
  
} else {
  warning("Kon ruwe FC data niet vinden. De _notAvg dataset is niet aangemaakt.")
}