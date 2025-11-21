# ====================================================
# Scriptnaam: get_gbif_occ.R
# Auteur: Margot Vermeylen
# Datum: 09-09-2025
# Beschrijving: 
# Dit script haalt craywatch occurrences van gbif (vanaf 2000) en filtert de gbif data eruit die we willen gebruiken
# ====================================================

library(rgbif)
library(dplyr)
library(sf)
library(readr)
library(glue)

# 1. Soorten en Taxonkeys ophalen
species <- c("Procambarus clarkii",
             "Procambarus virginalis",
             "Procambarus acutus",
             "Faxonius limosus",
             "Pacifastacus leniusculus",
             "Faxonius virilis",
             "Faxonius immunis",
             "Faxonius juvenilis",
             "Faxonius rusticus",
             "Pontastacus leptodactylus")


taxonkeys <-species %>% name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys

# 2. GBIF-data downloaden
gbif_user <- Sys.getenv("gbif_user")
gbif_pwd <- Sys.getenv("gbif_pwd")
gbif_email <- Sys.getenv("gbif_email")

set <-occ_download(
  pred_in("taxonKey", taxonkeys),
  pred_in("country", c("BE")),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2000),
  pred("occurrenceStatus", "PRESENT"),
  user=gbif_user,pwd=gbif_pwd,email=gbif_email,
  curlopts=list(http_version=2)
)

repeat{
  Sys.sleep(time = 5*length(taxonkeys))
  test_set <- occ_download_meta(set)
  if(test_set$status == "SUCCEEDED"){
    rawdata_set_imported <- occ_download_get(set,
                                             path = "~/GitHub/Craywatch-Rapport/R/data/intermediate/",
                                             overwrite = TRUE,
                                             curlopts=list(http_version=2),
                                             return = NULL,
                                             verbatim = NULL) %>% 
      occ_download_import()
    break
  }
  print(test_set$status)
}

unzip(paste0("~/GitHub/Craywatch-Rapport/R/data/intermediate/",test_set$key,".zip"), 
      exdir= paste0("~/GitHub/Craywatch-Rapport/R/data/intermediate/", test_set$key))
path=paste0("~/GitHub/Craywatch-Rapport/R/data/intermediate/", test_set$key,"/occurrence.txt")

CF_occ<-read.delim(path, header=TRUE)

years<-unique(CF_occ$year)
species_names<-unique(CF_occ$species)


occ_spatial <- st_as_sf(CF_occ, coords = c("decimalLongitude", "decimalLatitude"),
                        crs = "+proj=longlat +datum=WGS84")

occ_spatial <- occ_spatial %>%
  mutate(decimalLongitude = sf::st_coordinates(.)[,1],
         decimalLatitude = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() 


write_csv(occ_spatial, "~/GitHub/Craywatch-Rapport/R/data/input/gbif/gbif_occ_CF.csv")