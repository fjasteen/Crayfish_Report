# R/src/analysis_fysicochemie/00_config_fc.R

# 1. Laad de algemene project config (voor paden en packages)
source(here::here("./src/config.R")) 

# 2. Specifieke packages voor deze analyse
required_packages <- c(
"missMDA", # Voor PCA imputatie
"vegan",   # Voor PCA
"TOSTER"  # Voor equivalentie toets
)

invisible(lapply(required_packages, library, character.only = TRUE))


# 3. Lokale Paden (Binnen de output map een submap maken voor deze run)
dir_fc_output <- here(dir_data_output, "fysicochemie")
dir_fc_inter  <- here(dir_data_intermediate, "fysicochemie")
if(!dir.exists(dir_fc_output)) dir.create(dir_fc_output, recursive = TRUE)
if(!dir.exists(dir_fc_inter)) dir.create(dir_fc_inter, recursive = TRUE)

# 4. Analyse Parameters (AANPASBAAR)
# ---------------------------------------------------------
# [FS1] GBIF Filter: Hoe zeker moet de locatie zijn?
FC_PARAM_MAX_UNCERTAINTY <- 100 

# Ruimtelijke koppeling
FC_PARAM_BUFFER_DIST     <- 10   # meter (standaard)
FC_PARAM_RIVER_WIDTH_BUF <- 10   # meter (extra buffer bovenop halve breedte)

# Temporele koppeling
FC_PARAM_YEAR_WINDOW     <- 3    # Huidig jaar + 2 voorgaande
FC_PARAM_MONTH_START     <- 5    # Mei
FC_PARAM_MONTH_END       <- 10   # Oktober

# PCA
FC_PARAM_PCA_VARS <- c("Cl.", "N.t", "O2", "EC.20", "T", "pH", "Secchi", "P.t", "ZS")