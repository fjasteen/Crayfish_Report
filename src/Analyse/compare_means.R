library(corrplot)
library(dplyr)

# Lees data in 
FC_data <- read.table("~/GitHub/Craywatch-Rapport/R/data/output/data_fc_cray_increase.txt", header = TRUE, sep = "", quote = "\"")
FC_data_not_average <- read.table("~/GitHub/Craywatch-Rapport/R/data/output/data_fc_cray_notAvg_increase.txt", header = TRUE, sep = "\t",  quote = "\"", row.names = NULL)

# Zorg ervoor dat de kolom jaar correct is geformatteerd
FC_data_not_average <- FC_data_not_average %>%
  mutate(sample_point_jaar = as.factor(format(as.Date(sample_datum_monstername, format = "%Y-%m-%d"), "%Y")),
         sample_point_maand = as.numeric(format(as.Date(sample_datum_monstername, format = "%Y-%m-%d"), "%m")))

FC_zomer_data <- FC_data_not_average %>%
  filter(sample_point_maand <= 10 & sample_point_maand >= 5)

# 1. Bereken het Zuivere Eénjarige Gemiddelde (T_X) uit de ruwe data
# (Herhaal de berekening, maar zorg dat sample_point_jaar in FC_een_jaar_gem een factor is)
FC_een_jaar_gem <- FC_zomer_data %>%
  group_by(ID, sample_point_jaar) %>%
  summarise(ZS_een_jaar = mean(ZS, na.rm = TRUE), .groups = 'drop')

# 2. Koppel de 1-jarige en 3-jarige waarden op de unieke waarnemingscluster
FC_data_vergelijk <- FC_data %>%
  rename(ZS_drie_jaar = ZS) %>%
  left_join(
    FC_een_jaar_gem %>%
      # CONVERSIE FIX: Converteer de factor naar numeriek/integer
      mutate(sample_point_jaar = as.integer(as.character(sample_point_jaar))), 
    by = c("ID", "year" = "sample_point_jaar")
  )

# 3. Voer de Gepaarde T-test uit
FC_data_compleet <- FC_data_vergelijk %>%
  filter(!is.na(ZS_een_jaar) & !is.na(ZS_drie_jaar))

t_resultaat_gepaard <- t.test(FC_data_compleet$ZS_een_jaar, 
                              FC_data_compleet$ZS_drie_jaar, 
                              paired = TRUE)

print("Resultaten van de Gepaarde T-test (ZS_een_jaar vs. ZS_drie_jaar):")
print(t_resultaat_gepaard)

# Voer alleen uit nadat de FC_data_compleet is gemaakt
FC_data_compleet <- FC_data_compleet %>%
  mutate(Afwijking = ZS_een_jaar - ZS_drie_jaar) %>%
  arrange(desc(abs(Afwijking)))

head(FC_data_compleet %>% select(ID, year, ZS_een_jaar, ZS_drie_jaar, Afwijking), 10)