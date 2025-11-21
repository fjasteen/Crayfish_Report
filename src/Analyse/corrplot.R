# Laad het pakket
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)


FC_data <- read.table("~/GitHub/Craywatch-Rapport/R/data/output/data_fc_cray_rivierbreedte.txt", header = TRUE, sep = "", quote = "\"")

# duid de nodige kolommen aan
cor_kolommen <- c("faxonius.limosus", "procambarus.clarkii", "Cl.", "N.t", "O2", "EC.20", "T", "pH", "Secchi", "P.t", "ZS")

cor_data <- FC_data[, cor_kolommen]

# Definiëer een realistisch maximum (bijv. 1000)
MAX_O2_REALISTISCH <- 1000 

# Vervang alle waarden die boven deze drempel liggen door NA
cor_data$O2[cor_data$O2 > MAX_O2_REALISTISCH] <- NA

# Controleer de summary opnieuw: de Max. en Mean moeten nu correct zijn.
print(summary(cor_data$O2))
print(summary(cor_data$Cl.))
print(summary(cor_data$N.t))
print(summary(cor_data$EC.20))
print(summary(cor_data$T))
print(summary(cor_data$pH))
print(summary(cor_data$Secchi))
print(summary(cor_data$P.t))
print(summary(cor_data$ZS))

# Correlatiematrix
cor_matrix <- cor(
  cor_data,
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Functie om de p-waarden matrix te maken (identiek aan de cor.mtest, maar zonder de list/extra checks die faalden)
get_p_matrix <- function(mat, method, use) {
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > j) {
        # Bereken cor.test alleen voor unieke paren
        result <- cor.test(mat[, i], mat[, j], method = method, use = use)
        p.mat[i, j] <- p.mat[j, i] <- result$p.value
      }
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# Bereken de p-waarden matrix
p_matrix <- get_p_matrix(
  cor_data, 
  method = "spearman",
  use = "pairwise.complete.obs"
)

# Optioneel: Creëer een NIEUWE correlatiematrix waar insignificante waarden NA zijn
cor_matrix_significant <- cor_matrix
cor_matrix_significant[p_matrix > 0.05] <- NA 

# Creëer de uiteindelijke plot
corrplot(
  cor_matrix_significant, # Gebruik de gemaskerde matrix met NA's
  method = "circle",
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45,
  
  
  # # Optioneel: voeg de coëfficiënten toe
  # addCoef.col = "grey30",
  # number.cex = 0.6
)


# Creëer de scatterplot
ggplot(cor_data, aes(x = cor_data$Cl., y = faxonius.limosus)) +
  # Gebruik geom_point om de datapunten te plotten
  geom_point(position = position_jitter(width = 0.05, height = 0.05), # Voeg jitter toe om overlapping te vermijden
             alpha = 0.6, color = "darkblue") +
  
  # Voeg een trendlijn toe
  # 'method = "lm"' voor een lineaire lijn (Pearson)
  # 'method = "loess"' voor een gladde curve (vaak beter voor ecologische data)
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  
  # Labels en titel
  labs(title = "Relatie tussen CPUE van P. clarkii en Chloride (Cl.)",
       x = "Chloride (Cl.)",
       y = "Faxonius limosus") 
  

# Maak boxplots voor aan- of afwezigheid
data_long_species <- cor_data %>%
  pivot_longer(
    cols = c(faxonius.limosus, procambarus.clarkii), 
    names_to = "Soort", 
    values_to = "Aanwezigheid" 
  ) %>%
  
  # Filter rijen waar de 'Aanwezigheid' (0/1) NA is (als de soort onbekend is)
  filter(!is.na(Aanwezigheid)) %>%
  # 
  # Maak de 'Aanwezigheid' variabele een factor voor de plot
  mutate(Aanwezigheid = as.factor(Aanwezigheid))

print(head(data_long_species)) # Bekijk hoe de data er nu uitziet

# Maak een aangepaste labeller met cursieve namen
species_labels_italic <- c(
  "faxonius.limosus" = "italic(F.~limosus)",  # Gebruik '~' voor een spatie in cursieve tekst
  "procambarus.clarkii" = "italic(P.~clarkii)"
)

# definieer de FC-parameters
fc_parameters <- c("Cl.", "N.t", "O2", "EC.20", "T", "pH", "Secchi", "P.t", "ZS") 

# Loop over de FC-parameters
for (param in fc_parameters) {
  
  # Maak de plot
  p <- ggplot(data_long_species, aes(x = Aanwezigheid, y = !!sym(param))) +
    
    # Gebruik 'Soort' om de boxplots te groeperen/facetten
    geom_boxplot(aes(fill = Aanwezigheid), alpha = 0.7) + 
    
    # splits de plot in kolommen voor elke soort
    facet_wrap(~ Soort, 
               labeller = labeller(Soort = as_labeller(species_labels_italic, 
                                                       default = label_parsed))
    ) +
    
    # Custom labels en titels
    scale_x_discrete(labels = c("0" = "Afwezig", "1" = "Aanwezig")) +
    scale_fill_manual(values = c("0" = "lightblue", "1" = "darkmagenta")) +
    labs(
      y = param,
      x = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      # Maak de tekst van de FACET-labels (de soortnamen boven de plots) groter
      strip.text = element_text(size = 20), 
      
      # Maak de tekst op de assen (bijv. "Afwezig", "Aanwezig" op de x-as) groter
      axis.text = element_text(size = 18), 
      
      # Maak de TITEL van de assen (bijv. de 'y = param' tekst) groter
      axis.title = element_text(size = 18))
  
  # Toon de plot
  print(p)
}

ggplot(cor_data_filterd_FL, aes(x = as.factor(faxonius.limosus), y = ZS)) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "red", "1" = "lightblue")) + 
  labs(title = "Zwevende stoffen bij aan- of afwezigheid van F. limosus",
       x = "F. limosus Aanwezigheid (0 = Afwezig, 1 = Aanwezig)",
       y = "zwevende stoffen (mg/l)") +
  theme_minimal()