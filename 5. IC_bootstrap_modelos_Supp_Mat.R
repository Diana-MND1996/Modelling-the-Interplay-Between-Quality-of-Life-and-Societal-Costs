# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 
# BOOTSTRAP COST MODELS
# Supplementary Material (TABLE S2-S6)
# Date: June 2025
# ============================================================================

# Install important packages
install.packages("flextable", type = "binary")

# Libraries required
library(boot)
library(dplyr)
library(flextable)
library(officer)

load("costs_data.RData")
View(data)

# Model list
modelos <- list(
  model_Ycare = glm(CosT_Ycare ~ SEXC + GOI + CFCS + ETIOL + MRICS +
                      Zarith_Score + z_QALYsC,
                    family = Gamma(link = "log"), data = data),
  model_SocialCost = glm(GTotCost ~ SEXP + SEXC + GMFCS + ETIOL + MRICS +
                           Zarith_Score + z_AGEP,
                         data = data, family = Gamma(link = "log")),
  model_CostsCP = glm(Tot_CostCP ~ SEXP + GMFCS + GOI + SCH_SUPP + VSS + VFCS +
                        EDACS + CFCS + ETIOL + z_AGEC + z_HOUSEINCY +
                        z_QALYsA + z_QALYsC,
                      data = data, family = Gamma(link = "log")),
  model_CostPayer = glm(Tot_CostGov ~ GMFCS + CIVISTAT + Zarith_Score,
                        data = data, family = Gamma(link = "log")),
  model_MedCare = glm(Tot_MedCare ~ SEXC + SEXP + EDACS + z_QALYsA,
                      data = data, family = Gamma(link = "log")),
  model_CostOutPocket = glm(Tot_OutPocket ~ SEXP + GMFCS + GOI + ETIOL + 
                              z_AGEC + z_HOUSEINCY + z_QALYsC,
                            data = data, family = Gamma(link = "log"))
)

# General function for bootstrapping and extraction of Confidence Intervals (CI)
analizar_modelo <- function(modelo, nombre, data, R = 1000) {
  coef_names <- names(coef(modelo))
  n_coef <- length(coef_names)
  
  boot_fun <- function(data, indices) {
    d <- data[indices, ]
    fit <- try(update(modelo, data = d), silent = TRUE)
    if (inherits(fit, "try-error") || length(coef(fit)) != n_coef) {
      return(rep(NA, n_coef))
    } else {
      return(coef(fit))
    }
  }
  
  set.seed(123)
  boot_results <- boot(data = data, statistic = boot_fun, R = R)
  
  tabla_ic <- lapply(1:n_coef, function(i) {
    ci <- try(boot.ci(boot_results, type = c("perc", "bca"), index = i), silent = TRUE)
    est <- coef(modelo)[i]
    if (inherits(ci, "try-error") || is.null(ci$bca)) {
      data.frame(
        Variable = coef_names[i],
        Estimate = est,
        Perc_low = NA, Perc_high = NA,
        BCa_low = NA, BCa_high = NA,
        Signif = ""
      )
    } else {
      signif_marker <- ifelse(ci$bca[4] > 0 | ci$bca[5] < 0, "*", "")
      data.frame(
        Variable = coef_names[i],
        Estimate = round(est, 4),
        Perc_low = round(ci$percent[4], 4),
        Perc_high = round(ci$percent[5], 4),
        BCa_low = round(ci$bca[4], 4),
        BCa_high = round(ci$bca[5], 4),
        Signif = signif_marker
      )
    }
  }) %>% bind_rows()
  
  tabla_flex <- tabla_ic %>%
    flextable() %>%
    set_caption(paste0("IC Bootstrap 95% - ", nombre)) %>%
    autofit()
  
  return(tabla_flex)
}

# Word document with all tables
doc <- read_docx()

for (nombre in names(modelos)) {
  modelo <- modelos[[nombre]]
  tabla <- analizar_modelo(modelo, nombre, data = data)
  doc <- body_add_par(doc, value = nombre, style = "heading 1")
  doc <- body_add_flextable(doc, tabla)
}

print(doc, target = "IC_bootstrap_models.docx")
