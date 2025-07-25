# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 
# VARIABLE SELECTION AND MODEL FITTING WITH GLMULTI
# Date: June 2025
# ============================================================================
# ========================
# DATA AND LIBRARY LOADING
# ========================

load("costs_data.RData")

library(glmulti)
library(dplyr)
library(stringr)
library(future)
library(future.apply)

# ========================
# PARALLELIZATION SETUP
# ========================

plan(multisession, workers = availableCores() - 2)

# ========================
# GENERAL COST MODELS (GLMULTI IN PARALLEL)
# ========================

# Dependent variables
dependent_vars <- c("GTotCost", "Tot_CostCP", "CosT_Ycare", "Tot_CostGov", "Tot_MedCare", "Tot_OutPocket")

# Function to fit model with glmulti
run_model <- function(dep_var) {
  formula <- as.formula(paste(dep_var, "~ z_AGEC + SEXC + z_AGEP + SEXP + EDULEVELC +
                               z_HOUSEINCY + CIVISTAT + CPT_grouped + GMFCS + GOI +
                               LevJobCP + Zarith_Score + Social_Class + z_QALYsA +
                               SCH_SUPP + VSS + VFCS + MACS + EDACS + CFCS + z_QALYsC +
                               BFMF + ETIOL + MRICS + RESID + IMP_INDEX"))
  
  glmulti(formula, data = data, family = Gamma(link = "log"),
          level = 1, method = "h", crit = "aic",
          confsetsize = 10, plotty = FALSE, report = TRUE)
  
  # Add attribute with dependent variable name
  attr(model_result, "dep_var") <- dep_var
  
  return(model_result)
}

# Parallel model fitting
models <- future_lapply(dependent_vars, run_model, future.seed = TRUE)

# Save fitted models
save(models, file = "modelos_optimo_aic.RData")

# ========================
# DIFFERENT COST MODEL (Tot_CostTTCPAL)
# ========================

# Part 1: Logistic regression (presence vs absence of cost)
CostTH <- data %>%
  mutate(cost_positive = ifelse(Tot_CostTTCPAL > 0, 1, 0))

best_part1 <- glmulti(
  cost_positive ~ z_AGEP + SEXP + EDULEVELC + z_QALYsC + z_QALYsA +
    z_HOUSEINCY + CIVISTAT + CPT_grouped + GMFCS + GOI +
    LevJobCP + Zarith_Score + Social_Class + SCH_SUPP + VSS +
    VFCS + MACS + EDACS + CFCS + BFMF + ETIOL + MRICS + RESID + IMP_INDEX,
  data = CostTH,
  level = 1, method = "h", crit = "aic",
  family = binomial
)

save(list = "best_part1", file = "best_part1.RData")

# ========================
# Part 2: GLMULTI with different GLM families
# ========================

# Filter data with positive cost
positive_data <- data %>% filter(Tot_CostTTCPAL > 0)

# List of families to test (excluding Poisson)
test_list <- list(
  gau_id = gaussian("identity"),
  gau_log = gaussian("log"),
  gam_id = Gamma("identity"),
  gam_log = Gamma("log")
)

# Base formula for glmulti
glmulti_formula <- Tot_CostTTCPAL ~ z_AGEP + SEXP + EDULEVELC + z_QALYsC + z_QALYsA +
  z_HOUSEINCY + CIVISTAT + CPT_grouped + GMFCS + LevJobCP +
  Zarith_Score + Social_Class + SCH_SUPP + VSS +
  VFCS + MACS + EDACS + CFCS + BFMF + ETIOL + MRICS +
  RESID + IMP_INDEX

# Create output folder if it doesn't exist
output_path <- "modelos_glmulti"
dir.create(output_path, showWarnings = FALSE)

# Fit glmulti models for each family
for (test_name in names(test_list)) {
  cat("Processing:", test_name, "\n")
  
  data_use <- if (grepl("gam", test_name)) positive_data else data
  
  glmulti_result <- glmulti(
    glmulti_formula,
    data = data_use,
    level = 1,
    method = "h",
    crit = "aic",
    family = test_list[[test_name]],
    confsetsize = 10
  )
  
  save(glmulti_result, file = file.path(output_path, str_c("glmulti_", test_name, ".RData")))
  cat("Model saved:", test_name, "\n\n")
}
