# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 

# Preparation of dataset
# Specify covariates
# Date: junio 2025
# ============================================================================

# 1. Load the necessary libraries ---------------------------------------------
library(readxl) # For importing data
library(dplyr) # For data manipulation
library(tidyr) # For data reorganisation

# 2. Import data ----------------------------------------------------------
data <- read_excel("DATABASE_COSTCP.xlsx", sheet = "data_Cost")
View(data)

# 3. Converting categorical variables to factors -----------------------------
variables_a_factor <- c("SEXC", "SEXP", "EDULEVELC", "CPT", "GMFCS", "GMFCS2", 
                        "LevJobCP", "Social_Class", "SCH_SUPP", "VSS", "VFCS", 
                        "MACS", "EDACS", "CFCS", "BFMF", "ETIOL", "MRICS", 
                        "RESID","GOI", "IMP_INDEX")

data <- data %>%
  mutate(across(all_of(variables_a_factor), as.factor))

# 4. Re-categorisation of types of cerebral palsy (CPT) ------------------------------------------------
data <- data %>%
  mutate(CPT_grouped = factor(ifelse(CPT %in% c("Ataxic", "Dyskinetic", "Mixed"), 
                                     "No_Spastic", "Spastic")))

# 5. Scaling numerical variables ----------------------------------------------
vars <- c("AGEP", "AGEC", "HOUSEINCY", "Zarith_Score", 
          "QALYsA", "QALYsC", "EVALYsA", "EVALYsC")

data <- data %>%
  mutate(across(all_of(vars), ~ scale(.)[,1], .names = "z_{.col}"))

View(data)



# 6. Final Verification: Frequency Tables ---------------------------------

frecuencia_total <- data %>%
  dplyr::select(all_of(c(variables_a_factor, "CPT_grouped"))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Categoria") %>%
  count(Variable, Categoria, name = "Frecuencia")

print(frecuencia_total)
print(frecuencia_total, n = Inf)
View(frecuencia_total)


# 7. Save the processed data --------------------------------------------
save(data, file = "costs_data.RData")


