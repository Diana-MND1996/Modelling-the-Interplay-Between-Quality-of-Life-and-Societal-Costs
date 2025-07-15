# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 
# DESCRIPTIVE RESULTS
# Supplementary Material (Table S1)
# Date: June 2025
# ============================================================================
####Table article Modelling
# Install packages
install.packages("gtsummary")
install.packages("dplyr")
install.packages("flextable")
install.packages("officer")

# Load the necessary libraries
library(dplyr)
library(gtsummary)
library(flextable)
library(officer)

load("costs_data.RData")
View(data)

# 1. Create group variable
data <- data %>%
  mutate(cost_group = ifelse(Tot_CostTTCPAL == 0, "With Out-of-pocket
Healthcare Cost", "Without Out-of-pocket Healthcare cost"))

# 2. Create summary table
tabla_resumen <- data %>%
  select(cost_group, AGEC, AGEP, SEXC, SEXP, CIVISTAT, PROFACT, EDULEVELC,
         GOI, LevJobCP, HOUSEINCM, HOUSEINCY, Social_Class, SCH_SUPP, SCH_GRD,
         CUME, SUBVECP, GMFCS, VSS, VFCS, MACS, EDACS, CFCS, BFMF, CPT, ETIOL,
         IMP_INDEX, MRICS, RESID) %>%
  tbl_summary(by = cost_group,           # group by cost
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing = "no") %>%
  add_overall() %>%                      # add totals column
  add_p()                                # add p-value to compare groups

# 3. Show table
tabla_resumen
1

# Convert table to flextable format
tabla_word <- as_flex_table(tabla_resumen)

# Create Word document and save
doc <- read_docx() %>%
  body_add_flextable(tabla_word)

print(doc, target = "Costs_table.docx")




























