# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 
# Quality of Life (QoL) Model Variables Selection, Final QoL Models and Cross Validation
# Date: June 2025
# ============================================================================

# Load required libraries
library(caret)
library(VGAM)
library(Metrics)  # For RMSE, MAE, ME
library(dplyr)
library(MASS)



load("costs_data.RData")
View(data)

#================================================================================================
#COMPLETE MODELS AND INTERPRETATION
#================================================================================================
#QUALITY OF LIFE CHILDREN
#================================================================================================
#OLS MODEL
#================================================================================================

# Selection of variables for OLS
lm_qalys <- stepAIC(lm(QALYsC ~ AGEC + SEXC + AGEP + SEXP + 
                         EDULEVELC + HOUSEINCY + CIVISTAT +
                         CPT_grouped + GMFCS + LevJobCP +
                         poly(Zarith_Score,3) + Social_Class + 
                         SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                         EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL +
                         MRICS + RESID + IMP_INDEX, data = data,
                       direction = "both"))

summary(lm_qalys)

#================================================================================================
#TOBIT MODEL
#================================================================================================
library(VGAM)
tobit_qalys <- vglm(QALYsC ~ AGEC + SEXC + AGEP + SEXP + 
                      EDULEVELC + HOUSEINCY + CIVISTAT +
                      CPT_grouped + GMFCS + LevJobCP +
                      poly(Zarith_Score,3) + Social_Class + 
                      SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                      EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL +
                      MRICS + RESID + IMP_INDEX, 
                    tobit(Lower=-Inf, Upper=1), 
                    data = data)

#selection of variables tobit model
summary(tobit_qalys)

tobit_qalys <- step4vglm(tobit_qalys)
summary(tobit_qalys)

#================================================================================================
#QUALITY OF LIFE CAREGIVERS
#================================================================================================


#================================================================================================
#OLS MODELS
#================================================================================================
# Selection of variables for OLS WITH STEP
lm_qalys1 <- stepAIC(lm(QALYsA ~ AGEC + SEXC + AGEP + SEXP + 
                          EDULEVELC + HOUSEINCY + CIVISTAT +
                          CPT_grouped + GMFCS + LevJobCP +
                          poly(Zarith_Score,3) + Social_Class + 
                          SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                          EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL + CosT_Ycare +
                          MRICS + RESID + IMP_INDEX, data = data,
                        direction = "both"))

summary(lm_qalys1)

#================================================================================================
#TOBIT MODEL
#================================================================================================
library(VGAM)
tobit_qalys1 <- vglm(QALYsA ~ AGEC + SEXC + AGEP + SEXP + 
                       EDULEVELC + HOUSEINCY + CIVISTAT +
                       CPT_grouped + GMFCS + LevJobCP +
                       poly(Zarith_Score,3) + Social_Class + 
                       SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                       EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL +
                       MRICS + RESID + IMP_INDEX, 
                     tobit(Lower=-Inf, Upper=1), 
                     data = data)

#selection of variables tobit model
summary(tobit_qalys1)

tobit_qalys1 <- step4vglm(tobit_qalys1)
summary(tobit_qalys1)

#================================================================================================
#GLM Adult MODEL
#================================================================================================
#STEP GLM
library(MASS)  # Load library for stepAIC

# Initial model with all variables
glm_full <- glm(QALYsA ~ AGEC + SEXC + AGEP + SEXP + 
                  EDULEVELC + HOUSEINCY + CIVISTAT +
                  CPT_grouped + GMFCS + LevJobCP +
                  poly(Zarith_Score,3) + Social_Class + 
                  SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                  EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL +
                  MRICS + RESID + IMP_INDEX, 
                family = gaussian(link = "logit"), 
                data = data)

# Apply stepwise variable selection (bidirectional)
glm_qalys <- stepAIC(glm_full, direction = "both")

# Summary of the final model
summary(glm_qalys)

#================================================================================================
#ZARITH SCALE
#================================================================================================
#OVERLOAD MODELS
#================================================================================================
#OLS MODELS
#================================================================================================
# Variable selection for lm WITH STEP
OLS_Zarith <- stepAIC(lm(Zarith_Score ~ AGEC + SEXC + AGEP + SEXP + 
                          EDULEVELC + HOUSEINCY + CIVISTAT +
                          CPT_grouped + GMFCS + LevJobCP +
                          QALYsA + Social_Class + QALYsC +
                          SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                          EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL + CosT_Ycare +
                          MRICS + RESID + IMP_INDEX, data = data,
                        direction = "both"))

summary(OLS_Zarith)

#================================================================================================
#TOBIT MODEL CAREGIVERS
#================================================================================================
library(VGAM)
tobit_zarith <- vglm(Zarith_Score ~ AGEC + SEXC + AGEP + SEXP + 
                       EDULEVELC + HOUSEINCY + CIVISTAT +
                       CPT_grouped + GMFCS + LevJobCP +
                       QALYsA + Social_Class + QALYsC +
                       SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                       EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL + CosT_Ycare +
                       MRICS + RESID + IMP_INDEX, 
                     tobit(Lower=-0, Upper=88), 
                     data = data)

#selection of variables tobit model
summary(tobit_zarith)

tobit_zarith <- step4vglm(tobit_zarith)
summary(tobit_zarith)

#================================================================================================
#GLM MODEL
#================================================================================================
#STEP GLM
library(MASS)  # Load library for stepAIC

# Initial model with all variables
glm_fullzarith <- glm(QALYsA ~ AGEC + SEXC + AGEP + SEXP + 
                        EDULEVELC + HOUSEINCY + CIVISTAT +
                        CPT_grouped + GMFCS + LevJobCP +
                        QALYsA + Social_Class + QALYsC +
                        SCH_SUPP + VSS + VFCS + MACS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                        EDACS + CFCS + BFMF + ETIOL + Tot_CostTTCPAL + CosT_Ycare +
                        MRICS + RESID + IMP_INDEX, 
                      family = gaussian(link = "identity"), 
                      data = data)

# Apply stepwise variable selection (bidirectional)
glm_zarith <- stepAIC(glm_fullzarith, direction = "both")

# Summary of the final model
summary(glm_zarith)

################################################################################################
#CROSS-VALIDATION K-FOLDS CHILDREN
################################################################################################

# Define number of folds
set.seed(123)  # For reproducibility
k_folds <- 10
folds <- createFolds(data$QALYsC, k = k_folds, list = TRUE, returnTrain = TRUE)

# Initialise vectors to store errors
errors_lm <- data.frame(RMSE = numeric(k_folds), MAE = numeric(k_folds), ME = numeric(k_folds))
errors_tobit <- data.frame(RMSE = numeric(k_folds), MAE = numeric(k_folds), ME = numeric(k_folds))

# Cross-validation for both models
for (i in 1:k_folds) {
  
  # Split data into train and test
  train_data <- data[folds[[i]], ]
  test_data <- data[-folds[[i]], ]
  #================================================================================================
  # ---- LINEAR MODEL (OLS) ----
  #================================================================================================
  lm_model <- lm(QALYsC ~ AGEP + SEXP + 
                   HOUSEINCY +
                   CPT_grouped + GMFCS + LevJobCP +
                   VSS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                   CFCS + ETIOL + Tot_CostTTCPAL, data = train_data)
  
  # Predictions
  pred_lm <- predict(lm_model, newdata = test_data)
  
  # Calculation of metrics
  errors_lm$RMSE[i] <- rmse(test_data$QALYsC, pred_lm)
  errors_lm$MAE[i] <- mae(test_data$QALYsC, pred_lm)
  errors_lm$ME[i] <- mean(test_data$QALYsC - pred_lm)  # Mean error
  
  
  #================================================================================================
  # ---- MODEL TOBIT ----
  #================================================================================================
  tobit_model <- vglm(QALYsC ~ AGEP + SEXP + 
                        HOUSEINCY + CIVISTAT +
                        CPT_grouped + GMFCS + LevJobCP +
                        VSS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                        CFCS + ETIOL + Tot_CostTTCPAL,
                      tobit(Lower = -Inf, Upper = 1), 
                      data = train_data)
  # Predictions
  pred_tobit <- predict(tobit_model, newdata = test_data, type = "response")
  
  # Calculation of metrics
  errors_tobit$RMSE[i] <- rmse(test_data$QALYsC, pred_tobit)
  errors_tobit$MAE[i] <- mae(test_data$QALYsC, pred_tobit)
  errors_tobit$ME[i] <- mean(test_data$QALYsC - pred_tobit)
}
#================================================================================================
#---- FINAL RESULTS ----
#================================================================================================
cat("\n=== Cross Validation Results ===\n")
cat("\n--- OLS MODEL ---\n")
cat("RMSE Average:", mean(errors_lm$RMSE), "\n")
cat("MAE Average:", mean(errors_lm$MAE), "\n")
cat("ME Average:", mean(errors_lm$ME), "\n")

cat("\n--- TOBIT MODEL ---\n")
cat("RMSE Average:", mean(errors_tobit$RMSE), "\n")
cat("MAE Average:", mean(errors_tobit$MAE), "\n")
cat("ME Average:", mean(errors_tobit$ME), "\n")

#================================================================================================
# CROSS-VALIDATION k folds for adults HRQoL
#================================================================================================
# Required libraries
library(caret)   # To create folds
library(Metrics) # For RMSE, MAE, ME
library(VGAM)    # For Tobit
library(MASS)    # For Stepwise in GLM and OLS

# Number of folds
k <- 10
set.seed(123)  # For reproducibility
folds <- createFolds(data$QALYsA, k = k, list = TRUE)

# Initialize error storage
errors_lm <- data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric())
errors_tobit <- data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric())
errors_glm <- data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric())

# Cross-validation
for (i in 1:k) {
  # Create training and test sets
  test_idx <- folds[[i]]
  train_data <- data[-test_idx, ]
  test_data <- data[test_idx, ]
 
  #================================================================================================
  ####### OLS MODEL ########
  #================================================================================================
  lm_model <- lm(QALYsA ~ AGEP + SEXP + 
                   EDULEVELC + HOUSEINCY + CIVISTAT +
                   GMFCS + poly(Zarith_Score,3) +  
                   SCH_SUPP + Tot_MedCare +
                   BFMF + RESID, data = train_data)
  
  lm_preds <- predict(lm_model, newdata = test_data)
  
  errors_lm <- rbind(errors_lm, data.frame(
    RMSE = rmse(test_data$QALYsA, lm_preds),
    MAE = mae(test_data$QALYsA, lm_preds),
    ME = mean(test_data$QALYsA - lm_preds)
  ))
  #================================================================================================
  ####### TOBIT MODEL ########
  #================================================================================================
  tobit_model <- vglm(QALYsA ~ AGEP + SEXP + 
                        EDULEVELC + HOUSEINCY + CIVISTAT +
                        GMFCS + poly(Zarith_Score,3) +  
                        SCH_SUPP + Tot_MedCare +
                        BFMF + RESID, tobit(Lower = -Inf, Upper = 1), 
                      data = train_data)
  
  tobit_preds <- predict(tobit_model, newdata = test_data, type = "response")
  
  errors_tobit <- rbind(errors_tobit, data.frame(
    RMSE = rmse(test_data$QALYsA, tobit_preds),
    MAE = mae(test_data$QALYsA, tobit_preds),
    ME = mean(test_data$QALYsA - tobit_preds)
  ))
  
  #================================================================================================
  ####### GLM MODEL ########
  #================================================================================================
  glm_model <- glm(QALYsA ~ AGEP + SEXP + 
                     EDULEVELC + HOUSEINCY + CIVISTAT +
                     CPT_grouped + GMFCS + LevJobCP +
                     poly(Zarith_Score,3) + SCH_SUPP + VFCS + Tot_CostGov + Tot_MedCare + Tot_OutPocket +
                     EDACS + BFMF + ETIOL + Tot_CostTTCPAL +
                     MRICS + RESID, data = train_data, 
                   family = gaussian(link = "logit"))
  
  glm_preds <- predict(glm_model, newdata = test_data, type = "response")
  
  errors_glm <- rbind(errors_glm, data.frame(
    RMSE = rmse(test_data$QALYsA, glm_preds),
    MAE = mae(test_data$QALYsA, glm_preds),
    ME = mean(test_data$QALYsA - glm_preds)
  ))
}

#================================================================================================
#RESULTS
#================================================================================================
# Average errors
cat("\n--- OLS MODEL ---\n")
cat("Average RMSE:", mean(errors_lm$RMSE), "\n")
cat("Average MAE:", mean(errors_lm$MAE), "\n")
cat("Average ME:", mean(errors_lm$ME), "\n")

cat("\n--- TOBIT MODEL ---\n")
cat("Average RMSE:", mean(errors_tobit$RMSE), "\n")
cat("Average MAE:", mean(errors_tobit$MAE), "\n")
cat("Average ME:", mean(errors_tobit$ME), "\n")

cat("\n--- GLM MODEL ---\n")
cat("Average RMSE:", mean(errors_glm$RMSE), "\n")
cat("Average MAE:", mean(errors_glm$MAE), "\n")
cat("Average ME:", mean(errors_glm$ME), "\n")

#================================================================================================
# CROSS-VALIDATION CAREGIVER BURDEN (ADULTS)
#================================================================================================

# Required libraries
library(caret)   # To create folds
library(Metrics) # For RMSE, MAE, ME
library(VGAM)    # For Tobit
library(MASS)    # For Stepwise in GLM and OLS

# Number of folds
k <- 10
set.seed(123)  # For reproducibility
folds <- createFolds(data$Zarith_Score, k = k, list = TRUE)

# Initialize error storage
errors_lm <- data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric())
errors_tobit <- data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric())
errors_glm <- data.frame(RMSE = numeric(), MAE = numeric(), ME = numeric())

# Cross-validation
for (i in 1:k) {
  # Create training and test sets
  test_idx <- folds[[i]]
  train_data <- data[-test_idx, ]
  test_data <- data[test_idx, ]
  
  #================================================================================================
  ####### OLS MODEL ########
  #================================================================================================
  lm_model <- lm(Zarith_Score ~ AGEP + SEXP + 
                   GMFCS + LevJobCP +
                   QALYsA + Social_Class + 
                   Tot_CostGov + CosT_Ycare +
                   RESID, data = train_data)
  
  lm_preds <- predict(lm_model, newdata = test_data)
  
  errors_lm <- rbind(errors_lm, data.frame(
    RMSE = rmse(test_data$Zarith_Score, lm_preds),
    MAE = mae(test_data$Zarith_Score, lm_preds),
    ME = mean(test_data$Zarith_Score - lm_preds)
  ))
  
  #================================================================================================
  ####### TOBIT MODEL ########
  #================================================================================================
  tobit_model <- vglm(Zarith_Score ~ AGEP + SEXP + 
                        GMFCS + LevJobCP +
                        QALYsA + Social_Class + Tot_CostGov + CosT_Ycare +
                        RESID, tobit(Lower = -0, Upper = 88), 
                      data = train_data)
  
  tobit_preds <- predict(tobit_model, newdata = test_data, type = "response")
  
  errors_tobit <- rbind(errors_tobit, data.frame(
    RMSE = rmse(test_data$Zarith_Score, tobit_preds),
    MAE = mae(test_data$Zarith_Score, tobit_preds),
    ME = mean(test_data$Zarith_Score - tobit_preds)
  ))
  
  #================================================================================================
  ####### GLM MODEL ########
  #================================================================================================
  glm_model <- glm(Zarith_Score ~ AGEP + SEXP + EDULEVELC + HOUSEINCY + 
                     GMFCS + QALYsC + Tot_MedCare + RESID, 
                   data = train_data, 
                   family = gaussian(link = "identity"))
  
  glm_preds <- predict(glm_model, newdata = test_data, type = "response")
  
  errors_glm <- rbind(errors_glm, data.frame(
    RMSE = rmse(test_data$Zarith_Score, glm_preds),
    MAE = mae(test_data$Zarith_Score, glm_preds),
    ME = mean(test_data$Zarith_Score - glm_preds)
  ))
}
#================================================================================================
#RESULTS
#================================================================================================
# Average errors
cat("\n--- OLS MODEL ---\n")
cat("Average RMSE:", mean(errors_lm$RMSE), "\n")
cat("Average MAE:", mean(errors_lm$MAE), "\n")
cat("Average ME:", mean(errors_lm$ME), "\n")

cat("\n--- TOBIT MODEL ---\n")
cat("Average RMSE:", mean(errors_tobit$RMSE), "\n")
cat("Average MAE:", mean(errors_tobit$MAE), "\n")
cat("Average ME:", mean(errors_tobit$ME), "\n")

cat("\n--- GLM MODEL ---\n")
cat("Average RMSE:", mean(errors_glm$RMSE), "\n")
cat("Average MAE:", mean(errors_glm$MAE), "\n")
cat("Average ME:", mean(errors_glm$ME), "\n")
