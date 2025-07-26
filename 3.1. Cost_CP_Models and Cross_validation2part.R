# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 
# FINAL COST MODELS AND CROSS VALIDATION OF TWO PART MODELS
# Date: June 2025
# ============================================================================

# ==============================================================================================
# DATA LOADING AND LIBRARIES
# =============================================================================================
library(dplyr)
library(Metrics)  # For RMSE, MAE, ME
library(readxl)     # To import data
library(tidyr)      # For data reorganisation

load("costs_data.RData")
View(data)

#================================================================================================
#GET THE FINAL MODELS OF THE OBJECT AFTER SELECTING VARIABLES WITH 2 TO THE P (GLMULTI)
#================================================================================================

#CosT_Ycare------------> Cost of informal care
summary(part1_model_CosT_Ycare@objects[[1]]) 

#GTotCost------------> Social cost of cerebral palsy
summary(part1_model_GTotCost@objects[[1]]) 

#Tot_CostCP----------> Cost of cerebral palsy
summary(part1_model_Tot_CostCP@objects[[1]]) 

# Tot_CostGov--------> Costs associated with government
summary(part1_model_Tot_CostGov@objects[[1]])

#Tot_MedCare----------> Medical care cost
summary(part1_model_Tot_MedCare@objects[[1]])

#Tot_OutPocket_part----------> Total out-of-pocket CP costs
summary(part1_model_Tot_OutPocket@objects[[1]])

#================================================================================================        
#TWO PARTS MODEL-------------->Cost of incurring or not incurring IERT (Intensive and Emerging Rehabilitation Therapies)
#================================================================================================
#Part_1_Tot_CostTTCPAL
summary(best_part1@objects[[1]])

#Part_2A_Tot_CostTTCPAL with Gaussian identity
summary(model_gau_id@objects[[1]]) 

#Part_2B_Tot_CostTTCPAL with Gaussian logit
summary(model_gau_log@objects[[1]]) 

#Part_2C_Tot_CostTTCPAL
summary(model_gam_log@objects[[1]]) 

#================================================================================================
# IF YOU CANNOT LOAD THE MODELS WITH OBJECTS, AFTER VARIABLES SELECTION
# Here below are the final models rewritten, after variables selection
#================================================================================================
#1) CosT_Ycare------------> Cost of informal care
#================================================================================================

# Ajustar el modelo GLM con familia Gamma y link log
model_Care <- glm(
  CosT_Ycare ~ SEXC + GOI + CFCS + ETIOL + MRICS +
    Zarith_Score + z_QALYsC,
  data = data,  
  family = Gamma(link = "log")
)

# Summary of the model
summary(model_Care)

#================================================================================================
#2) #GTotCost------------> Social cost of cerebral palsy
#================================================================================================
model_SocialCost <- glm(
  GTotCost ~ SEXP + SEXC + GMFCS + ETIOL + MRICS +
    Zarith_Score + z_AGEP,
  data = data,
  family = Gamma(link = "log")
)

# Summary of the model
summary(model_SocialCost)

#================================================================================================
#3) #Tot_CostCP----------> Cost of cerebral palsy
#================================================================================================

model_CostsCP <- glm(
  Tot_CostCP ~ SEXP + GMFCS + GOI + SCH_SUPP + VSS + VFCS +
    EDACS + CFCS + ETIOL + z_AGEC + z_HOUSEINCY +
    z_QALYsA + z_QALYsC,
  data = data,
  family = Gamma(link = "log")
)

# Summary of the model
summary(model_CostsCP)

#================================================================================================
#4) # Tot_CostGov--------> Costs associated with government
#================================================================================================
model_CostPayer <- glm(
  Tot_CostGov ~ GMFCS + CIVISTAT + Zarith_Score,
  data = data,
  family = Gamma(link = "log")
)

# Summary of the model
summary(model_CostPayer)

#================================================================================================
#5)#Tot_MedCare----------> Medical care cost
#================================================================================================
model_MedCare <- glm(Tot_MedCare ~ SEXC + SEXP + EDACS + z_QALYsA,data = data,
                     family = Gamma(link = "log")
)

# Summary of the model
summary(model_MedCare)

#================================================================================================
#6) #Tot_OutPocket_part----------> Total out-of-pocket CP costs
#================================================================================================
model_CostOutPocket <- glm(
  Tot_OutPocket ~ SEXP + GMFCS + GOI + ETIOL + 
    z_AGEC + z_HOUSEINCY  + z_QALYsC,
  data = data,
  family = Gamma(link = "log")
)

# Summary of the model
summary(model_CostOutPocket)

#================================================================================================
#7) TWO PARTS MODEL-------------->Cost OF IERT (Intensive and Emerging Rehabilitation Therapies)
#================================================================================================
# > Part 1: probability of any costs incurring in the period
# > Part 2: costs conditional on any costs incurring in the period

#================================================================================================
## Part 1 ----
#================================================================================================
# Probability of any costs incurring in the period

# > Logistic regression ----

###BEST_PART1_TotCostTTCPAL
CostTH <- data %>%
  mutate(positive_cost = ifelse(Tot_CostTTCPAL > 0, 1, 0))

# Logistic model (Part 1): probability of incurring a positive cost
model_part1_logit <- glm(
  positive_cost ~ SEXP + GMFCS + GOI + Social_Class + MRICS + z_AGEP + z_QALYsC,
  data = CostTH,
  family = binomial(link = "logit")
)

summary(model_part1_logit)
#================================================================================================
# > Part 2: costs conditional on any costs incurring in the period
#================================================================================================
# Filtrar datos con coste positivo
data_positive <- data %>% filter(Tot_CostTTCPAL > 0)

#================================================================================================
#Part_2A_Tot_CostTTCPAL with Gaussian identity
#================================================================================================

model_Cost_TTCPAL_gauID <- glm(
  Tot_CostTTCPAL ~ GMFCS + LevJobCP + Social_Class + SCH_SUPP + VFCS + RESID + z_QALYsC,
  data = data_positive,  # only data with positive total cost
  family = gaussian(link = "identity")
)

summary(model_Cost_TTCPAL_gauID)

#================================================================================================
#Part_2B_Tot_CostTTCPAL with Gaussian logit
#================================================================================================
model_Cost_TTCPAL_gaulog <- glm(
  Tot_CostTTCPAL ~ GMFCS + LevJobCP + Social_Class + SCH_SUPP + VFCS + RESID +
    Zarith_Score,
  data = data_positive,  # only data with positive total cost
  family = gaussian(link = "log")
)

summary(model_Cost_TTCPAL_gaulog)

#================================================================================================
#Part_2C_Tot_CostTTCPAL with gamma log
#================================================================================================
# Modelo Gamma log (Parte 2C)
#================================================================================================

model2P_Cost_TTCPAL_gammaLog <- glm(
  Tot_CostTTCPAL ~ GMFCS + Social_Class + SCH_SUPP + RESID + IMP_INDEX + z_QALYsC,
  data = data_positive,
  family = Gamma(link = "log")
)

summary(model2P_Cost_TTCPAL_gammaLog)


########################################################################################################
#CROSSED VALIDATION K-FOLDS TWO PARTS MODELS Tot_CostTTCPAL
########################################################################################################
library(caret)   # to create Folds
library(Metrics) # for rmse, mae

# Seed for reproducibility
set.seed(123)
k_folds <- 10

# Use only data with positive costs
data_pos <- subset(data, Tot_CostTTCPAL > 0)

# Create folds
folds <- createFolds(data_pos$Tot_CostTTCPAL, k = k_folds, list = TRUE, returnTrain = TRUE)

# Initialise data frames for errors
errors_gauID <- data.frame(RMSE = numeric(k_folds), MAE = numeric(k_folds), ME = numeric(k_folds))
errors_gauLog <- data.frame(RMSE = numeric(k_folds), MAE = numeric(k_folds), ME = numeric(k_folds))
errors_gammaLog <- data.frame(RMSE = numeric(k_folds), MAE = numeric(k_folds), ME = numeric(k_folds))

# Cross Validation
for (i in 1:k_folds) {
  
  # Split data
  train_data <- data_pos[folds[[i]], ]
  test_data <- data_pos[-folds[[i]], ]
  
  #================================================================================================
  ## ---- GAUSSIAN MODEL IDENTITY ----
  #================================================================================================
  gauID_model <- glm(
    Tot_CostTTCPAL ~ GMFCS + LevJobCP + Social_Class + SCH_SUPP + VFCS + RESID + z_QALYsC,
    data = train_data,
    family = gaussian(link = "identity")
  )
  
  pred_gauID <- predict(gauID_model, newdata = test_data)
  
  errors_gauID$RMSE[i] <- rmse(test_data$Tot_CostTTCPAL, pred_gauID)
  errors_gauID$MAE[i] <- mae(test_data$Tot_CostTTCPAL, pred_gauID)
  errors_gauID$ME[i] <- mean(test_data$Tot_CostTTCPAL - pred_gauID)
  
  #================================================================================================
  ## ---- GAUSSIAN MODEL LOG ----
  #================================================================================================
  gauLog_model <- glm(
    Tot_CostTTCPAL ~ GMFCS + LevJobCP + Social_Class + SCH_SUPP + VFCS + RESID +
      Zarith_Score,
    data = train_data,
    family = gaussian(link = "log")
  )
  
  pred_gauLog <- predict(gauLog_model, newdata = test_data, type = "response")
  
  errors_gauLog$RMSE[i] <- rmse(test_data$Tot_CostTTCPAL, pred_gauLog)
  errors_gauLog$MAE[i] <- mae(test_data$Tot_CostTTCPAL, pred_gauLog)
  errors_gauLog$ME[i] <- mean(test_data$Tot_CostTTCPAL - pred_gauLog)
  
  #================================================================================================
  ## ---- GAMMA LOG MODEL ----
  #================================================================================================
  gammaLog_model <- glm(
    Tot_CostTTCPAL ~ GMFCS + Social_Class + SCH_SUPP + RESID + IMP_INDEX +
      z_QALYsC,
    data = train_data,
    family = Gamma(link = "log")
  )
  
  pred_gammaLog <- predict(gammaLog_model, newdata = test_data, type = "response")
  
  errors_gammaLog$RMSE[i] <- rmse(test_data$Tot_CostTTCPAL, pred_gammaLog)
  errors_gammaLog$MAE[i] <- mae(test_data$Tot_CostTTCPAL, pred_gammaLog)
  errors_gammaLog$ME[i] <- mean(test_data$Tot_CostTTCPAL - pred_gammaLog)
}

#================================================================================================
# Results
#================================================================================================
cat("\n=== Cross Validation Results ===\n")
cat("\n--- GAUSSIAN MODEL IDENTITY ---\n")
cat("RMSE Average:", mean(errors_gauID$RMSE), "\n")
cat("MAE Average:", mean(errors_gauID$MAE), "\n")
cat("ME Average:", mean(errors_gauID$ME), "\n")

cat("\n--- LOG GAUSSIAN MODEL ---\n")
cat("RMSE Average:", mean(errors_gauLog$RMSE), "\n")
cat("MAE Average:", mean(errors_gauLog$MAE), "\n")
cat("ME Average:", mean(errors_gauLog$ME), "\n")

cat("\n--- GAMMA LOG MODEL ---\n")
cat("RMSE Average:", mean(errors_gammaLog$RMSE), "\n")
cat("MAE Average:", mean(errors_gammaLog$MAE), "\n")
cat("ME Average:", mean(errors_gammaLog$ME), "\n")
