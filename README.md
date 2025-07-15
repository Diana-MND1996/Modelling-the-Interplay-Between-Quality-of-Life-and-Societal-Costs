# Modelling Quality-of-Life-and-Costs
Modelling the Interplay Between Quality of Life and Societal Costs in Pediatric Cerebral Palsy: Individual-Level Evidence from Spain

### Diana Marcela Nova Díaz, Paloma Arana Rivera, Sergio Aguilera Albesa, Eduardo Sánchez Iriso, Diego Rivera

Individual-level economic models can better capture the heterogeneity of clinical severity, caregiving needs, and social context in pediatric cerebral palsy (CP). This study aimed to develop and validate prediction models estimating the impact of clinical, sociodemographic, socioeconomic, and quality-of-life (QoL) characteristics on disaggregated social and healthcare costs, and conversely, to examine how these characteristics and cost components influence QoL in pediatric CP in Spain.

This repository contains 6 R.script files containing the data and results of the article ‘Modelling the Interplay Between Quality of Life and Societal Costs in Pediatric Cerebral Palsy: Evidence at the individual level in Spain’. Each is described below:

## 📂 Scripts incluidos

- [1. Data preparation.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/1.%20Data%20preparation.R):
 It contains the data previously organized and classified by variable type, preparing it for analysis and saving the final dataset for use.
- [2. Descriptive Results Table S1.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/2.%20Descriptive%20Results%20Table%20S1.R): This script presents the baseline characteristics of 148 individuals with cerebral palsy (CP) and their caregivers. The results are disaggregated according to the presence or absence of out-of-pocket (OOP) healthcare expenditures. It generates the output for Supplementary Table S1 of the manuscript.
- [3. Variable Selection Costs Model.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/3.%20Variable%20Selection%20Costs%20Models.R): This script performs variable selection and model fitting using the glmulti package to explore the relationship between individual and contextual characteristics, quality of life (QALYs), and various types of societal costs in children with cerebral palsy. It includes parallelized model estimation for multiple cost outcomes and compares generalized linear models (GLMs) across different family/link functions based on AIC.
- [3.1. Cost_CP_Models and Cross_validation.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/3.1.%20Cost_CP_Models%20and%20Cross_validation2part.R): Fits the final cost models selected in previous steps (Gamma GLMs) for six cost outcomes and implements a two‑part model for intensive & emerging rehabilitation‑therapy costs. The script then performs 10‑fold cross‑validation, comparing Gaussian‑identity, Gaussian‑log and Gamma‑log specifications, and reports RMSE, MAE and mean error.
- [4. QoL_var_selection_care_overlap.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/4.%20QoL_var_selection_care_overload_finalModels_CrossValidation.R): It shows the variable selection, final construction and cross-validation of statistical models (OLS, Tobit and GLM) that analyse the relationship between quality of life and costs of Cerebral Palsy in children and their caregivers in Spain. It includes modelling at the individual level, automatic variable selection using stepwise, and evaluation of predictive performance using k-fold cross-validation.
- [5. IC_bootstrap_modelos_Supplementary.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/5.%20IC_bootstrap_modelos_Supp_Mat.R)
- [6. Graphic_Predict_Final_Models_Fig2.R](https://github.com/Diana-MND1996/Modelling-the-Interplay-Between-Quality-of-Life-and-Societal-Costs/blob/main/6.%20Graphic_Predict_Final_Models_Fig2.R)

     
   
