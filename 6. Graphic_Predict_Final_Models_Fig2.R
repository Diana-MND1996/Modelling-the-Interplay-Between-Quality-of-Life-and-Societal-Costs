# ============================================================================
# Title: Modelling the Interplay Between Quality of Life and Societal Costs in 
# Pediatric Cerebral Palsy: Individual-Level Evidence from Spain 
# FIGURE 2: Predicted relationships between costs, HRQoL and key determinants based on final models
# Date: June 2025
# ============================================================================

#========================
# 1. Required packages
#========================
library(sjPlot)      # For ggpredict()
library(ggeffects)   # idem
library(ggplot2)     # graphics
library(gridExtra)   # (if you want to combine panels)

#========================
# 2. Load data
#========================
load("costs_data.RData")   

#-------------------------------------------------
# FIGURE 2A Social cost (GTotCost) vs Zarit Score
#-------------------------------------------------
model_SocialCost <- glm(
  GTotCost ~ SEXP + SEXC + GMFCS + ETIOL + MRICS +
    poly(Zarith_Score, 3) + AGEP,
  data   = data,
  family = Gamma(link = "log")
)

pred_data1 <- ggpredict(
  model_SocialCost,
  terms          = c("Zarith_Score", "GMFCS", "AGEP"),
  back.transform = TRUE
)

# We eliminate confidence bands
pred_data1$conf.low  <- NA
pred_data1$conf.high <- NA

plot_cost1 <- plot(
  pred_data1,
  colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
) +
  labs(
    x        = "Caregiver burden (Zarit Score)",
    y        = "Predicted Societal Cost (€)",
    title    = "Predicted Societal Costs by Caregiver Burden, GMFCS, and Patient Age",
    colour   = "GMFCS level",   # Title for the legend
    linetype = "GMFCS level"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "figure2A_costs_ZB_short.jpeg",
  plot   = plot_cost1,
  width  = 10, height = 8, dpi = 600, units = "in", device = "jpeg"
)

#---------------------------------------------------------
# FIGURE 2B Total cost CP (Tot_CostCP) vs HRQoL (QALYsC)
#---------------------------------------------------------
model_CostsCP <- glm(
  Tot_CostCP ~ SEXP + GMFCS + GOI + SCH_SUPP + VSS + VFCS +
    EDACS + CFCS + ETIOL + AGEC + HOUSEINCY +
    QALYsA + QALYsC,
  data   = data,
  family = Gamma(link = "log")
)

pred_data2 <- ggpredict(
  model_CostsCP,
  terms          = c("QALYsC", "GMFCS", "GOI"),
  back.transform = TRUE
)

pred_data2$conf.low  <- NA
pred_data2$conf.high <- NA

plot_cost2 <- plot(
  pred_data2,
  colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
) +
  labs(
    x        = "Health-Related Quality of Life (QALYs)",
    y        = "Predicted Cost of Illness (€)",
    title    = "Predicted Cost of Illness by HRQoL, GMFCS Level, and Geographical Origin",
    colour   = "GMFCS level",
    linetype = "GMFCS level"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "figure2B_costs_qalys_short.jpeg",
  plot   = plot_cost2,
  width  = 10, height = 8, dpi = 600, units = "in", device = "jpeg"
)

#---------------------------------------------------------------------
# FIGURE 2C Cost of informal care (CosT_Ycare) vs HRQoL (QALYsC)
#---------------------------------------------------------------------
model_Care <- glm(
  CosT_Ycare ~ SEXC + GOI + CFCS + ETIOL + MRICS +
    poly(Zarith_Score, 3) + QALYsC,
  data   = data,
  family = Gamma(link = "log")
)

pred_data3 <- ggpredict(
  model_Care,
  terms          = c("QALYsC", "ETIOL", "GOI"),
  back.transform = TRUE
)

pred_data3$conf.low  <- NA
pred_data3$conf.high <- NA

plot_cost3 <- plot(
  pred_data3,
  colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
) +
  labs(
    x        = "Health-Related Quality of Life (QALYs)",
    y        = "Predicted Informal Costs of Care (€)",
    title    = "Predicted Informal Care Costs by HRQoL, Etiology of CP, and Geographical Origin",
    colour   = "Etiology of CP",   # Title for the legend
    linetype = "Etiology of CP"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "figure2C_costs_qalys_short.jpeg",
  plot   = plot_cost3,
  width  = 10, height = 8, dpi = 600, units = "in", device = "jpeg"
)


#JOIN GRAPHICS

plot_cost1 <- plot_cost1 + theme(plot.title = element_text(size = 11))
plot_cost2 <- plot_cost2 + theme(plot.title = element_text(size = 11))
plot_cost3 <- plot_cost3 + theme(plot.title = element_text(size = 11))


library(patchwork)

design_ABizq_Cder <- "
AC
BC
"

combined_panel <- plot_cost1 + plot_cost2 + plot_cost3 +
  plot_layout(design = design_ABizq_Cder) +
  plot_annotation(tag_levels = 'A')

ggsave(
  "figure_costs_AB-izq_C-der_smalltitles.jpeg",
  plot   = combined_panel,
  width  = 16, height = 10, dpi = 600, units = "in"
)
#------------------------------
# End of script
#------------------------------

