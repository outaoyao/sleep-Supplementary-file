library(survey)
library(dplyr)
library(emmeans)
library(ggplot2)

data_path <- "D:\\Supplementary file\\2017-2023R data 11059_1.csv"
data <- read.csv(data_path)

data_clean <- data %>%
  mutate(
    sfyy = factor(sfyy, levels = c(0, 1), labels = c("non-depression", "depression")),
    smjlfl = factor(smjlfl, levels = c(0, 1, 2, 3), 
                    labels = c("MSRD-G 0", "MSRD-G I", "MSRD-G II", "MSRD-G III")),
    bmifd = factor(bmifd, levels = c(0, 1, 2, 3), 
                   labels = c("BMI:18.5-24.9kg/m²", "BMI:<18.5kg/m²", "BMI:25-29.9kg/m²", "BMI:≥30kg/m²")),
    DMDEDUC2 = factor(DMDEDUC2, levels = c(0, 1, 2),
                      labels = c("Below high school", "High school graduate", "College or above")),
    tnbqk = factor(tnbqk, levels = c(0, 1, 2), 
                   labels = c("No diabetes", "Has diabetes", "Pre-diabetes")),
    gzqk = factor(gzqk, levels = c(0, 1), 
                  labels = c("Employed", "Unemployed")),
    DMDMARTZ = factor(DMDMARTZ, levels = c(1, 2, 3),
                      labels = c("Married", "Widowed/Divorced/Separated", "Never married")),
    hdqkefl = factor(hdqkefl, levels = c(0, 1), 
                     labels = c("Mild activity", "Moderate/Vigorous activity")),
    BPQ020 = factor(BPQ020, levels = c(0, 1), labels = c("No", "Yes")),
    RIDRETH1 = factor(RIDRETH1, levels = c(1, 2, 3, 4, 5),
                      labels = c("Mexican American", "Other Hispanic", "White", "Black", "Other Race")),
    yjfd = factor(yjfd, levels = c(1, 2), labels = c("Non-drinker", "Drinker")),
    RIAGENDR = factor(RIAGENDR, levels = c(0, 1), labels = c("Female", "Male")),
    xyqk = factor(xyqk, levels = c(1, 2), labels = c("Non-smoker", "Smoker")),
    jtpir = factor(jtpir, levels = c(0, 1, 2),
                   labels = c("1.30-3.49", "<1.30", "≥3.50"))
  ) %>%
  # Remove missing values
  na.omit()

design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~mecx,
  nest = TRUE,
  data = data_clean
)

model <- svyglm(
  sfyy ~ smjlfl * bmifd + DMDEDUC2 + tnbqk + gzqk + DMDMARTZ + 
    hdqkefl + BPQ020 + RIDRETH1 + yjfd + RIDAGEYR + RIAGENDR + 
    jtpir + xyqk,
  design = design,
  family = quasibinomial()
)

cat("Model summary:\n")
summary(model)

cat("\nModel goodness of fit check (AIC):\n")
AIC(model)

survey_weights <- weights(design)

emm_results <- emmeans(
  model,
  specs = ~ smjlfl | bmifd,
  non.nuisance = c("smjlfl", "bmifd"),
  type = "response",
  wts = survey_weights
)

cat("Marginal mean results (accounting for complex survey weights):\n")
summary(emm_results)

emm_ci <- confint(emm_results)
cat("\n95% confidence intervals for marginal means:\n")
print(emm_ci)

contrast_bonferroni <- contrast(
  emm_results,
  method = "pairwise",
  adjust = "bonferroni"
)

cat("\nSimple effects test (Bonferroni correction):\n")
print(contrast_bonferroni)

contrast_bh <- contrast(
  emm_results,
  method = "pairwise",
  adjust = "fdr"  
)

cat("\nSimple effects test (BH correction):\n")
print(contrast_bh)

raw_p_values <- summary(contrast_bonferroni)$p.value

bonferroni_adjusted <- p.adjust(raw_p_values, method = "bonferroni")

bh_adjusted <- p.adjust(raw_p_values, method = "BH")

comparison_results <- data.frame(
  Comparison = summary(contrast_bonferroni)$contrast,
  BMI_Group = summary(contrast_bonferroni)$bmifd,
  Raw_p = raw_p_values,
  Bonferroni_adjusted = bonferroni_adjusted,
  BH_adjusted = bh_adjusted
)

cat("\nMultiple comparison correction results comparison:\n")
print(comparison_results)

emm_df <- as.data.frame(emm_ci)  # Ensure emm_ci contains confidence interval results

color_blind_friendly <- c(
  "BMI:18.5-24.9kg/m²" = "#0072B2",  # Blue
  "BMI:<18.5kg/m²" = "#E69F00",      # Orange
  "BMI:25-29.9kg/m²" = "#6A3D9A",    # Blue-green
  "BMI:≥30kg/m²" = "#CC79A7"         # Pink-purple
)

ggplot(emm_df, aes(x = smjlfl, y = prob * 100, color = bmifd, group = bmifd)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL * 100, ymax = asymp.UCL * 100), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = color_blind_friendly, name = "BMI Categories") +
  labs(title = "Association between MSRD-G and depression prevalence stratified by BMI categories",
       subtitle = "Marginal means and 95% confidence intervals",
       x = "MSRD-G",
       y = "Depression prevalence (%)",
       color = "BMI Categories") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


results_table <- emm_df %>%
  select(bmifd, smjlfl, prob, asymp.LCL, asymp.UCL) %>%
  mutate(Estimate = sprintf("%.3f", prob),
         CI = sprintf("(%.3f-%.3f)", asymp.LCL, asymp.UCL)) %>%
  select(bmifd, smjlfl, Estimate, CI)

cat("\nMarginal mean results table (for reporting):\n")
print(results_table)

p_value_table <- comparison_results %>%
  mutate(Bonferroni_sig = ifelse(Bonferroni_adjusted < 0.05, "Yes", "No"),
         BH_sig = ifelse(BH_adjusted < 0.05, "Yes", "No")) %>%
  select(Comparison, BMI_Group, Raw_p, Bonferroni_adjusted, BH_adjusted, Bonferroni_sig, BH_sig)

cat("\nMultiple comparison correction results table (for reporting):\n")
print(p_value_table)


#write.csv(comparison_results, "multiple_comparison_correction_results.csv", row.names = FALSE)
#write.csv(emm_df, "marginal_means_results.csv", row.names = FALSE)

# cat("\nAnalysis completed! Results saved to CSV files.\n")