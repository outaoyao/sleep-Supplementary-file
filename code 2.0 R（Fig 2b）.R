library(tidyverse)
library(survey)
library(broom)


data <- read.csv("D:\\Supplementary file\\2017-2023R data 11059_1.csv", stringsAsFactors = FALSE)

data$sfyy <- factor(data$sfyy, levels = c(0, 1), labels = c("non-depression", "depression"))

data$gzjxsjxsz <- as.numeric(data$gzjxsjxsz)
data$zmjxsjxsz <- as.numeric(data$zmjxsjxsz)
data$pjjxsjxsz <- as.numeric(data$pjjxsjxsz)

design_obj <- svydesign(
  id = ~SDMVPSU,          
  strata = ~SDMVSTRA,     
  weights = ~mecx,        
  data = data,
  nest = TRUE             
)

time_vars <- c("gzjxsjxsz", "zmjxsjxsz", "pjjxsjxsz")

results_list <- list()
for (var in time_vars) {
  
  formula <- as.formula(paste("~sfyy"))
  by_formula <- as.formula(paste("~", var))
  
  prop_result <- svyby(formula, by_formula, design_obj, svymean, na.rm = TRUE, vartype = c("se", "ci"))
  
  depression_prop <- prop_result[, paste0("sfyydepression")]
  depression_se <- prop_result[, paste0("se.sfyydepression")]
  depression_ci_lower <- prop_result[, paste0("ci_l.sfyydepression")]
  depression_ci_upper <- prop_result[, paste0("ci_u.sfyydepression")]
  
  
  result_df <- data.frame(Time_Period = prop_result[[var]],
                          Variable = var,
                          Depression_Probability = depression_prop,
                          Standard_Error = depression_se,
                          CI_lower = depression_ci_lower,
                          CI_upper = depression_ci_upper)
  results_list[[var]] <- result_df
}


all_results <- do.call(rbind, results_list)
rownames(all_results) <- NULL


test_results <- list()

for (var in time_vars) {
  
  formula <- as.formula(paste("~", var, "+ sfyy"))
  
  
  chisq_test <- svychisq(formula, design_obj, statistic = "F")
  
  
  test_results[[var]] <- tidy(chisq_test) %>% mutate(Variable = var) %>% select(Variable, Statistic = statistic, P_value = p.value)
}


all_test_results <- do.call(rbind, test_results)
rownames(all_test_results) <- NULL


print("Weighted depression probabilities by time period:")
print(all_results, row.names = FALSE)

print("Association tests between time variables and depression status:")
print(all_test_results, row.names = FALSE)


write.csv(all_results, "weighted_depression_probabilities_by_time_period.csv", row.names = FALSE)
write.csv(all_test_results, "depression_time_variable_association_tests.csv", row.names = FALSE)


all_test_results <- do.call(rbind, test_results)
rownames(all_test_results) <- NULL


raw_p_values <- all_test_results$P_value


bonferroni_adj_p <- p.adjust(raw_p_values, method = "bonferroni")
# Benjamini-Hochberg correction (controls False Discovery Rate)
bh_adj_p <- p.adjust(raw_p_values, method = "BH")


all_test_results$Bonferroni_adj_p <- bonferroni_adj_p
all_test_results$BH_adj_p <- bh_adj_p


print("Weighted depression probabilities by time period:")
print(all_results, row.names = FALSE)

print("Association tests between time variables and depression status (with multiple comparison correction):")
print(all_test_results, row.names = FALSE)


library(ggplot2)




all_results <- all_results %>%
  mutate(Variable_Label = case_when(
    Variable == "gzjxsjxsz" ~ "Workday wake time",
    Variable == "zmjxsjxsz" ~ "Weekend wake time", 
    Variable == "pjjxsjxsz" ~ "Average wake time"
  ))

fill_colors <- c("Workday wake time" = "steelblue", 
                 "Weekend wake time" = "gold", 
                 "Average wake time" = "#555555")


ggplot(all_results, aes(x = as.factor(Time_Period), y = Depression_Probability * 100, fill = Variable_Label)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = CI_lower * 100, ymax = CI_upper * 100), 
                position = position_dodge(0.8), width = 0.3, alpha = 0.6) +
  labs(
    title = "Association between wake time and depression prevalence in the NHANES (2017–2023)",
    x = "Hour (h)", 
    y = "Depression prevalence (%)",
    fill = " "
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_fill_manual(values = fill_colors)