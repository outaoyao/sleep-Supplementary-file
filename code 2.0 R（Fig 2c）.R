library(survey)
library(broom)
library(dplyr)

data <- read.csv("D:\\Supplementary file\\2017-2023R data 11059_1.csv", stringsAsFactors = FALSE)
data$sfyy <- factor(data$sfyy, levels = c(0, 1), labels = c("non-depression", "depression"))
data$gzzdxsz <- as.numeric(data$gzzdxsz)
data$zmzdxsz <- as.numeric(data$zmzdxsz)
data$pjsmzdxsz <- as.numeric(data$pjsmzdxsz)

design_obj <- svydesign(
  id 
  = ~SDMVPSU,          
  strata 
  = ~SDMVSTRA,     
  weights 
  = ~mecx,        
  data 
  = data,
  nest 
  = TRUE             
)

time_vars <- c("gzzdxsz", "zmzdxsz", "pjsmzdxsz")

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

bh_adj_p <- p.adjust(raw_p_values, method = "BH")


all_test_results$Bonferroni_adj_p <- bonferroni_adj_p
all_test_results$BH_adj_p <- bh_adj_p


print("Weighted depression probabilities by time period:")
print(all_results, row.names = FALSE)

print("Association tests between time variables and depression status (with multiple comparison correction):")
print(all_test_results, row.names = FALSE)


library(ggplot2)
library(dplyr) 


all_results <- all_results %>%
  mutate(Variable_Label = case_when(
    Variable == "gzzdxsz" ~ "Workday sleep midpoint",
    Variable == "zmzdxsz" ~ "Weekend sleep midpoint", 
    Variable == "pjsmzdxsz" ~ "Average sleep midpoint"
  ))


fill_colors <- c("Workday sleep midpoint" = "#7B68EE", 
                 "Weekend sleep midpoint" = "#9932CC", 
                 "Average sleep midpoint" = "#BBBBBB")


ggplot(all_results, aes(x = as.factor(Time_Period), y = Depression_Probability * 100, fill = Variable_Label)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = CI_lower * 100, ymax = CI_upper * 100), 
                position = position_dodge(0.8), width = 0.3, colour = "black", alpha = 0.6) +
  labs(title = "Association between sleep midpoint and depression prevalence in the NHANES (2017–2023)",
       x = "Hour (h)", 
       y = "Depression prevalence (%)",
       fill = " ") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  scale_fill_manual(values = fill_colors)