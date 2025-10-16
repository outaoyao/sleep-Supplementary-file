library(survey)
library(dplyr)
library(forestplot)
library(grid)
library(car)

data <- read.csv("D:\\Supplementary file\\2017-2023R data 11059_1.csv", stringsAsFactors = FALSE)


data$sfyy <- factor(data$sfyy, levels = c(0, 1), labels = c("non-depression", "depression"))

data$smjl <- factor(data$smjl, levels = c(0, 1, 2, 3, 4, 5, 6), 
                    labels = c("0 points", "1 point", "2 points", "3 points", 
                               "4 points", "5 points", "6 points"))

data$RIAGENDR <- factor(data$RIAGENDR, levels = c(0, 1), labels = c("Female", "Male"))
data$RIDRETH1 <- factor(data$RIDRETH1, levels = c(1, 2, 3, 4, 5), 
                        labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", 
                                   "Non-Hispanic Black", "Other Race"))

data$DMDEDUC2 <- factor(data$DMDEDUC2, levels = c(0, 1, 2),
                        labels = c("Less than high school", "High school graduate or equivalent", "Some college or above"))
data$DMDMARTZ <- factor(data$DMDMARTZ, levels = c(1, 2, 3),
                        labels = c("Married/Living with partner", "Widowed/Divorced/Separated", "Never married"))
data$jtpir <- factor(data$jtpir, levels = c(0, 1, 2),
                     labels = c("1.30-3.49", "<1.30", "≥3.50"))
data$gzqk <- factor(data$gzqk, levels = c(0, 1),
                    labels = c("Has job or business", "No job or business"))
data$BPQ020 <- factor(data$BPQ020, levels = c(0, 1),
                      labels = c("No", "Yes"))
data$tnbqk <- factor(data$tnbqk, levels = c(0, 1, 2),
                     labels = c("No diabetes", "Has diabetes", "Pre-diabetes"))
data$xyqk <- factor(data$xyqk, levels = c(1, 2),
                    labels = c("Non-smoker", "Current smoker"))
data$hdqkefl <- factor(data$hdqkefl, levels = c(0, 1),
                       labels = c("Mild recreational activity", "Moderate/Vigorous recreational activity"))
data$bmifd <- factor(data$bmifd, levels = c(0, 1, 2, 3),
                     labels = c("18.5-24.9kg/m²", "<18.5kg/m²", "25-29.9kg/m²", "≥30kg/m²"))
data$yjfd <- factor(data$yjfd, levels = c(1, 2),
                    labels = c("Non-drinker", "Drinker"))


design_obj <- svydesign(
  id = ~SDMVPSU,          
  strata = ~SDMVSTRA,     
  weights = ~mecx,        
  data = data,
  nest = TRUE             
)


model1 <- svyglm(
  sfyy ~ smjl,
  design = design_obj,
  family = quasibinomial()  
)

model2 <- svyglm(
  sfyy ~ smjl + RIAGENDR + RIDAGEYR + RIDRETH1,
  design = design_obj,
  family = quasibinomial()
)

model3 <- svyglm(
  sfyy ~ smjl + RIAGENDR + RIDAGEYR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + 
    tnbqk + xyqk + hdqkefl + bmifd + yjfd,
  design = design_obj,
  family = quasibinomial()
)


extract_results <- function(model, model_name) {
  coefs <- coef(model)
  cis <- confint(model)
  ors <- exp(coefs)
  or_cis <- exp(cis)
  
  results <- data.frame(
    Model = model_name,
    Variable = names(coefs),
    OR = ors,
    CI_lower = or_cis[, 1],
    CI_upper = or_cis[, 2],
    P_value = summary(model)$coefficients[, 4]
  )
  return(results)
}


results1 <- extract_results(model1, "Model 1: Unadjusted")
results2 <- extract_results(model2, "Model 2: Demographically adjusted")
results3 <- extract_results(model3, "Model 3: Fully adjusted")


all_results <- rbind(
  results1[grepl("smjl", results1$Variable), ],
  results2[grepl("smjl", results2$Variable), ],
  results3[grepl("smjl", results3$Variable), ]
)

print(all_results)


cat("\n=== Model 1 Summary ===\n")
summary(model1)

cat("\n=== Model 2 Summary ===\n")
summary(model2)

cat("\n=== Model 3 Summary ===\n")
summary(model3)


p_values <- all_results$P_value[all_results$Variable != "(Intercept)"]

bonferroni_adj <- p.adjust(p_values, method = "bonferroni")
bh_adj <- p.adjust(p_values, method = "BH")

all_results$Bonferroni_adj_p <- NA
all_results$BH_adj_p <- NA

non_intercept_indices <- which(all_results$Variable != "(Intercept)")
all_results$Bonferroni_adj_p[non_intercept_indices] <- bonferroni_adj
all_results$BH_adj_p[non_intercept_indices] <- bh_adj

cat("\n=== Adjusted results ===\n")
print(all_results)


cat("\n=== Model 3 Variance Inflation Factor (VIF) ===\n")
vif_values <- vif(model3)
print(vif_values)


model3_data <- all_results[all_results$Model == "Model 3: Fully adjusted" & grepl("smjl", all_results$Variable), ]

model3_data$Level <- gsub("smjl", "", model3_data$Variable)
model3_data$Level <- factor(model3_data$Level,
                            levels = c("0 points", "1 point", "2 points", "3 points", 
                                       "4 points", "5 points", "6 points"),
                            labels = c("0 points", "1 point", "2 points", "3 points", 
                                       "4 points", "5 points", "6 points"))


ref_group <- data.frame(
  Model = "Model 3: Fully adjusted",
  Variable = "smjl 0 points",
  OR = 1,
  CI_lower = 1,
  CI_upper = 1,
  P_value = NA,
  Bonferroni_adj_p = NA,
  BH_adj_p = NA,
  Level = "0 points"
)

model3_data <- rbind(ref_group, model3_data)


model3_data$P_value_formatted <- ifelse(
  is.na(model3_data$P_value) | model3_data$Level == "Level 0",
  "ref",
  ifelse(model3_data$P_value < 0.001, 
         "<0.001", 
         sprintf("%.3f", model3_data$P_value))
)


labeltext <- cbind(
  c("Multidimensional sleep rhythm 
    dysregulation score", as.character(model3_data$Level)),
  c("OR (95% CI)", 
    ifelse(model3_data$Level == "0 point", 
           "ref",
           sprintf("%.2f (%.2f-%.2f)", model3_data$OR, model3_data$CI_lower, model3_data$CI_upper))),
  c("P value", model3_data$P_value_formatted)
)

mean <- c(NA, model3_data$OR)
lower <- c(NA, model3_data$CI_lower)
upper <- c(NA, model3_data$CI_upper)

is_summary <- c(TRUE, rep(FALSE, nrow(model3_data)))


forestplot(labeltext = labeltext,
           mean = mean,
           lower = lower,
           upper = upper,
           is.summary = is_summary,
           graph.pos = 3,
           xlog = TRUE,
           zero = 1,
           col = fpColors(box = "#5F9EA0", lines = "#5F9EA0", summary = "black"),
           boxsize = 0.25,
           lwd.ci = 2,
           ci.vertices = TRUE,
           title = "Association between multidimensional sleep rhythm dysregulation 
           and depression risk (Model 3)",
           xlab = "OR (95% CI)",
           txt_gp = fpTxtGp(
             xlab = gpar(cex = 1.1),
             title = gpar(cex = 1.2, fontface = "bold"),
             label = gpar(cex = 1.0),
             ticks = gpar(cex = 0.9),
             summary = gpar(cex = 1.0, fontface = "bold")
           ),
           hrzl_lines = list(
             "2" = gpar(lwd = 1, col = "black")
           )
)


cat("\n=== Trend test (smjl as continuous variable) ===\n")
trend_model <- svyglm(
  sfyy ~ as.numeric(smjl) + RIAGENDR + RIDAGEYR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + 
    tnbqk + xyqk + hdqkefl + bmifd + yjfd,
  design = design_obj,
  family = quasibinomial()
)

trend_summary <- summary(trend_model)
trend_coef <- coef(trend_model)["as.numeric(smjl)"]
trend_p <- trend_summary$coefficients["as.numeric(smjl)", "Pr(>|t|)"]

cat(sprintf("Trend coefficient: %.3f, P-value: %.4f\n", trend_coef, trend_p))


freq_table <- table(data$smjl)
print(freq_table)

rel_freq_table <- prop.table(freq_table) * 100
print(rel_freq_table)


