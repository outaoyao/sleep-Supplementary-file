library(survey)
library(rms)
library(ggplot2)
library(cowplot)


source("D:/Supplementary file/svyggrcs1.8R.R")

data_path <- "D:\\Supplementary file\\2017-2023R data 11059_1.csv"
nhanes_data <- read.csv(data_path)


nhanes_data$sfyy <- factor(nhanes_data$sfyy, levels = c(0, 1), labels = c("non-depression", "depression"))
nhanes_data$bmifd <- factor(nhanes_data$bmifd, levels = c(0, 1, 2, 3), 
                            labels = c("18.5-24.9kg/m²", "<18.5kg/m²", "25-29.9kg/m²", "≥30kg/m²"))
nhanes_data$yjfd <- factor(nhanes_data$yjfd, levels = c(1, 2), labels = c("Non-drinker", "Drinker"))
nhanes_data$RIAGENDR <- factor(nhanes_data$RIAGENDR, levels = c(0, 1), labels = c("Female", "Male"))
nhanes_data$RIDRETH1 <- factor(nhanes_data$RIDRETH1, levels = 1:5,
                               labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", 
                                          "Non-Hispanic Black", "Other Race"))
nhanes_data$DMDEDUC2 <- factor(nhanes_data$DMDEDUC2, levels = c(0, 1, 2),
                               labels = c("Less than high school", "High school or equivalent", "Some college or above"))
nhanes_data$DMDMARTZ <- factor(nhanes_data$DMDMARTZ, levels = 1:3,
                               labels = c("Married/Living with partner", "Widowed/Divorced/Separated", "Never married"))
nhanes_data$jtpir <- factor(nhanes_data$jtpir, levels = c(0, 1, 2),
                            labels = c("1.30-3.49", "<1.30", "≥3.50"))
nhanes_data$gzqk <- factor(nhanes_data$gzqk, levels = c(0, 1),
                           labels = c("Employed or self-employed", "Unemployed"))
nhanes_data$BPQ020 <- factor(nhanes_data$BPQ020, levels = c(0, 1),
                             labels = c("No", "Yes"))
nhanes_data$tnbqk <- factor(nhanes_data$tnbqk, levels = c(0, 1, 2),
                            labels = c("No diabetes", "Diabetes", "Pre-diabetes"))
nhanes_data$xyqk <- factor(nhanes_data$xyqk, levels = c(1, 2),
                           labels = c("Non-smoker", "Current smoker"))
nhanes_data$hdqkefl <- factor(nhanes_data$hdqkefl, levels = c(0, 1),
                              labels = c("Light recreational activity", "Moderate/Vigorous recreational activity"))

design <- svydesign(
  id = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  weights = ~mecx, 
  data = nhanes_data, 
  nest = TRUE
)

dd <- datadist(nhanes_data)
options(datadist = "dd")

model_gzrs <- svyglm(
  sfyy ~ rcs(gzrs, 4) + RIDAGEYR + bmifd + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design,
  family = quasibinomial()
)

model_zmrs <- svyglm(
  sfyy ~ rcs(zmrs, 4) + RIDAGEYR + bmifd + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design,
  family = quasibinomial()
)

model_pjrs <- svyglm(
  sfyy ~ rcs(pjrs, 4) + RIDAGEYR + bmifd + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design,
  family = quasibinomial()
)

rcs_gzrs <- svyggrcs(model_gzrs)
rcs_zmrs <- svyggrcs(model_zmrs)
rcs_pjrs <- svyggrcs(model_pjrs)

plots_list <- list(rcs_gzrs, rcs_zmrs, rcs_pjrs)
combined_plots <- lapply(plots_list, function(p) {
  p + geom_vline(xintercept = 22, linetype = "dashed", color = "blue", alpha = 0.7)
})
print(rcs_gzrs) 
print(rcs_zmrs)
print(rcs_pjrs)

data_gzrs <- rcs_gzrs[["newdat"]]
data_zmrs <- rcs_zmrs[["newdat"]] 
data_pjrs <- rcs_pjrs[["newdat"]] 

data_gzrs$variable <- "gzrs"
data_zmrs$variable <- "zmrs"
data_pjrs$variable <- "pjrs"

colnames(data_zmrs) <- colnames(data_gzrs)
colnames(data_pjrs) <- colnames(data_gzrs)

combined_data <- rbind(data_gzrs, data_zmrs, data_pjrs)

head(combined_data)

overlap_plot <- ggplot(combined_data, aes(x = gzrs, y = yhat, group = variable, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = 22, linetype = "dashed", color = "#FF8C00", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.8, size = 0.7) +
  labs(x = "Hour(h)", y = "OR(95%CI)", 
       title = "RCS curves depicting the relationship between sleep onset time and depression risk",
       color = NULL, fill = NULL) +
  scale_color_manual(values = c("gzrs" = "sienna", "zmrs" = "mediumpurple", "pjrs" = "#888888"),
                     labels = c("gzrs" = "Weekday sleep onset time", 
                                "zmrs" = "Weekend sleep onset time", 
                                "pjrs" = "Average sleep onset time")) +
  scale_fill_manual(values = c("gzrs" = "sienna", "zmrs" = "mediumpurple", "pjrs" = "#888888"),
                    labels = c("gzrs" = "Weekday sleep onset time", 
                               "zmrs" = "Weekend sleep onset time", 
                               "pjrs" = "Average sleep onset time")) +
  scale_x_continuous(breaks = seq(floor(min(combined_data$gzrs)), 
                                  ceiling(max(combined_data$gzrs)), 
                                  by = 1)) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black")
  )

print(overlap_plot)