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

model_gzjx <- svyglm(
  sfyy ~ rcs(gzjx, 4) + RIDAGEYR + RIAGENDR + RIDRETH1,
  design = design,
  family = quasibinomial()
)

model_zmjx <- svyglm(
  sfyy ~ rcs(zmjx, 4) + RIDAGEYR + RIAGENDR + RIDRETH1,
  design = design,
  family = quasibinomial()
)

model_pjjx <- svyglm(
  sfyy ~ rcs(pjjx, 4) + RIDAGEYR + RIAGENDR + RIDRETH1,
  design = design,
  family = quasibinomial()
)

rcs_gzjx <- svyggrcs(model_gzjx)
rcs_zmjx <- svyggrcs(model_zmjx)
rcs_pjjx <- svyggrcs(model_pjjx)

plots_list <- list(rcs_gzjx, rcs_zmjx, rcs_pjjx)
combined_plots <- lapply(plots_list, function(p) {
  p + geom_vline(xintercept = 6, linetype = "dashed", color = "blue", alpha = 0.7)
})
print(rcs_gzjx) 
print(rcs_zmjx)
print(rcs_pjjx)

data_gzjx <- rcs_gzjx[["newdat"]] 
data_zmjx <- rcs_zmjx[["newdat"]] 
data_pjjx <- rcs_pjjx[["newdat"]] 

data_gzjx$variable <- "gzjx"
data_zmjx$variable <- "zmjx"
data_pjjx$variable <- "pjjx"

colnames(data_zmjx) <- colnames(data_gzjx)
colnames(data_pjjx) <- colnames(data_gzjx)

combined_data <- rbind(data_gzjx, data_zmjx, data_pjjx)

head(combined_data)

overlap_plot <- ggplot(combined_data, aes(x = gzjx, y = yhat, group = variable, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "#FF8C00", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.8, size = 0.7) +
  labs(x = "Hour(h)", y = "OR(95%CI)", 
       title = "RCS curves depicting the relationship between wake time and depression risk",
       color = NULL, fill = NULL) +
  scale_color_manual(values = c("gzjx" = "steelblue", "zmjx" = "gold", "pjjx" = "#555555"),
                     labels = c("gzjx" = "Weekday wake time", 
                                "zmjx" = "Weekend wake time", 
                                "pjjx" = "Average wake time")) +
  scale_fill_manual(values = c("gzjx" = "steelblue", "zmjx" = "gold", "pjjx" = "#555555"),
                    labels = c("gzjx" = "Weekday wake time", 
                               "zmjx" = "Weekend wake time", 
                               "pjjx" = "Average wake time")) +
  scale_x_continuous(breaks = seq(floor(min(combined_data$gzjx)), 
                                  ceiling(max(combined_data$gzjx)), 
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