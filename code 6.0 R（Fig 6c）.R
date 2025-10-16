library(survey)
library(rms)
library(ggplot2)
library(cowplot)
library(dplyr)


source("D:/Supplementary file/svyggrcs1.8R.R")


data_path <- "D:\\Supplementary file\\2017-2023R data 11059_1.csv"
nhanes_data <- read.csv(data_path)


nhanes_data$sfyy <- factor(nhanes_data$sfyy, levels = c(0, 1), labels = c("non-depression", "depression"))
nhanes_data$sffp <- factor(nhanes_data$sffp, levels = c(0, 1), labels = c("Non-obese group", "Obese group"))
nhanes_data$yjfd <- factor(nhanes_data$yjfd, levels = c(1, 2), labels = c("Non-drinker", "Drinker"))
nhanes_data$RIAGENDR <- factor(nhanes_data$RIAGENDR, levels = c(0, 1), labels = c("Female", "Male"))
nhanes_data$RIDRETH1 <- factor(nhanes_data$RIDRETH1, levels = 1:5,
                               labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", 
                                          "Non-Hispanic Black", "Other Race"))
nhanes_data$DMDEDUC2 <- factor(nhanes_data$DMDEDUC2, levels = c(0, 1, 2),
                               labels = c("Less than high school", "High school graduate or equivalent", "Some college or above"))
nhanes_data$DMDMARTZ <- factor(nhanes_data$DMDMARTZ, levels = 1:3,
                               labels = c("Married/Living with partner", "Widowed/Divorced/Separated", "Never married"))
nhanes_data$jtpir <- factor(nhanes_data$jtpir, levels = c(0, 1, 2),
                            labels = c("1.30-3.49", "<1.30", "≥3.50"))
nhanes_data$gzqk <- factor(nhanes_data$gzqk, levels = c(0, 1),
                           labels = c("Has job or business", "No job or business"))
nhanes_data$BPQ020 <- factor(nhanes_data$BPQ020, levels = c(0, 1),
                             labels = c("No", "Yes"))
nhanes_data$tnbqk <- factor(nhanes_data$tnbqk, levels = c(0, 1, 2),
                            labels = c("No diabetes", "Has diabetes", "Pre-diabetes"))
nhanes_data$xyqk <- factor(nhanes_data$xyqk, levels = c(1, 2),
                           labels = c("Non-smoker", "Current smoker"))
nhanes_data$hdqkefl <- factor(nhanes_data$hdqkefl, levels = c(0, 1),
                              labels = c("Mild recreational activity", "Moderate/Vigorous recreational activity"))


design <- svydesign(
  id = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  weights = ~mecx, 
  data = nhanes_data, 
  nest = TRUE
)


dd <- datadist(nhanes_data)
options(datadist = "dd")


design_nonobese <- subset(design, sffp == "Non-obese group")
design_obese <- subset(design, sffp == "Obese group")


model_gzzd_nonobese <- svyglm(
  sfyy ~ rcs(gzzd, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_nonobese,
  family = quasibinomial()
)

model_zmzd_nonobese <- svyglm(
  sfyy ~ rcs(zmzd, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_nonobese,
  family = quasibinomial()
)

model_pjsmzd_nonobese <- svyglm(
  sfyy ~ rcs(pjsmzd, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_nonobese,
  family = quasibinomial()
)


model_gzzd_obese <- svyglm(
  sfyy ~ rcs(gzzd, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_obese,
  family = quasibinomial()
)

model_zmzd_obese <- svyglm(
  sfyy ~ rcs(zmzd, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_obese,
  family = quasibinomial()
)

model_pjsmzd_obese <- svyglm(
  sfyy ~ rcs(pjsmzd, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_obese,
  family = quasibinomial()
)


rcs_gzzd_nonobese <- svyggrcs(model_gzzd_nonobese)
rcs_zmzd_nonobese <- svyggrcs(model_zmzd_nonobese)
rcs_pjsmzd_nonobese <- svyggrcs(model_pjsmzd_nonobese)


rcs_gzzd_obese <- svyggrcs(model_gzzd_obese)
rcs_zmzd_obese <- svyggrcs(model_zmzd_obese)
rcs_pjsmzd_obese <- svyggrcs(model_pjsmzd_obese)


extract_data <- function(rcs_obj, variable_name, group_name) {
  data <- rcs_obj[["newdat"]]
  data$variable <- variable_name
  data$group <- group_name
  
  
  var_col_name <- names(data)[1]
  names(data)[1] <- "sleep_time"  # Unified name: sleep_time
  
  return(data)
}


data_gzzd_nonobese <- extract_data(rcs_gzzd_nonobese, "gzzd", "Non-obese group")
data_zmzd_nonobese <- extract_data(rcs_zmzd_nonobese, "zmzd", "Non-obese group")
data_pjsmzd_nonobese <- extract_data(rcs_pjsmzd_nonobese, "pjsmzd", "Non-obese group")


data_gzzd_obese <- extract_data(rcs_gzzd_obese, "gzzd", "Obese group")
data_zmzd_obese <- extract_data(rcs_zmzd_obese, "zmzd", "Obese group")
data_pjsmzd_obese <- extract_data(rcs_pjsmzd_obese, "pjsmzd", "Obese group")


combined_data <- rbind(
  data_gzzd_nonobese, data_zmzd_nonobese, data_pjsmzd_nonobese,
  data_gzzd_obese, data_zmzd_obese, data_pjsmzd_obese
)


head(combined_data)
str(combined_data)


plot_nonobese <- ggplot(combined_data %>% filter(group == "Non-obese group"), 
                        aes(x = sleep_time, y = yhat, group = variable, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  # Add red vertical dashed line at X=2
  geom_vline(xintercept = 2, linetype = "dashed", color = "#FF8C00", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.8, size = 0.7) +
  labs(x = "Hour(h)", y = "OR(95%CI)", 
       title = "Non-obese group:",
       color = NULL, fill = NULL) +
  scale_color_manual(
    values = c("gzzd" = "#7B68EE", "zmzd" = "#9932CC", "pjsmzd" = "#BBBBBB"),
    labels = c("gzzd" = "Weekday sleep midpoint", "zmzd" = "Weekend sleep midpoint", "pjsmzd" = "Average sleep midpoint")
  ) +
  scale_fill_manual(
    values = c("gzzd" = "#7B68EE", "zmzd" = "#9932CC", "pjsmzd" = "#BBBBBB"),
    labels = c("gzzd" = "Weekday sleep midpoint", "zmzd" = "Weekend sleep midpoint", "pjsmzd" = "Average sleep midpoint")
  ) +
  
  scale_x_continuous(breaks = 0:23) +
  theme_classic() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot_obese <- ggplot(combined_data %>% filter(group == "Obese group"), 
                     aes(x = sleep_time, y = yhat, group = variable, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  
  geom_vline(xintercept = 2, linetype = "dashed", color = "#FF8C00", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.8, size = 0.7) +
  labs(x = "Hour(h)", y = "OR(95%CI)", 
       title = "Obese group:",
       color = NULL, fill = NULL) +
  scale_color_manual(
    values = c("gzzd" = "#7B68EE", "zmzd" = "#9932CC", "pjsmzd" = "#BBBBBB"),
    labels = c("gzzd" = "Weekday sleep midpoint", "zmzd" = "Weekend sleep midpoint", "pjsmzd" = "Average sleep midpoint")
  ) +
  scale_fill_manual(
    values = c("gzzd" = "#7B68EE", "zmzd" = "#9932CC", "pjsmzd" = "#BBBBBB"),
    labels = c("gzzd" = "Weekday sleep midpoint", "zmzd" = "Weekend sleep midpoint", "pjsmzd" = "Average sleep midpoint")
  ) +
  
  scale_x_continuous(breaks = 0:23) +
  theme_classic() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


combined_plot <- plot_grid(plot_nonobese, plot_obese, ncol = 2, labels = "AUTO")


title <- ggdraw() + 
  draw_label("RCS curves depicting the relationship between sleep midpoint and depression risk, stratified by obesity status",
             fontface = 'bold', size = 14)


final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 0.9))


print(final_plot)


legend_plot <- ggplot() +
  geom_vline(xintercept = 2, linetype = "dashed", color = "#FF8C00", size = 1) +
  xlim(0, 1) + ylim(0, 1) +
  annotate("text", x = 0.2, y = 0.8, label = "Red line: X = 2 (Optimal midpoint)", hjust = 0) +
  annotate("text", x = 0.2, y = 0.6, label = "Black line: Y = 1 (Reference OR)", hjust = 0) +
  theme_void()