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

model_gzrs_nonobese <- svyglm(
  sfyy ~ rcs(gzrs, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_nonobese,
  family = quasibinomial()
)

model_zmrs_nonobese <- svyglm(
  sfyy ~ rcs(zmrs, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_nonobese,
  family = quasibinomial()
)

model_pjrs_nonobese <- svyglm(
  sfyy ~ rcs(pjrs, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_nonobese,
  family = quasibinomial()
)

model_gzrs_obese <- svyglm(
  sfyy ~ rcs(gzrs, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_obese,
  family = quasibinomial()
)

model_zmrs_obese <- svyglm(
  sfyy ~ rcs(zmrs, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_obese,
  family = quasibinomial()
)

model_pjrs_obese <- svyglm(
  sfyy ~ rcs(pjrs, 4) + RIDAGEYR + yjfd + RIAGENDR + RIDRETH1 + 
    DMDEDUC2 + DMDMARTZ + jtpir + gzqk + BPQ020 + tnbqk + xyqk + hdqkefl,
  design = design_obese,
  family = quasibinomial()
)

rcs_gzrs_nonobese <- svyggrcs(model_gzrs_nonobese)
rcs_zmrs_nonobese <- svyggrcs(model_zmrs_nonobese)
rcs_pjrs_nonobese <- svyggrcs(model_pjrs_nonobese)

rcs_gzrs_obese <- svyggrcs(model_gzrs_obese)
rcs_zmrs_obese <- svyggrcs(model_zmrs_obese)
rcs_pjrs_obese <- svyggrcs(model_pjrs_obese)

        extract_data <- function(rcs_obj, variable_name, group_name) {
  data <- rcs_obj[["newdat"]]
  data$variable <- variable_name
  data$group <- group_name
  
  var_col_name <- names(data)[1]
  names(data)[1] <- "sleep_time"  # Unified name: sleep_time
  
  return(data)
}

data_gzrs_nonobese <- extract_data(rcs_gzrs_nonobese, "gzrs", "Non-obese group")
data_zmrs_nonobese <- extract_data(rcs_zmrs_nonobese, "zmrs", "Non-obese group")
data_pjrs_nonobese <- extract_data(rcs_pjrs_nonobese, "pjrs", "Non-obese group")

data_gzrs_obese <- extract_data(rcs_gzrs_obese, "gzrs", "Obese group")
data_zmrs_obese <- extract_data(rcs_zmrs_obese, "zmrs", "Obese group")
data_pjrs_obese <- extract_data(rcs_pjrs_obese, "pjrs", "Obese group")

combined_data <- rbind(
  data_gzrs_nonobese, data_zmrs_nonobese, data_pjrs_nonobese,
  data_gzrs_obese, data_zmrs_obese, data_pjrs_obese
)

head(combined_data)
str(combined_data)

plot_nonobese <- ggplot(combined_data %>% filter(group == "Non-obese group"), 
                        aes(x = sleep_time, y = yhat, group = variable, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  # Add red vertical dashed line at X=22
  geom_vline(xintercept = 22, linetype = "dashed", color = "#FF8C00", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.8, size = 0.7) +
  labs(x = "Hour(h)", y = "OR(95%CI)", 
       title = "Non-obese group:",
       color = NULL, fill = NULL) +
  scale_color_manual(
    values = c("gzrs" = "sienna", "zmrs" = "mediumpurple", "pjrs" = "#888888"),
    labels = c("gzrs" = "Weekday sleep onset time", "zmrs" = "Weekend sleep onset time", "pjrs" = "Average sleep onset time")
  ) +
  scale_fill_manual(
    values = c("gzrs" = "sienna", "zmrs" = "mediumpurple", "pjrs" = "#888888"),
    labels = c("gzrs" = "Weekday sleep onset time", "zmrs" = "Weekend sleep onset time", "pjrs" = "Average sleep onset time")
  ) +
  scale_x_continuous(breaks = 0:23) +
  theme_classic() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot_obese <- ggplot(combined_data %>% filter(group == "Obese group"), 
                     aes(x = sleep_time, y = yhat, group = variable, color = variable)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  
  geom_vline(xintercept = 22, linetype = "dashed", color = "#FF8C00", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", alpha = 0.8, size = 0.7) +
  labs(x = "Hour(h)", y = "OR(95%CI)", 
       title = "Obese group:",
       color = NULL, fill = NULL) +
  scale_color_manual(
    values = c("gzrs" = "sienna", "zmrs" = "mediumpurple", "pjrs" = "#888888"),
    labels = c("gzrs" = "Weekday sleep onset time", "zmrs" = "Weekend sleep onset time", "pjrs" = "Average sleep onset time")
  ) +
  scale_fill_manual(
    values = c("gzrs" = "sienna", "zmrs" = "mediumpurple", "pjrs" = "#888888"),
    labels = c("gzrs" = "Weekday sleep onset time", "zmrs" = "Weekend sleep onset time", "pjrs" = "Average sleep onset time")
  ) +
  
  scale_x_continuous(breaks = 0:23) +
  theme_classic() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- plot_grid(plot_nonobese, plot_obese, ncol = 2, labels = "AUTO")


title <- ggdraw() + 
  draw_label("RCS curves depicting the relationship between sleep onset time and depression risk, stratified by obesity status",
             fontface = 'bold', size = 14)


final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 0.9))


print(final_plot)


legend_plot <- ggplot() +
  geom_vline(xintercept = 22, linetype = "dashed", color = "#FF8C00", size = 1) +
  xlim(0, 1) + ylim(0, 1) +
  annotate("text", x = 0.2, y = 0.8, label = "Red line: X = 22 (Reference line)", hjust = 0) +
  annotate("text", x = 0.2, y = 0.6, label = "Black line: Y = 1 (Reference OR)", hjust = 0) +
  theme_void()