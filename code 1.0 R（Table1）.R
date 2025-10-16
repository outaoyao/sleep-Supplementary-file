library(tidyverse)
library(tableone)
library(survey)
data <- read.csv("D:\\Supplementary file\\2017-2023R data 11059_1.csv", stringsAsFactors = FALSE)

data$sfyy <- factor(data$sfyy, levels = c(0, 1), labels = c("non-depression", "depression"))

data$bmifd <- factor(data$bmifd, levels = c(0, 1, 2, 3),
                     labels = c("18.5-24.9kg/m²", "<18.5kg/m²", "25-29.9kg/m²", "≥30kg/m²"))

data$yjfd <- factor(data$yjfd, levels = c(1, 2), labels = c("Non-drinking", "Drinking"))

data$RIAGENDR <- factor(data$RIAGENDR, levels = c(0, 1), labels = c("Female", "Male"))

data$nlfd <- factor(data$nlfd, levels = c(1, 2, 3), 
                    labels = c("20-39 years (Young adulthood)", "40-59 years (Middle age)", "60 years and above"))

data$RIDRETH1 <- factor(data$RIDRETH1, levels = c(1, 2, 3, 4, 5),
                        labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", 
                                   "Non-Hispanic Black", "Other Race"))

data$DMDEDUC2 <- factor(data$DMDEDUC2, levels = c(0, 1, 2),
                        labels = c("Less than high school", "High school graduate", "Some college or above"))

data$DMDMARTZ <- factor(data$DMDMARTZ, levels = c(1, 2, 3),
                        labels = c("Married/Living with partner", "Widowed/Divorced/Separated", "Never married"))

data$jtpir <- factor(data$jtpir, levels = c(0, 1, 2),
                     labels = c("1.30-3.49", "<1.30", "≥3.50"))

data$gzqk <- factor(data$gzqk, levels = c(0, 1),
                    labels = c("Has job or business", "No job or business"))

data$BPQ020 <- factor(data$BPQ020, levels = c(0, 1),
                      labels = c("No", "Yes"))

data$tnbqk <- factor(data$tnbqk, levels = c(0, 1, 2),
                     labels = c("No diabetes", "Diabetes", "Prediabetes"))

data$xyqk <- factor(data$xyqk, levels = c(1, 2),
                    labels = c("Non-smoking", "Smoking"))

data$hdqkefl <- factor(data$hdqkefl, levels = c(0, 1),
                       labels = c("Light recreational activity", "Moderate/Vigorous recreational activity"))

data$smjlfl <- factor(data$smjlfl, levels = c(0, 1, 2, 3),
                      labels = c("No disorder", "Mild disorder", "Moderate disorder", "Severe disorder"))

data$gzsmsj <- factor(data$gzsmsj, levels = c(0, 1, 2),
                      labels = c("Normal sleep duration", "Insufficient sleep", "Excessive sleep"))

data$zmsmsj <- factor(data$zmsmsj, levels = c(0, 1, 2),
                      labels = c("Normal sleep duration", "Insufficient sleep", "Excessive sleep"))

data$smjzfd <- factor(data$smjzfd, levels = c(0, 1, 2),
                      labels = c("Normal sleep duration", "Insufficient sleep", "Excessive sleep"))

all_vars <- c("bmifd", "yjfd", "RIAGENDR", "nlfd", "RIDRETH1", "DMDEDUC2", 
              "DMDMARTZ", "jtpir", "gzqk", "BPQ020", "tnbqk", "xyqk", 
              "hdqkefl", "smjlfl", "gzsmsj", "zmsmsj", "smjzfd")

overall_counts <- list()

for (var in all_vars) {
  overall_counts[[var]] <- table(data[[var]], useNA = "ifany")
}

print(overall_counts)

stratified_counts <- list()

for (var in all_vars) {
  stratified_counts[[var]] <- table(data$sfyy, data[[var]], useNA = "ifany")
}

print(stratified_counts)
nhanes_design <- svydesign(
  id = ~SDMVPSU,        
  strata = ~SDMVSTRA,    
  weights = ~mecx,      
  nest = TRUE,           
  data = data
)


overall_weighted <- list()
for (var in all_vars) {
  overall_weighted[[var]] <- svymean(~get(var), design = nhanes_design, na.rm = TRUE)
}

print(overall_weighted)


stratified_weighted <- list()
for (var in all_vars) {
  formula <- as.formula(paste("~", var))
  stratified_weighted[[var]] <- svyby(formula, ~sfyy, design = nhanes_design, svymean, na.rm = TRUE)
}

print(stratified_weighted)