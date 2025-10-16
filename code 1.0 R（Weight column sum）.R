library(survey)  


file_path <- "D:\\Supplementary file\\2017-2023R data 11059_1.csv"
data <- read.csv(file_path)  


design <- svydesign(
  id = ~SDMVPSU,          
  strata = ~SDMVSTRA,     
  weights = ~mecx,        
  data = data,
  nest = TRUE             
)


sum_weights <- sum(data$mecx, na.rm = TRUE)

print(paste("Weight column sum：", sum_weights))

weighted_mean_age <- svymean(~RIDAGEYR, design, na.rm = TRUE)


print("Weighted mean age and its standard error")
print(weighted_mean_age)

weighted_depression <- svymean(~sfyy, design, na.rm = TRUE)


print("Weighted prevalence of depression and standard error：")
print(weighted_depression)

confint_weighted <- confint(weighted_depression, level = 0.95)
print("The 95% confidence interval for the weighted prevalence：")
print(confint_weighted)