
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Survey-weighted design ==== 

# 1. Create survey-weighted design 
NHANES_design <- svydesign(
   ids = ~ PSU,
   strata = ~ Strata,
   weights = ~ MEC_weight ,
   nest = TRUE,
   data = Data
)

# 2. Survey-weighted SBP histogram
svyhist (
   ~SBP_avg, 
   NHANES_design,
   col = "lightblue",
   border = "black",
   main = "Distribution of Mean Systolic Blood Pressure (Survey-weighted)",
   x = "SBP (mmHg)"
)

# 3. Survey-weighted SBP Q-Q plot
svyqqmath(
   ~SBP_avg, 
   NHANES_design,
   main = "Q-Q Plot of Mean Systolic Blood Pressure (Survey-weighted)",
   xlab = "Theoretical quantiles",
   ylab = "Weighted observed quantiles"
)

# 4. Survey-weighted DBP histogram
svyhist (
   ~DBP_avg, 
   NHANES_design,
   col = "lightblue",
   border = "black",
   main = "Distribution of Mean Diastolic Blood Pressure (Survey-weighted)",
   x = "DBP (mmHg)"
)

# 5. Survey-weighted DBP Q-Q plot
svyqqmath(
   ~DBP_avg, 
   NHANES_design,
   main = "Q-Q Plot of Mean Diastolic Blood Pressure (Survey-weighted)",
   xlab = "Theoretical quantiles",
   ylab = "Weighted observed quantiles"
)