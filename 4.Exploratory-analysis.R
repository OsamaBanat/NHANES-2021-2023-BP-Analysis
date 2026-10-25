
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Constants ==== 
####################################################
# Explore outcomes                    line 12
# Explore covariates                  line 132
####################################################

# ==== Explore outcomes ====

## ==== 1. Create look-up table ====
lookup <- list(
   Sex  = "Sex",
   Age  = "Age",
   Race = "Race/ethnicity",
   Marital_status = "Marital status",
   IPR        = "IPR",
   Education  = "Education level",
   Systolic1  = "SBP: First measurment",
   Systolic2  = "SBP: Second measurment",
   Systolic3  = "SBP: Third measurment",
   Diastolic1 = "DBP: First measurment",
   Diastolic2 = "DBP: Second measurment",
   Diastolic3 = "DBP: Third measurment",
   Pulse1     = "Pulse rate: First measurment",
   Pulse2     = "Pulse rate: Second measurment",
   Pulse3     = "Pulse rate: Third measurment",
   Insurance  = "Insurance status",
   WC  = "WC",
   BMI = "BMI",
   SCr = "Serum Creatinine",
   ACR = "ACR",
   HbA1c     = "HbA1c",
   Diabetes  = "Diabetes status",
   Taking_BP_meds = "BP-lowering medications",
   SBP_avg = "Mean SBP",
   DBP_avg = "Mean DBP",
   Pulse_avg = "Mean pulse rate",
   eGFR = "eGFR"
)

## ==== 2. Distribution of BP outcomes ====

for (BP in c("SBP_avg", "DBP_avg")) {
   
   # Histogram 
   print(
      ggplot(
         Data, 
         aes(x = .data[[BP]])
      ) +
         geom_histogram(
            aes(y = after_stat(density)),
            color = "black", 
            fill = "lightblue"
         ) +
         stat_function(
            fun = dnorm,
            args = list(
               mean = mean(Data[[BP]], na.rm = TRUE), 
               sd= sd(Data[[BP]], na.rm = TRUE)
            ), 
            color = "red", 
            linewidth = 1
         ) +
         labs(
            title = paste(
               "Distribution of",
               lookup[[BP]],
               "(unweighted)"
            ), 
            y = "Density",
            x = paste(
               lookup[[BP]],
               "(mmHg)"
            ) 
         )
   )
   
   # Boxplot
   print(
      ggplot(
      Data, 
      aes(y = .data[[BP]])
      ) + 
      geom_boxplot(fill = "lightblue") + 
      labs(
         title = paste(
            "Boxplot of",
            lookup[[BP]],
            "(unweighted)"
         ),
         y = paste(
            lookup[[BP]],
            "(mmHg)"
         ),
         x = ""
      )
   )
   
   # QQ Plot
   print(
      ggplot(
      Data, 
      aes(sample = .data[[BP]])
   ) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(
         title = paste(
            "Q-Q plot of",
            lookup[[BP]],
            "(unweighted)"
         ),
         x="Theoretical quantiles",
         y="Observed quantiles"
         )
   )
   
   # Descriptive summary
   cat("\n", "Outcome:", lookup[[BP]], "\n")
   print(summary(Data[[BP]]))
   
   # Anderson-Darling normality test
   print(ad.test(Data[[BP]]))

}

# ==== Explore covariates ====

## ==== 1. Distribution of covariates ====

# Create a for loop to plot histograms and boxplots, and calculate summaries
# for each column
Exploring <- function(vars) {
   for (var in vars) {
      
      Var_label = lookup[[var]]
      
      # Histogram with normal distribution curve
      Histogram <- ggplot(
         Data, 
         aes(x = .data[[var]])
      ) +
         geom_histogram(
            aes(y = after_stat(density)),
            bins = 30,
            color = "black",
            fill = "lightblue"
         ) +
         stat_function(
            fun = dnorm,
            args = list(
               mean = mean(Data[[var]], na.rm = TRUE),
               sd = sd(Data[[var]], na.rm = TRUE)
            ),
            color = "red",
            linewidth = 1
         ) +
         labs(
            title = paste("Histogram of", Var_label),
            y = "Density",
            x = Var_label
         ) 
      
      print(Histogram)
      
      # Boxplot
      Box_plot <- ggplot(
         Data, 
         aes(y = .data[[var]])
      ) +
         geom_boxplot(fill = "lightblue") +
         labs(
            title = paste("Boxplot of", Var_label),
            y = Var_label,
            x = ""
         ) 
      
      print(Box_plot)
      
      # Summary statistics
      cat("\n", "Variable:", Var_label, "\n")
      print(summary(Data[[var]]))
   }
}  

# Apply function
Exploring(
   c(
      "Systolic1", "Systolic2", "Systolic3", "Diastolic1", "Diastolic2", 
      "Diastolic3", "Pulse1", "Pulse2", "Pulse3", "Age", "IPR", 
      "WC", "BMI", "HbA1c", "ACR", "SCr",  "eGFR"
   )
)

## ==== 2. Explore continuous covariates relationship with outcomes ====

# Create a for loop to plot BP with continuous covariates
Exploring2_cont <- function(vars) {
   for (var in vars) {
      
      Var_label = lookup[[var]]
      for (BP in c("SBP_avg", "DBP_avg")) {
         
      # Histogram with normal distribution curve
      print (
         ggplot(
            Data,
            aes(
               x = .data[[var]],
               y = .data[[BP]]
            )
         ) +
            geom_point(
               alpha = 0.4,
               color = "steelblue"
            ) +
            geom_smooth(
               method = "lm",
               formula = y ~ x,
               color = "red",
               se = TRUE
            ) +
            geom_smooth(
               method = "loess",
               formula = y ~ x,
               color = "blue",
               linetype = "dashed",
               se = FALSE
            ) +
            labs(
               title = paste(
                  lookup[[BP]],
                  "across",
                  Var_label
               ),
               subtitle = "Linear regression (red) and LOESS smooth (blue)",
               x = Var_label,
               y = paste(
                  lookup[[BP]],
                  "(mmHg)"
                  )
            )
      )
      }
   }
}   

# Apply function
Exploring2_cont(
   c(
      "Age", "IPR", "WC", "BMI", "HbA1c", "ACR", "SCr",  "eGFR", "Pulse_avg"
   )
)

## ==== 3. Explore categorical covariates relationship with outcomes ====

# Create a for loop to plot BP with categorical covariates
Exploring2_cate <- function(vars) {
   for (var in vars) {
      
      Var_label = lookup[[var]]
      for (BP in c("SBP_avg", "DBP_avg")) {
         
         # Histogram with normal distribution curve
         print (
            ggplot(
               Data,
               aes(
                  x = .data[[var]],
                  y = .data[[BP]]
               )
            ) +
               geom_boxplot(
                  fill = "lightblue",
                  color = "black",
                  outlier.alpha = 0.4
               ) +
               labs(
                  title = paste(
                     lookup[[BP]],
                     "across",
                     Var_label
                  ),
                  x = Var_label,
                  y = paste(
                     lookup[[BP]],
                     "(mmHg)"
                     )
               ) +
               theme(
                  axis.text.x = element_text(
                     angle = 45,
                     hjust = 1
                  )
               )
         )
      }
   }
}

# Apply function
Exploring2_cate(
   c(
      "Sex", "Race", "Insurance", "Education", "Diabetes", "Marital_status", "Taking_BP_meds"
   )
)
