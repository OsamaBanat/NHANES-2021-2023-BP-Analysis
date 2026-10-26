
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Log variables ==== 

## ==== 1. Create log data ====
Data <- Data %>% 
   mutate (ACR_log = log10(ACR))

Data <- Data %>% 
   mutate (eGFR_log = log10(eGFR))

# Add labels to the look-up table
lookup <- c(
   lookup,
   ACR_log  = "Log (ACR)",
   eGFR_log = "Log (eGFR)"
)

## ==== 2. Check on log data ====

# Create a for loop to plot histograms and boxplots, and calculate summaries
# for each column

Exploring_log <- function(vars) {
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
                                sd = sd(Data[[var]], na.rm = TRUE)),
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
Exploring_log(
   c("ACR_log", "eGFR_log")
)

