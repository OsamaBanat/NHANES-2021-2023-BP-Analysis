
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Survey-weighted descriptive analysis ==== 

# Check on NAs
sum(Data$MEC_weight > 0)

# Function to return p <0.001 instead of 0.000
p0001 = function(x) {
   ifelse(
      is.na(x),
      "",
      ifelse(
         x < 0.001,
         "<0.001",
         sprintf("%.3f", x)
      )
   )
}

## ==== 1. Categorical Data ====

# Create a summarizing function to calculate: mean, 95%CI, and p value
weighted_Cate_summary <- function(var) {
   
   # Extract variable data
   x <- NHANES_design$variables[[var]]
   levels_x <- levels(x)
   
   # P-value
   pp <- svychisq(
      as.formula(paste("~", var, "+ Sex")),
      design = NHANES_design
   )$p.value
   
   ## Return summary as a table
   # Results container
   table <- list()
   
   # Table first row 
   table[[1]] <- tibble(
      Variable = lookup[[var]],
      All      = "",
      Males    = "",
      Females  = "",
      p_value  = pp
   )
   
   # Loop through variable's categories
   for (i in seq_along(levels_x)) {
      
      level <- levels_x[i]
      
      # Temporary design
      Temp_design <- NHANES_design
      
      # Create a dummy
      # 1 = participant belongs to this category
      # 0 = participant does not
      # NA = original variable was missing
      Temp_design$variables$Dummy <- ifelse(is.na(x),NA,
                                            ifelse(x == level, 1, 0)
      )
      
      # Extract males and females subsets
      Males_design <- subset(
         Temp_design,
         Sex == "Male"
      )
      
      Females_design <- subset(
         Temp_design,
         Sex == "Female"
      )
      
      # Calculate weighted proportion
      All_per     <- svymean(~Dummy, design = Temp_design, na.rm = TRUE)      
      Males_per   <- svymean(~Dummy, design = Males_design, na.rm = TRUE)
      Females_per <- svymean(~Dummy, design = Females_design, na.rm = TRUE)
      
      # Table's rest rows 
      table[[i + 1]] <- tibble(
         Variable = level,
         All      = sprintf(
            "%.1f (%.1f - %.1f)",
            coef(All_per) * 100,
            confint(All_per)[, 1] * 100,
            confint(All_per)[, 2] * 100
         ),
         
         Males    = sprintf(
            "%.1f (%.1f - %.1f)",
            coef(Males_per) * 100,
            confint(Males_per)[, 1] * 100,
            confint(Males_per)[, 2] * 100
         ),
         
         Females  = sprintf(
            "%.1f (%.1f - %.1f)",
            coef(Females_per) * 100,
            confint(Females_per)[, 1] * 100,
            confint(Females_per)[, 2] * 100
         ),
         
         p_value  = NA_real_
      )
   }
   
   bind_rows(table)
}

# Apply function and print table in a tidy way
weighted_cate <- map_dfr(
   c(
      "Race", "Marital_status", "Education", "Insurance", 
      "Taking_BP_meds", "Diabetes"
   ),
   weighted_Cate_summary
)%>%
   gt() %>%
   tab_style(
      style = cell_text(indent = px(20)),
      locations = cells_body(
         rows = All != ""
      )
   ) %>%
   fmt(
      columns = p_value,
      fns = p0001) %>%
   tab_header(
      title = md(
         "Table 1: Summary of baseline categorical characteristics"
      )
   )%>%
   cols_label(
      All     = "Total Sample",
      Males   = "Males",
      Females = "Females",
      p_value = "P-value"
   ) %>%
   tab_footnote(
      footnote = "P-values compare the proportion of each covariate between males and females using survey-weighted Chi square.",
      locations = cells_column_labels(columns = p_value)
   ) %>%
   tab_source_note(
      source_note = md(
         "Percentages and 95% CIs are survey-weighted."
      )
   )

weighted_cate

# Sex percentages
svymean(~Sex, design = NHANES_design, na.rm = TRUE) *100
confint(svymean(~Sex, design = NHANES_design, na.rm = TRUE)) *100

## ==== 2. Continuous Data ====

# Create a summarizing function to calculate: mean, 95%CI, and p value
weighted_Cont_summary <- function(var){
   
   # Extract variable
   x <- NHANES_design$variables[[var]]
   
   # Extract males and females subsets
   Males_design <- subset(
      NHANES_design,
      Sex == "Male"
      )
   
   Females_design <- subset(
      NHANES_design,
      Sex == "Female"
      )
   
   # Create a formula
   formula <- as.formula(paste0("~", var))
   
   # Calculate means
   All_mean     <- svymean(formula, NHANES_design, na.rm = TRUE)
   Males_mean   <- svymean(formula, Males_design, na.rm = TRUE)
   Females_mean <- svymean(formula, Females_design, na.rm = TRUE)
   
   
   # P-value
   pp <- svyglm(
      as.formula(paste0(var, " ~ Sex")),
      design = NHANES_design
   )
   
   # Return summary as a table
   table <- tibble(
      Variable = lookup[[var]],
      All      = sprintf(
                 "%.2f (%.2f - %.2f)",
                 coef(All_mean),
                 confint(
                    All_mean,
                    df = degf(NHANES_design)
                    )[, 1],  
                 confint(
                    All_mean,
                    df = degf(NHANES_design)
                 )[, 2]
                 ),
      
      Males    = sprintf(
                 "%.2f (%.2f - %.2f)",
                 coef(
                    Males_mean,
                    df = degf(NHANES_design)
                 ),
                 confint(
                    Males_mean,
                    df = degf(NHANES_design)
                 )[, 1],  
                 confint(
                    Males_mean,
                    df = degf(NHANES_design)
                 )[, 2]
                 ),
      
      Females  = sprintf(
                 "%.2f (%.2f - %.2f)",
                 coef(Females_mean),
                 confint(
                    Females_mean,
                    df = degf(NHANES_design)
                 )[, 1],  
                 confint(
                    Females_mean,
                    df = degf(NHANES_design)
                 )[, 2]
                 ),
      
      p_value  = regTermTest(pp, ~ Sex)$p
   )
   
   table
}

# Apply function and print table in a tidy way
weighted_cont <- map_dfr(
   c(
      "SBP_avg", "DBP_avg", "Pulse_avg", "Age", "IPR", "WC",
      "BMI", "HbA1c", "ACR", "SCr", "eGFR"
      ), 
   weighted_Cont_summary
   ) %>%
   gt() %>%
   fmt(
      columns = p_value,
      fns = p0001) %>%
   tab_header(
      title = md(
         "Table 2: Summary of baseline continuous characteristics"
         )
      )%>%
   cols_label(
      All     = "Total Sample",
      Males   = "Males",
      Females = "Females",
      p_value = "P-value"
   ) %>%
   tab_footnote(
      footnote = "P-values are from survey-adjusted Wald test comparing males and females.",
      locations = cells_column_labels(columns = p_value)
   ) %>%
   tab_source_note(
      source_note = md(
         "Means are presented as survey-weighted mean (95% CIs)."
      )
   )

weighted_cont
