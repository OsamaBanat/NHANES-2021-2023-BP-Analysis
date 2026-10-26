
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Constants ==== 
####################################################
# Weighted t-test                         line 13
# Weighted ANOVA                          line 84
# Descriptive plots for categorical data  line 251
####################################################

# ==== Weighted t-test ==== 

# Create a function to perform t-test and return results as a table
wt_ttest <- function(var) {
   
   Var_label = lookup [[var]]
   BP <- c("SBP","DBP")
   
   # Create an empty table
   ttest_table <- tibble()
   
   # Loop to apply function on SBP and DBP
   for (x in BP) {
      
      # Create a formula
      formula <- as.formula(
         paste0(x, "_avg~", var)
      )
      
      # Weighted t-test
      ttest <- svyttest(formula, design = NHANES_design)
      
      # Create a table
      ttest_table <- ttest_table %>% 
         bind_rows(
            tibble(
               Variable = Var_label,
               Outcome = x,
               BP_difference = sprintf(
                  "%.3f (%.3f - %.3f)",
                  as.numeric(ttest$estimate),
                  as.numeric(ttest$conf.int [1]),
                  as.numeric(ttest$conf.int [2])
               ), 
               p_value = as.numeric(ttest$p.value)
            )
         )
   }
   
   ttest_table
}

# Apply function
t_table <- wt_ttest ("Insurance") %>%
   gt() %>%
   fmt(
      columns = p_value,
      fns = p0001)%>%
   cols_label(
      Variable = "Variable",
      Outcome  = "Outcome",
      BP_difference = "BP difference",
      p_value  = "P-value"
   )%>%
   tab_header(
      title = md(
         "Weighted mean differences in SBP and DBP by binary subgroups"
      )
   ) %>%
   tab_footnote(
      footnote = "P-values are from survey-adjusted t-test comparing binary subgroups.",
      locations = cells_column_labels(columns = p_value)
   ) %>%
   tab_source_note(
      source_note = md(
         "All estimates are survey-weighted."
         )
   )

t_table

# ==== Weighted ANOVA ==== 

# Create a function to perform ANOVA (plus post-hoc) and return results as a table
wt_ANOVA <- function(BP, vars) {
   
   # Create empty table
   Results <- tibble()
   
   for (var in vars) {
      
      Var_label <- lookup [[var]]
      
      # Create formulas
      formula1 <- as.formula(
         paste0(BP, "_avg~", var)
      )
      
      formula2 <- as.formula(
         paste0("~", var)
      )
      
      # Weighted ANOVA
      Model <- svyglm(formula1, design = NHANES_design)
      
      # Estimated marginal means
      Emm <- emmeans(Model, formula2)
      
      # Tukey-adjusted pairwise comparisons
      Pair <- pairs(Emm, adjust = "tukey")
      
      # Overall P-value 
      ANV <- regTermTest(Model, formula2)
      
      # Create separated tables
      table_1 <- tibble (
         contrast = Var_label,
         overall = as.numeric(ANV$p),
         B = NA,
         adj.p.value =NA
      )
      
      if (ANV$p < 0.05) {
         
         table_2 <- tidy(Pair, conf.int = TRUE) %>% 
            mutate(
               B = sprintf(
                  "%.3f (%.3f - %.3f)",
                  estimate,
                  conf.low,
                  conf.high
               ),
               overall = NA
            ) %>% 
            select(c(contrast, overall, B, adj.p.value))
         
      } else {
         
         table_2 <- tibble()
      }
      
      # Combine tables as one
      Results <- bind_rows(Results, table_1, table_2)
   }
   
   # Retrun results in a tidy way
   Results %>% 
      gt() %>%
      fmt(
         columns = c(overall, adj.p.value),
         fns = p0001
      ) %>%
      cols_label(
         contrast = "Variable",
         overall  = "Overall p-value",
         B        = "Mean difference (95% CI)",
         adj.p.value = "Tukey-adjusted P-value"
      ) %>%
      sub_missing(
         columns = c(B, overall, adj.p.value),
         missing_text = ""
      ) %>%
      tab_style(
         style = cell_text(indent = px(15)),
         locations = cells_body(
            columns = contrast,
            rows = !contrast %in% c(
               "Race",
               "Marital status",
               "Education level",
               "Diabetes",
               "BP-lowering medications"
            )
         )
      ) %>%
      tab_style(
         style = cell_borders(
            sides = c("bottom", "left", "right"),
            color = "transparent",
            weight = px(0)
         ),
         locations = cells_body(
            rows = contrast %in% c(
               "Race",
               "Marital status",
               "Education level",
               "Diabetes",
               "BP-lowering medication"
            )
         )
      ) %>%
      tab_footnote(
         footnote = "Overall p-values compare all subgroups using survey-weighted ANOVA.",
         locations = cells_column_labels(columns = overall)
      )  %>%
      tab_footnote(
         footnote = " Tukey-adjusted pairwise p-values based on comparisons between subgroups.",
         locations = cells_column_labels(columns = adj.p.value)
      )
}

# Apply function for SBP
ANOVA_SBP <- wt_ANOVA(
   "SBP",
   c(
      "Race",
      "Marital_status",
      "Education",
      "Diabetes",
      "Taking_BP_meds"
   )
) %>%
   tab_header(
      title = md(
         "Table 3: Survey-weighted comparisons of non-binary categorical variables for SBP"
      )
   )

ANOVA_SBP

# Apply function for DBP
ANOVA_DBP <- wt_ANOVA(
   "DBP",
   c(
      "Race",
      "Marital_status",
      "Education",
      "Diabetes",
      "Taking_BP_meds"
   )
) %>%
   tab_header(
      title = md(
         "Table 4: Survey-weighted comparisons of non-binary categorical variables for DBP"
      )
   )

ANOVA_DBP

# Get mean and CI of specific race subgroup
svymean(~SBP_avg, 
        subset(NHANES_design, Race == "Non Hispanic Black"),
        na.rm = TRUE)

confint(svymean(~SBP_avg, 
                subset(NHANES_design, Race == "Non Hispanic Black"),
                na.rm = TRUE))

# ==== Descriptive plots for categorical data ==== 

# Create a function to plot weighted BP mean (with 95% CI) across covariates groups
plot_categorical <- function(var) {
   
   SBP_formula <- as.formula("~SBP_avg")
   
   DBP_formula <- as.formula("~DBP_avg")
   
   var_formula <- as.formula(
      paste0("~", var)
   )
   
   SBP_by <- svyby(
      SBP_formula, 
      var_formula,
      design = NHANES_design,
      FUN = svymean,
      na.rm = TRUE
   )
   
   DBP_by <- svyby(
      DBP_formula,
      var_formula,
      design = NHANES_design,
      FUN = svymean,
      na.rm = TRUE
   )
   
   # Convert SE into 95% CI
   SBP_by <- SBP_by %>%
      rename(BP_avg = SBP_avg) %>%
      mutate(
         CI_lower = BP_avg - 1.96 * se,
         CI_upper = BP_avg + 1.96 * se,
         BP_type = "SBP"
      )
   
   DBP_by <- DBP_by %>%
      rename(BP_avg = DBP_avg) %>%
      mutate(
         CI_lower = BP_avg - 1.96 * se,
         CI_upper = BP_avg + 1.96 * se,
         BP_type = "DBP"
      )
   
   # Combine DBP and SBP
   BP_by <- bind_rows(SBP_by, DBP_by)
   
   # Plot  BP weighted means with CI across var groups
   Var_label = lookup [[var]]
   
   ggplot(
      BP_by, 
      aes(x = .data[[var]], y = BP_avg)
   ) +
      geom_point() +
      geom_errorbar(
         aes(
            ymin = CI_lower, 
            ymax = CI_upper
         ),
         width = 0.15
      ) +
      facet_wrap(
         ~BP_type, 
         scales = "free_y"
      ) +
      labs(
         title = paste0("Survey-weighted mean BP by ", Var_label, " (with 95% CI)"),
         x = Var_label,
         y = "Mean BP (mmHg)"
      ) +
      theme(
         axis.text.x = element_text(
            angle = 45,
            hjust = 1
         )
      )
}

# Apply function
map(
   c("Sex", "Insurance", "Taking_BP_meds", "Race", "Marital_status", "Education", "Diabetes"),
   plot_categorical
)

