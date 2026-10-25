
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Constants ==== 
####################################################
# Create new design                        line 13
# Alternative model specifications         line 45
# Basic vs primary vs extended models      line 189
####################################################

# ==== Create new design ====

# Copy design
MultiM_design <- NHANES_design

# New race variable with 3 groups 
MultiM_design$variables$Race_new <- factor(
   ifelse(
      MultiM_design$variables$Race == "Non Hispanic White",
      "Non Hispanic White",
      ifelse(
         MultiM_design$variables$Race == "Non Hispanic Black",
         "Non Hispanic Black",
         "Other Racial Groups"
      )
   ),
   levels = c("Non Hispanic White","Non Hispanic Black", "Other Racial Groups")
)

# New education variable with 2 groups
MultiM_design$variables$Education_new <- factor(
   ifelse(
      MultiM_design$variables$Education %in% c(
         "Less than 9th grade",
         "9-11th grade (Includes 12th grade with no diploma)"
      ),
      "12th grade or Less",
      "High school graduate/GED or above"
   ),
   levels = c("12th grade or Less", "High school graduate/GED or above")
)

# ==== Alternative model specifications ====
Alter_mod_tab <- function(BP){
   
   # Model formulas
   Mformulas <- list(
      
      M1 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status + Education_new +
         IPR + Insurance + WC + ACR_log + HbA1c"
      ),
      
      M2 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status + Education_new +
      IPR + Insurance + WC + ACR_log + Diabetes"
      ),
      
      M3 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status +
      Education_new + IPR + Insurance +
      WC + eGFR_log + HbA1c"
      ),
      
      M4 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status +
      Education_new + IPR + Insurance +
      WC + eGFR_log + Diabetes"
      ),
      
      M5 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status +
      Education_new + IPR + Insurance +
      BMI + ACR_log + HbA1c"
      ),
      
      M6 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status +
      Education_new + IPR + Insurance +
      BMI + ACR_log + Diabetes"
      ),
      
      M7 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status +
      Education_new + IPR + Insurance +
      BMI + eGFR_log + HbA1c"
      ),
      
      M8 = paste0(
         BP, "_avg ~ Age + Sex + Race_new + Marital_status +
      Education_new + IPR + Insurance +
      BMI + eGFR_log + Diabetes"
      )
   )
   
   
   # Fitting models
   Models <- map(
      Mformulas,
      \(f) svyglm(f, design = MultiM_design)
   )
   
   # VIFs
   vifs <- map(
      Models, 
      vif
   )
   
   # Deviance
   DV <- map(
      Models, 
      deviance
   )
   
   # Return results as a table
   table <- tibble(
      Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8"),
      
      Adiposity = c(rep(c("WC", "BMI"),  each = 4)),
      
      Renal = c(rep(c("ACR","eGFR"), times = 2, each = 2)),
      
      Glycemic = c(rep(c("HbA1c","Diabetes"), times = 4)),
      
      Max_VIF = map_dbl(
         vifs,
         \(x) max(x[, "GVIF^(1/(2*Df))"])
      ),
      
      Deviance = as.numeric(DV)
   )
   
   # Present it in a tidy way
   table %>% 
      gt()  %>%
      cols_align(
         align = "center",
         columns = c(
            Adiposity,
            Renal,
            Glycemic,
            Max_VIF,
            Deviance
         )
      ) %>%
      fmt_number(
         columns = Max_VIF,
         decimals = 2
      ) %>% 
      cols_label(
         Max_VIF = "Maximum VIF"
      ) %>%
      tab_footnote(
         footnote = "ACR and eGFR were log-transformed before analysis.",
         locations = cells_column_labels(columns = "Renal")
      )  %>%
      tab_footnote(
         footnote = "HbA1c was modeled as a continuous variable, whereas diabetes status was modeled as a categorical variable (yes, no, borderline).",
         locations = cells_column_labels(columns = "Glycemic")
      ) %>%
      tab_footnote(
         footnote = "Model adjusted for age (continuous), sex (male, female), race/ethnicity (Non-Hispanic White, Non-Hispanic Black, other racial groups), marital status (never married, married/living with partner, widowed/divorced/separated), education (< or ≥12th grade), IPR (continuous), and insurance status (yes/no) plus adiposity,	renal, and glycemic variables.",
         locations = cells_column_labels(columns = "Model")
      ) 
}

# Apply function on SBP outcome
Alt_SBP <- Alter_mod_tab("SBP") %>%
   tab_header(
      title = md(
         "Table 5: Comparison between alternative model specifications for SBP"
      )
   ) 

Alt_SBP

# Apply function on DBP outcome
Alt_DBP <- Alter_mod_tab("DBP") %>%
   tab_header(
      title = md(
         "Table 6: Comparison between alternative model specifications for DBP"
      )
   ) 

Alt_DBP

# ==== Basic vs primary vs extended models ====
Coef_comp_tab <- function(BP, Adiposity, Renal){
   
   # Renal log variable
   Renal_log <- paste0(Renal, "_log")
   
   # Basic model
   Basic_vars <- c(
      "Sex", "Race_new", "Marital_status", "Education_new",  "Insurance",
      "Age", "IPR"
   )
   
   Basic_Mod <- svyglm(
      reformulate(Basic_vars, response = paste0(BP, "_avg")),
      design = MultiM_design
   )
   
   # Primary model
   Primary_vars <- c(
      Basic_vars, Adiposity , Renal_log, "HbA1c"
   )
   
   Primary_Mod <- svyglm(
      reformulate(Primary_vars, response = paste0(BP, "_avg")),
      design = MultiM_design
   )
   
   # Extended model for pulse rate
   Pulse_vars <- c(
      Primary_vars, "Pulse_avg"
   )
   
   Pulse_Mod <- svyglm(
      reformulate(Pulse_vars, response = paste0(BP, "_avg")),
      design = MultiM_design
   )
   
   # Format B (95% CI) + stars
   Format_coef <- function(model, variable) {
      
      Beta <- coef(model)[variable]
      
      CI <- confint(model)[variable, ]
      
      P <- coef(summary(model))[variable, 4]
      
      Stars <- case_when(
         P < 0.001 ~ "***",
         P < 0.01  ~ "**",
         P < 0.05  ~ "*",
         TRUE      ~ ""
      )
      
      sprintf(
         "%.3f (%.3f - %.3f)%s",
         Beta,
         CI[1],
         CI[2],
         Stars
      )
   }
   
   # Return results as table
   table <- tibble(
      Variable = c(
         "Sex", 
         "Male",
         "Female", 
         "Race/ethnicity", 
         "Non-Hispanic White",
         "Non-Hispanic Black", 
         "Other Racial Groups",
         "Marital status", 
         "Never married", 
         "Married/Living with partner", 
         "Widowed/Divorced/Separated", 
         "Education",
         "12th grade or Less",
         "High school graduate/GED or above",
         "Insurance", 
         "Not-covered", 
         "Covered",
         "Age", 
         "IPR", 
         Adiposity, 
         Renal,
         "HbA1c",
         "Mean Pulse rate"
      ),
      
      Basic = c(
         
         # Sex
         "",
         "Ref",
         Format_coef(Basic_Mod, "SexFemale"),
         
         # Race
         "", 
         "Ref",
         Format_coef(Basic_Mod, "Race_newNon Hispanic Black"),
         Format_coef(Basic_Mod, "Race_newOther Racial Groups"),
         
         # Marital status
         "", 
         "Ref",
         Format_coef(Basic_Mod, "Marital_statusMarried/Living with partner"),
         Format_coef(Basic_Mod, "Marital_statusWidowed/Divorced/Separated"),
         
         # Education
         "", 
         "Ref",
         Format_coef(Basic_Mod, "Education_newHigh school graduate/GED or above"),
         
         # Insurance
         "", 
         "Ref",
         Format_coef(Basic_Mod, "InsuranceCovered"),
         
         # Age
         Format_coef(Basic_Mod, "Age"),
         
         # IPR
         Format_coef(Basic_Mod, "IPR"),
         
         # Adiposity
         "",
         
         # Renal
         "",
         
         # HbA1c
         "",
         
         # Pulse rate
         ""
      ),
      
      Primary = c(
         
         # Sex
         "",
         "Ref",
         Format_coef(Primary_Mod, "SexFemale"),
         
         # Race
         "",
         "Ref",
         Format_coef(Primary_Mod, "Race_newNon Hispanic Black"),
         Format_coef(Primary_Mod, "Race_newOther Racial Groups"),
         
         # Marital status
         "",
         "Ref",
         Format_coef(Primary_Mod, "Marital_statusMarried/Living with partner"),
         Format_coef(Primary_Mod, "Marital_statusWidowed/Divorced/Separated"),
         
         # Education
         "",
         "Ref",
         Format_coef(Primary_Mod, "Education_newHigh school graduate/GED or above"),
         
         # Insurance
         "",
         "Ref",
         Format_coef(Primary_Mod, "InsuranceCovered"),
         
         # Age
         Format_coef(Primary_Mod, "Age"),
         
         # IPR
         Format_coef(Primary_Mod, "IPR"),
         
         # Adiposity
         Format_coef(Primary_Mod, Adiposity),
         
         # Renal
         Format_coef(Primary_Mod, Renal_log),
         
         # HbA1c
         Format_coef(Primary_Mod, "HbA1c"),
         
         # Pulse rate
         ""
      ),
      
      Pulse_Extended = c(
         
         # Sex
         "",
         "Ref",
         Format_coef(Pulse_Mod, "SexFemale"),
         
         # Race
         "",
         "Ref",
         Format_coef(Pulse_Mod, "Race_newNon Hispanic Black"),
         Format_coef(Pulse_Mod, "Race_newOther Racial Groups"),
         
         # Marital status
         "",
         "Ref",
         Format_coef(Pulse_Mod, "Marital_statusMarried/Living with partner"),
         Format_coef(Pulse_Mod, "Marital_statusWidowed/Divorced/Separated"),
         
         # Education
         "",
         "Ref",
         Format_coef(Pulse_Mod, "Education_newHigh school graduate/GED or above"),
         
         # Insurance
         "",
         "Ref",
         Format_coef(Pulse_Mod, "InsuranceCovered"),
         
         # Age
         Format_coef(Pulse_Mod, "Age"),
         
         # IPR
         Format_coef(Pulse_Mod, "IPR"),
         
         # Adiposity
         Format_coef(Pulse_Mod, Adiposity),
         
         # Renal
         Format_coef(Pulse_Mod, Renal_log),
         
         # HbA1c
         Format_coef(Pulse_Mod, "HbA1c"),
         
         # Pulse rate
         Format_coef(Pulse_Mod, "Pulse_avg")
      )
   )
   
   # Return table in a tidy way
   table %>%
      gt() %>% 
      tab_style(
         style = cell_text(indent = px(15)),
         locations = cells_body(
            columns = Variable,
            rows = Variable %in% c(
               "Male",
               "Female",
               "Non-Hispanic White",
               "Non-Hispanic Black",
               "Other Racial Groups",
               "Never married",
               "Married/Living with partner",
               "Widowed/Divorced/Separated",
               "12th grade or Less",
               "High school graduate/GED or above",
               "Not-covered",
               "Covered"
            )
         )
      ) %>%
      cols_label(
         Variable = "Variable",
         Basic = "Basic model: B (95% CI)",
         Primary = "Primary model: B (95% CI)",
         Pulse_Extended = "Pulse model: B (95% CI)"
      ) %>%
      cols_align(
         align = "center",
         columns = c(
            Basic,
            Primary,
            Pulse_Extended
         )
      ) %>%
      sub_missing(
         columns = c(Basic, Primary, Pulse_Extended),
         missing_text = ""
      ) %>%
      text_transform(
         locations = cells_body(
            columns = Basic,
            rows = Variable %in% c(Adiposity, Renal, "HbA1c")
         ),
         fn = function(x) {
            "—"
         }
      ) %>%
      text_transform(
         locations = cells_body(
            columns = c(
               Basic,
               Primary
            ),
            rows = Variable %in% "Mean Pulse rate"
         ),
         fn = function(x) {
            "—"
         }
      ) %>%
      tab_style(
         style = cell_borders(
            sides = "right",
            color = "grey60",
            weight = px(2)
         ),
         locations = cells_body(
            columns = c(
               Variable,
               Basic, 
               Primary, 
               Pulse_Extended
            )
         )
      ) %>%
      tab_style(
         style = cell_borders(
            sides = "right",
            color = "grey60",
            weight = px(2)
         ),
         locations = cells_column_labels(
            columns = c(
               Variable,
               Basic, 
               Primary, 
               Pulse_Extended
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
            rows = Variable %in% c(
               "Sex",
               "Race/ethnicity",
               "Marital status",
               "Education",
               "Insurance"
            )
         )
      ) %>%
      tab_footnote(
         footnote = "Model adjusted for age (continuous), sex (male, female), race/ethnicity (Non-Hispanic White, Non-Hispanic Black, other racial groups), marital status (never married, married/living with partner, widowed/divorced/separated), education (< or ≥12th grade), IPR (continuous), and insurance status (yes/no).",
         cells_column_labels(columns = "Basic")
      ) %>%
      tab_footnote(
         footnote = paste0(
            "Model adjusted for all variables in the basic model plus, ", Adiposity," (continuous), ", Renal, " (continuous), and HbA1c (continuous)."
         ),
         cells_column_labels(columns = "Primary")
      ) %>%
      tab_footnote(
         footnote = "Model adjusted for all variables in the primary model plus mean pulse rate.",
         cells_column_labels(columns = "Pulse_Extended")
      ) %>%
      tab_footnote(
         footnote = paste0(
            Renal, " was log-transformed before analysis."
         ),
         locations = cells_body(columns = "Variable", rows = 21)
      ) %>%
      tab_source_note(
         source_note = md(
            c(
               "All estimates are survey-weighted.",
               "P-values were obtained using survey-weighted Wald tests:",
               "∗ p < 0.05.",
               "∗∗ p < 0.01.",
               "∗∗∗ p < 0.001"
            )
         )
      )
}

# Apply function on SBP outcome
MBP_SBP <- Coef_comp_tab("SBP", "WC", "ACR") %>%
   tab_header(
      title = md(
         "Table 7: Comparison of adjusted associations across sequential multivariable regression models for SBP"
      )
   )

MBP_SBP

# Apply function on DBP outcome
MBP_DBP <- Coef_comp_tab("DBP", "BMI", "eGFR") %>%
   tab_header(
      title = md(
         "Table 8: Comparison of adjusted associations across sequential multivariable regression models for DBP"
      )
   ) 

MBP_DBP

# Check on p values
summary(svyglm(SBP_avg~Age + Sex + Race_new + Marital_status + Education_new +
                  IPR + Insurance + WC + ACR_log + HbA1c + Pulse_avg, design = MultiM_design))

summary(svyglm(DBP_avg~Age + Sex + Race_new + Marital_status + Education_new +
                  IPR + Insurance + BMI + eGFR_log + HbA1c + Pulse_avg, design = MultiM_design))
# Check on Vif
vif(svyglm(SBP_avg~Age + Sex + Race_new + Marital_status + Education_new +
                  IPR + Insurance + WC + ACR_log + HbA1c + Pulse_avg, design = MultiM_design))

vif(svyglm(DBP_avg~Age + Sex + Race_new + Marital_status + Education_new +
              IPR + Insurance + BMI + eGFR_log + HbA1c + Pulse_avg, design = MultiM_design))

# Check on deviance 
deviance(svyglm(SBP_avg~Age + Sex + Race_new + Marital_status + Education_new +
                  IPR + Insurance + WC + ACR_log + HbA1c + Pulse_avg, design = MultiM_design))

deviance(svyglm(DBP_avg~Age + Sex + Race_new + Marital_status + Education_new +
                   IPR + Insurance + BMI + eGFR_log + HbA1c + Pulse_avg, design = MultiM_design))
