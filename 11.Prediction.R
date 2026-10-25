
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Constants ==== 
####################################################
# Split data into train and test          line 35
# Create a recipe                         line 48
# Create 10-fold cross-validation         line 73
# Create models                           line 81
# Create workflows                        line 94
# Create tuning grid                      line 106
# Fit models                              line 113
# Collect model results                   line 136
# Choose best penalty for each model      line 144
# Create comparison table                 line 183
# Select overall best penalized model     line 261
# Tuning plot                             line 268
# Select the final predictive model       line 302
# Fit the final predictive model          line 308
# Extract coefficients                    line 337
# Diagnostic plots                        line 508
# Get results of train and test data      line 612
# Observed vs predicted plot for CV       line 655
# Categorical subgroup performance        line 688
# Residual analysis                       line 779
# Return results                          line 828
# Apply function                          line 851
####################################################

# Build a function to choose best model and select/investigate final model
BP_Model <- function(Outcome, BP_label) {

## ==== 1. Split data into train and test ====

set.seed(1234)

Split <- initial_split(
   Data, 
   prop = 0.8, 
   strata = !!sym(Outcome)
)

Train <- training(Split)
Test  <- testing (Split)

## ==== 2. Create a recipe ====

# Define variables

Variables <- c( 
   "Age", "Sex", "Race", "Marital_status", "Education", "IPR", "Insurance", 
   "BMI", "WC", "ACR_log", "eGFR_log", "Diabetes", "HbA1c", "Taking_BP_meds",
   "Pulse_avg"
)

# Create a formula 
BP_formula <- reformulate(
   termlabels = Variables,
   response = Outcome
)

# Create a recipe
BP_recipe <- recipe(
   BP_formula,
   data = Train
   ) %>%
   step_zv(all_predictors()) %>%                 # Removes zero-variance predictors
   step_dummy(all_nominal_predictors()) %>%      # Convert categorical predictors into dummy 
   step_normalize(all_numeric_predictors())      # Standardize every numeric predictor

## ==== 3. Create 10-fold cross-validation ====
Folds <- vfold_cv(
   Train,
   v = 10,
   repeats = 5,
   strata = !!sym(Outcome)
)

## ==== 4. Create models ====

## Ordinary linear regression
LM_spec <- linear_reg() %>%
   set_engine("lm")

## Penalized regression
P_model <- linear_reg (
   penalty = tune(), 
   mixture = tune()
   ) %>%
   set_engine (engine = "glmnet") 

## ==== 5. Create workflows ====

# Ordinary linear regression
LM_WF <- workflow() %>%
   add_recipe(BP_recipe) %>%
   add_model(LM_spec)

# Penalized regression
P_WF <- workflow() %>% 
   add_recipe (BP_recipe) %>% 
   add_model(P_model)

## ==== 6. Create tuning grid ====
Penalty_grid <- grid_regular(
   penalty(),
   mixture(range = c(0, 1)),  # Contains Ridge and Lasso regressions
   levels = c(50, 11)
)

## ==== 7. Fit models ====

# Ordinary linear regression
LM_results <- fit_resamples(
   LM_WF,
   resamples = Folds,
   metrics = metric_set(rmse, mae, rsq),
   control = control_resamples(
      save_pred = TRUE
   )
)

# Penalized regression
P_results <- tune_grid (
   P_WF, 
   resamples = Folds, 
   grid = Penalty_grid,
   metrics = metric_set(rmse, mae, rsq),
   control = control_grid(
      save_pred = TRUE
   )
)

## ==== 8. Collect model results ====

# Ordinary linear regression
Best_LM <- collect_metrics(LM_results) 

# Penalized regression
P_metrics <- collect_metrics(P_results)

## ==== 9. Choose best penalty for each model ====

# Ridge model
Best_Ridge <- P_metrics %>%
   filter(
      .metric == "rmse",
      mixture == 0
   ) %>%
   slice_min(
      order_by = mean,
      n = 1,
      with_ties = FALSE
   )

# Lasso model
Best_Lasso <- P_metrics %>%
   filter(
      .metric == "rmse",
      mixture == 1
   ) %>%
   slice_min(
      order_by = mean,
      n = 1,
      with_ties = FALSE
   )

# Elastic Net model
Best_El <- P_metrics %>%
   filter(
      .metric == "rmse",
      mixture > 0,
      mixture < 1
   ) %>%
   slice_min(
      order_by = mean,
      n = 1,
      with_ties = FALSE
   )

## ==== 10. Create comparison table ====

## Ordinary linear regression
LM_summary <- Best_LM %>%
   select(.metric, mean, std_err) %>%
   mutate(
      Model = "Ordinary linear regression",
      Mixture = NA_real_,
      Penalty = NA_real_
   )

## Ridge model
Ridge_summary <- P_metrics %>%
   filter(.config == Best_Ridge$.config) %>%
   select(.metric, mean, std_err) %>%
   mutate(
      Model = "Ridge",
      Mixture = 0,
      Penalty = Best_Ridge$penalty
   )

## Lasso model
Lasso_summary <- P_metrics %>%
   filter(.config == Best_Lasso$.config) %>%
   select(.metric, mean, std_err) %>%
   mutate(
      Model = "Lasso",
      Mixture = 1,
      Penalty = Best_Lasso$penalty
   )

## Elastic Net model
El_summary <- P_metrics %>%
   filter(.config == Best_El$.config) %>%
   select(.metric, mean, std_err) %>%
   mutate(
      Model = "Elastic Net",
      Mixture = Best_El$mixture,
      Penalty = Best_El$penalty
   )

## Combine results in a tidy table
Train_table <- bind_rows(
   LM_summary,
   Ridge_summary,
   Lasso_summary,
   El_summary
) %>%
   pivot_wider(
      names_from = .metric,
      values_from = c(mean, std_err)
   ) %>%
   mutate(
      Outcome = BP_label,
      Mixture = ifelse(
         is.na(Mixture),
         "—",
         sprintf("%.2f", Mixture)
      ),
      Penalty = ifelse(
         is.na(Penalty),
         "—",
         sprintf("%.4f", Penalty)
      ),
      RMSE = sprintf("%.4f", mean_rmse),
      MAE  = sprintf("%.4f", mean_mae),
      R2   = sprintf("%.4f", mean_rsq)
   ) %>%
   select(
      Outcome,
      Model,
      Mixture,
      Penalty,
      RMSE,
      MAE,
      R2
   )

## ==== 11. Select overall best penalized model ====

Best_GLMNET <- select_best(
   P_results,
   metric = "rmse"
)

## ==== 12. Tuning plot ====

Tuning_RMSE <- P_metrics %>%
   filter(.metric == "rmse")

Tuning_plot <- ggplot(
   Tuning_RMSE,
   aes(
      x = penalty,
      y = mean,
      group = factor(mixture),
      color = factor(mixture)
   )
) +
   geom_line() +
   geom_point(size = 1.3) +
   geom_point(
      data = Tuning_RMSE %>%
         filter(
            mixture == Best_GLMNET$mixture,
            penalty == Best_GLMNET$penalty
         ),
      aes(x = penalty, y = mean),
      size = 4,
      inherit.aes = FALSE
   ) +
   scale_x_log10() +
   labs(
      x = "Penalty (log scale)",
      y = "Mean cross-validated RMSE",
      color = "Lasso proportion",
      title = paste0("Hyperparameter tuning of", BP_label, "penalized regression models")
   ) 

## ==== 13. Select the final predictive model ====
Final_WF <- finalize_workflow(
   P_WF, 
   Best_GLMNET
   )

## ==== 14. Fit the final predictive model ====
Final_fit <- last_fit(
   Final_WF,
   split = Split,
   metrics = metric_set(
      rmse, 
      mae, 
      rsq
      )
)

# Final test performance
Final_test_metrics <- collect_metrics(Final_fit) %>%
   pivot_wider(
      names_from = .metric,
      values_from = .estimate
   ) %>%
   mutate(
      Outcome = BP_label
   ) %>%
   select(
      Outcome,
      rmse,
      mae,
      rsq
   )

Final_workflow <- extract_workflow(Final_fit)

## ==== 15. Extract coefficients from the final predictive model ====

# Extract coefficients
Final_coefficients <- Final_fit %>%
   extract_fit_parsnip() %>%
   tidy()

Coefficient_table <- tibble(
   Variable = c(
      "Sex",
      "Male",
      "Female",
      
      "Race/ethnicity",
      "Mexican American",
      "Non Hispanic White",
      "Non Hispanic Black",
      "Non Hispanic Asian",
      "Other Hispanic",
      "Other Race",
      
      "Marital status",
      "Never married",
      "Married/Living with partner",
      "Widowed/Divorced/Separated",
      
      "Education",
      "Less than 9th grade",
      "9–11th grade (includes 12th grade with no diploma)",
      "High school graduate/GED or equivalent",
      "Some college or AA degree",
      "College graduate or above",
      
      "Insurance",
      "Not covered",
      "Covered",
      
      "Age",
      "IPR",
      "BMI",
      "WC",
      "eGFR",
      "ACR",
      "Mean Pulse rate",
      "HbA1c",
      
      "Diabetes",
      "No",
      "Yes",
      "Borderline",
      
      "BP-lowering medications",
      "Not diagnosed",
      "Not taking",
      "Taking"
   ),
   Estimate = c(
   NA,
   0,
   Final_coefficients$estimate[Final_coefficients$term == "Sex_Female"],
   
   NA,
   0,
   Final_coefficients$estimate[Final_coefficients$term == "Race_Non.Hispanic.White"],
   Final_coefficients$estimate[Final_coefficients$term == "Race_Non.Hispanic.Black"],
   Final_coefficients$estimate[Final_coefficients$term == "Race_Non.Hispanic.Asian"],
   Final_coefficients$estimate[Final_coefficients$term == "Race_Other.Hispanic"],
   Final_coefficients$estimate[Final_coefficients$term == "Race_Other.Race"],
   
   NA,
   0,
   Final_coefficients$estimate[
      Final_coefficients$term == "Marital_status_Married.Living.with.partner"
   ],
   Final_coefficients$estimate[
      Final_coefficients$term == "Marital_status_Widowed.Divorced.Separated"
   ],
   
   NA,
   0,
   Final_coefficients$estimate[
      Final_coefficients$term ==
         "Education_X9.11th.grade..Includes.12th.grade.with.no.diploma."
   ],
   Final_coefficients$estimate[
      Final_coefficients$term ==
         "Education_High.school.graduate.GED.or.equivalent"
   ],
   Final_coefficients$estimate[
      Final_coefficients$term ==
         "Education_Some.college.or.AA.degree"
   ],
   Final_coefficients$estimate[
      Final_coefficients$term ==
         "Education_College.graduate.or.above"
   ],
   
   NA,
   0,
   Final_coefficients$estimate[
      Final_coefficients$term == "Insurance_Covered"
   ],
   
   Final_coefficients$estimate[Final_coefficients$term == "Age"],
   Final_coefficients$estimate[Final_coefficients$term == "IPR"],
   Final_coefficients$estimate[Final_coefficients$term == "BMI"],
   Final_coefficients$estimate[Final_coefficients$term == "WC"],
   Final_coefficients$estimate[Final_coefficients$term == "eGFR_log"],
   Final_coefficients$estimate[Final_coefficients$term == "ACR_log"],
   Final_coefficients$estimate[Final_coefficients$term == "Pulse_avg"],
   Final_coefficients$estimate[Final_coefficients$term == "HbA1c"],
   
   NA,
   0,
   Final_coefficients$estimate[
      Final_coefficients$term == "Diabetes_Yes"
   ],
   Final_coefficients$estimate[
      Final_coefficients$term == "Diabetes_Borderline"
   ],
   
   NA,
   0,
   Final_coefficients$estimate[
      Final_coefficients$term == "Taking_BP_meds_Not.taking"
   ],
   Final_coefficients$estimate[
      Final_coefficients$term == "Taking_BP_meds_Taking"
   ]
)
) %>%
   mutate(
      Estimate = round(Estimate, 3)
   ) %>% 
   gt() %>%
   text_transform(
      locations = cells_body(
         columns = Estimate,
         rows = Variable %in% c("Sex", "Race/ethnicity", "Marital status", "Education", "Insurance", "Diabetes", "BP-lowering medications")
      ),
      fn = function(x) {
         ""
      }
   ) %>% 
   text_transform(
      locations = cells_body(
         columns = Estimate,
         rows = Variable %in% c("Male", "Mexican American", "Never married", "Less than 9th grade", "Not covered", "No", "Not diagnosed")
      ),
      fn = function(x) {
         "Ref"
      }
   )

# Final model diagnostic data
Final_train_predictions <- predict(
   Final_workflow,
   new_data = Train
) %>%
   bind_cols(
      Train %>% select(all_of(Outcome))
   ) %>%
   rename(
      Observed = all_of(Outcome),
      Predicted = .pred
   ) %>%
   mutate(
      Residual = Observed - Predicted,
      Standardized_Residual = Residual / sd(Residual, na.rm = TRUE)
   ) 

## ==== 16. Diagnostic plots ====

### ==== Plot 1: Residuals vs fitted values ====

Diagnostic_residuals <- ggplot(
   Final_train_predictions,
   aes(
      Predicted, 
      Residual)
)  +
   geom_point(
      color = "#ff9800",
      alpha = 0.35
   ) +
   geom_smooth(
      method = "loess",
      se = TRUE
   ) +
   geom_hline(
      yintercept = 0,
      linetype = "dashed"
   ) +
   labs(
      x = "Fitted values (mmHg)",
      y = "Residuals (mmHg)",
      title = "Residuals versus fitted values"
   )

### ==== Plot 2: Normal Q-Q plot ====

Diagnostic_QQ <- ggplot(
   Final_train_predictions,
   aes(
      sample = Residual
      )
) +
   stat_qq() +
   stat_qq_line() +
   labs(
      x = "Theoretical quantiles",
      y = "Standardized residuals",
      title = "Q-Q plot"
   )

### ==== Plot 3: Scale-location plot ====

Diagnostic_scale <- ggplot(
   Final_train_predictions,
   aes(
      x = Predicted,
      y = sqrt(abs(Standardized_Residual))
   )
) +
   geom_point(
      color = "orange",
      alpha = 0.35
   ) +
   geom_smooth(
      method = "loess",
      se = TRUE
   ) +
   labs(
      x = "Fitted values (mmHg)",
      y = expression(sqrt("|Standardized residuals|")),
      title = "Scale-location"
   )

### ==== Plot 4: Residual histogram ====

Diagnostic_residual_histogram <- ggplot(
      Final_train_predictions,
      aes(x = Residual)
   ) +
      geom_histogram(
         aes(y = after_stat(density)),
         bins = 30
      ) +
      geom_density() +
      geom_vline(
         xintercept = 0,
         linetype = "dashed"
      ) +
      labs(
         x = "Residual",
         y = "Density",
         title = "Residual Distribution"
      ) 

### ==== Combine four diagnostic plots ====

Diagnostic_plots <- (
   Diagnostic_residuals +
      Diagnostic_QQ +
      Diagnostic_scale +
      Diagnostic_residual_histogram
) +
   plot_annotation(
      title = paste(
         "Diagnostic plots for final",
         BP_label,
         "predictive model"
      )
   )

## ==== 17. Get results of train and test data ====

# Cross-validation predictions
CV_predictions <- collect_predictions(P_results) %>%
   filter(
      .config == Best_GLMNET$.config
   )

CV_predictions_mean <- CV_predictions %>%
   group_by(.row) %>%
   summarise(
      Observed = first(.data[[Outcome]]),
      Predicted = mean(.pred, na.rm = TRUE),
      .groups = "drop"
   ) %>%
   mutate(
      Residual = Observed - Predicted,
      Dataset = "Cross-validation"
   )

# Held-out test results
Test_predictions <- Test %>%
   bind_cols(
      predict(
         Final_workflow,
         new_data = Test
      )
   ) %>%
   rename(
      Observed = .data[[Outcome]],
      Predicted = .pred,
   ) %>%
   mutate(
      Residual = Observed - Predicted,
      Dataset = "Held-out test"
   )

# Combine test and train data
Prediction_plot_data <- bind_rows(
   CV_predictions_mean,
   Test_predictions
)

## ==== 18. Observed vs predicted plot for CV ====

Prediction_plot <- ggplot(
   Prediction_plot_data,
   aes(
      x = Observed,
      y = Predicted
   )
) +
   geom_point(
      color = "orange",
      alpha = 0.35
   ) +
   geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed"
   ) +
   facet_wrap(
      ~ Dataset,
      ncol = 2
   ) +
   coord_equal() +
   labs(
      x = paste("Observed", BP_label, "(mmHg)"),
      y = paste("Predicted", BP_label, "(mmHg)"),
      title = paste(
         "Observed versus predicted",
         BP_label
      ),
      subtitle = "Cross-validated predictions versus held-out test predictions"
   )

## ==== 19. Categorical subgroup performance ====

# Create a function to return a table with RMSE, MAE, R2, and mean error
Cate_Subgroup_table <- function(vars){
   
   # Create an empty table
   tab <- tibble()
   
   # Create a function to calculate RMSE, MAE, R2, and mean error
   Categorical_metrics <- function(data) {
      
      data %>%
         summarise(
            RMSE = sqrt(
               mean(Residual^2, na.rm = TRUE)
            ),
            
            MAE = mean(
               abs(Residual),
               na.rm = TRUE
            ),
            
            Mean_Error = mean(
               Residual,
               na.rm = TRUE
            ),
            
            R2 = 1 -
               sum(
                  Residual^2,
                  na.rm = TRUE
               ) /
               sum(
                  (
                     Observed - mean(Observed, na.rm = TRUE)
                  )^2,
                  na.rm = TRUE
               ),
            n = n()
         )
   }
   
   # Apply function for each variable 
   for(var in vars){
      
      result <- Test_predictions %>%
         group_by(
            Group = .data[[var]]
         ) %>%
         Categorical_metrics() %>%
         ungroup() %>%
         mutate(
            Variable = lookup[[var]],
            .before = Group
         )
      
      tab <- bind_rows(
         tab,
         result
      )
   }
   
   # Format final table
   tab <- tab %>%
      mutate(
         RMSE = sprintf("%.3f", RMSE),
         MAE = sprintf("%.3f", MAE),
         Mean_Error = sprintf("%.3f", Mean_Error),
         R2 = sprintf("%.3f", R2)
      ) %>%
      select(
         Variable,
         Group,
         n,
         RMSE,
         MAE,
         Mean_Error,
         R2
      )
   
   # Return results
   tab
}

# Apply function on categorical variables
Cat_sub_tab <- Cate_Subgroup_table(
   c(
      "Sex", "Race", "Taking_BP_meds", "Diabetes"
     )
) 
   
## ==== 20. Residual analysis for continuous variables ====

Plot_residual <- function(vars) {
   
   # Apply function for each variable
   map (
      vars, 
      function(var) {
      
   ggplot(
      Test_predictions,
      aes(
         x = .data[[var]],
         y = Residual
      )
   ) +
      geom_point(
         color = "orange",
         alpha = 0.35
      ) +
      geom_smooth(
         method = "loess",
         se = TRUE
      ) +
      geom_hline(
         yintercept = 0,
         linetype = "dashed"
      ) +
      labs(
         x = lookup[[var]],
         y = "Prediction residual (mmHg)"
      )
   }
   )
}

# Apply function on continuous variables
Residual_plots <- wrap_plots(
   Plot_residual(
      c(
         "Age", "BMI", "WC", "ACR_log", "eGFR_log", "HbA1c", "Pulse_avg"
      )
   ),
   ncol = 3
) +
   plot_annotation(
      title = paste0("Prediction error across continuous variables in ", BP_label, " final prediction model")
   )

## ====21. Function returns ====
list(
   Best_LM = Best_LM,
   Best_Ridge = Best_Ridge,
   Best_Lasso = Best_Lasso,
   Best_El = Best_El,
   Best_GLMNET = Best_GLMNET,

   Train_table = Train_table,
   Tuning_plot = Tuning_plot,
   
   Final_WF = Final_WF,
   Final_test_metrics = Final_test_metrics,
   
   Coefficient_table = Coefficient_table,
   Prediction_plot = Prediction_plot,
   Diagnostic_plots = Diagnostic_plots,
   
   Cat_sub_tab = Cat_sub_tab,
   Residual_plots = Residual_plots
   )
}

# Apply function
SBP_Model <- BP_Model("SBP_avg", "SBP")

DBP_Model <- BP_Model("DBP_avg", "DBP")

# Cross-validation performance presentation 
Train_table2 <- bind_rows(
   SBP_Model$Train_table, 
   DBP_Model$Train_table
   ) %>%
   gt(
      groupname_col = "Outcome",
      row_group_as_column = TRUE
   )  %>%
   tab_header(
      title = "Table 9: Cross-validation performance of candidate SBP and DBP predictive models"
   ) %>%
   tab_source_note(
      source_note = md("Values are based on the reported repeated cross-validation results.")
   )

# Overall test-set metrics
Final_test_metrics2 <- bind_rows(
   SBP_Model$Final_test_metrics, 
   DBP_Model$Final_test_metrics
   ) %>%
   gt() %>%
   cols_label(
      rmse    = "RMSE",
      mae    = "MAE",
      rsq = "R2"
   ) %>%
   fmt_number(
      columns = c(rmse, mae, rsq),
      decimals = 4
   ) %>%
   tab_header(
      title = "Table 10: Overall test-set metrics performance of final SBP and DBP predictive models"
   ) %>%
   tab_source_note(
      source_note = md("Final model selected: Lasso for SBP prediction and Ridge for DBP prediction. Metrics were calculated using the held-out test set.")
   )

# Coefficients tables presentation 
SBP_Model$Coefficient_table <- SBP_Model$Coefficient_table %>%
   tab_style(
      style = cell_text(indent = px(15)),
      locations = cells_body(
         columns = Variable,
         rows = Variable %in% c(
            "Male",
            "Female",
            "Mexican American",
            "Non Hispanic White",
            "Non Hispanic Black",
            "Non Hispanic Asian",
            "Other Hispanic",
            "Other Race",
            "Never married",
            "Married/Living with partner",
            "Widowed/Divorced/Separated",
            "Less than 9th grade",
            "9–11th grade (includes 12th grade with no diploma)",
            "High school graduate/GED or equivalent",
            "Some college or AA degree",
            "College graduate or above",
            "Not covered",
            "Covered",
            "No",
            "Yes",
            "Borderline",
            "Not diagnosed",
            "Not taking",
            "Taking"
         )
      )
   ) %>% 
   tab_header(
      title = "Table 11: Coefficients for SBP final prediction model"
   ) %>%
   tab_footnote(
      footnote = "eGFR was log-transformed before analysis.",
      locations = cells_body(columns = "Variable", rows = 28)
   ) %>%
   tab_footnote(
      footnote = "ACR was log-transformed before analysis.",
      locations = cells_body(columns = "Variable", rows = 29)
   )

DBP_Model$Coefficient_table <- DBP_Model$Coefficient_table %>% 
   tab_style(
      style = cell_text(indent = px(15)),
      locations = cells_body(
         columns = Variable,
         rows = Variable %in% c(
            "Male",
            "Female",
            "Mexican American",
            "Non Hispanic White",
            "Non Hispanic Black",
            "Non Hispanic Asian",
            "Other Hispanic",
            "Other Race",
            "Never married",
            "Married/Living with partner",
            "Widowed/Divorced/Separated",
            "Less than 9th grade",
            "9–11th grade (includes 12th grade with no diploma)",
            "High school graduate/GED or equivalent",
            "Some college or AA degree",
            "College graduate or above",
            "Not covered",
            "Covered",
            "No",
            "Yes",
            "Borderline",
            "Not diagnosed",
            "Not taking",
            "Taking"
         )
      )
   )  %>% 
   tab_header(
      title = "Table 12: Coefficients for DBP final prediction model"
   ) %>%
   tab_footnote(
      footnote = "eGFR was log-transformed before analysis.",
      locations = cells_body(columns = "Variable", rows = 28)
   ) %>%
   tab_footnote(
      footnote = "ACR was log-transformed before analysis.",
      locations = cells_body(columns = "Variable", rows = 29)
   )

# Subgroups tables presentation 
SBP_Model$Cat_sub_tab <- SBP_Model$Cat_sub_tab %>%
   gt(
      groupname_col = "Variable"
   ) %>%
   cols_label(
      Variable = "Variable",
      Group = "Subgroup",
      n = "N",
      RMSE = "RMSE (mmHg)",
      MAE = "MAE (mmHg)",
      Mean_Error = "Mean error (mmHg)",
      R2 = "R²"
   ) %>%
   cols_align(
      align = "left",
      columns = Group
   ) %>%
   tab_options(
      row_group.as_column = TRUE
   ) %>%
   tab_header(
      title = "Table 13: Predictive performance across prespecified SBP subgroups"
   )

DBP_Model$Cat_sub_tab <- DBP_Model$Cat_sub_tab %>%
   gt(
      groupname_col = "Variable"
   ) %>%
   cols_label(
      Variable = "Variable",
      Group = "Subgroup",
      n = "N",
      RMSE = "RMSE (mmHg)",
      MAE = "MAE (mmHg)",
      Mean_Error = "Mean error (mmHg)",
      R2 = "R²"
   ) %>%
   cols_align(
      align = "left",
      columns = Group
   ) %>%
   tab_options(
      row_group.as_column = TRUE
   ) %>%
   tab_header(
      title = "Table 14: Predictive performance across prespecified DBP subgroups"
   )

# Show results
Train_table2
Final_test_metrics2

SBP_Model$Coefficient_table
DBP_Model$Coefficient_table

SBP_Model$Diagnostic_plots
DBP_Model$Diagnostic_plots

SBP_Model$Prediction_plot
DBP_Model$Prediction_plot

SBP_Model$Residual_plots
DBP_Model$Residual_plots
