
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ==== Correlation Map Heat ====

# 1. Calculate survey‐weighted Pearson correlation
cor_result <- svycor(
   ~ SBP_avg + DBP_avg + Pulse_avg + Age + IPR + WC + BMI + HbA1c + 
      ACR_log + eGFR_log,
   design = NHANES_design,
   sig.stats = TRUE,
   bootn = 2000
)

# 2. Extract correlation and p-value matrices
cor_matrix <- cor_result$cors
p_matrix <- cor_result$p.values

# 3. Fix labels
rownames(cor_matrix) <- lookup[rownames(cor_matrix)]
colnames(cor_matrix) <- lookup[colnames(cor_matrix)]

rownames(p_matrix) <- lookup[rownames(p_matrix)]
colnames(p_matrix) <- lookup[colnames(p_matrix)]

# 4. Convert correlation matrix to long format
cor_long <- as.data.frame(cor_matrix) %>%
   rownames_to_column("Variable1") %>%
   pivot_longer(
      -Variable1,
      names_to = "Variable2",
      values_to = "r"
   )

p_long <- as.data.frame(p_matrix) %>%
   rownames_to_column("Variable1") %>%
   pivot_longer(
      -Variable1,
      names_to = "Variable2",
      values_to = "p"
   )

# 5. Combine them and make star system
heatmap_data <- cor_long %>%
   left_join(
      p_long,
      by = c("Variable1", "Variable2")
   ) %>%
   mutate(
      Variable1 = factor(Variable1, levels = colnames(cor_matrix)),
      Variable2 = factor(Variable2, levels = rev(colnames(cor_matrix)))
   ) %>%
   filter(Variable1 != Variable2) %>%
   mutate(
      stars = case_when(
         p < 0.001 ~ "***",
         p < 0.01  ~ "**",
         p < 0.05  ~ "*",
         TRUE      ~ ""
      )
   )

# 6. Plot as heat map
ggplot(
   heatmap_data,
   aes(
      x = Variable1,
      y = Variable2,
      fill = r
   )
) +
   geom_tile() +
   geom_text(
      aes(label = paste0(
         round(r, 2),
         stars
      )
      ),
      size = 3
   ) +
   scale_fill_gradient2(
      low = "darkblue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Pearson r"
   ) +
   theme_minimal() +
   theme(
      axis.text.x = element_text(
         angle = 45,
         hjust = 1
      ),
      axis.title = element_blank()
   ) +
   coord_equal()
