
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES data: August 2021-August 2023 cycle
###################################################

# ===== Constants ==== 
####################################################
# Cleaning data                           line 15
# Combine data                            line 214
# Create derived variables                line 227
# Exclusion criteria                      line 297
# Adjust categorical data variables       line 348
####################################################

# ==== Cleaning data ====

## ==== 1. Urine Albumin-Creatinine ratio (ACR) ====

# Rename columns 
Alb_Cr <- Alb_Cr %>% 
   rename (
      Albumin    = URXUMA,
      Creatinine = URXUCR,
      ACR        = URDACT
   )

# Selecting interested in columns 
Alb_Cr <- Alb_Cr %>% 
   select(
      SEQN, 
      Albumin, 
      Creatinine, 
      ACR
   )

## ==== 2. Biochemistry (Serum Creatinine) ====

# Rename columns 
Biochemistry <- Biochemistry %>% 
   rename (SCr = LBXSCR)

# Selecting interested in columns 
Biochemistry <- Biochemistry %>% 
   select(
      SEQN, SCr
   )

## ==== 3. Body ====

# Rename columns 
Body <- Body %>% 
   rename (
      Weight = BMXWT,
      Height = BMXHT,
      WC     = BMXWAIST,
      BMI    = BMXBMI
   )

# Selecting interested in columns 
Body <- Body %>% 
   select(
      SEQN, Weight, Height, WC, BMI
   )

## ==== 4. Blood Pressure (BP) ====

# Rename columns 
BP <- BP %>% rename (
   Systolic1  = BPXOSY1,
   Systolic2  = BPXOSY2,
   Systolic3  = BPXOSY3,
   Diastolic1 = BPXODI1,
   Diastolic2 = BPXODI2,
   Diastolic3 = BPXODI3,
   Pulse1 = BPXOPLS1,
   Pulse2 = BPXOPLS2,
   Pulse3 = BPXOPLS3
)

# Selecting interested in columns 
BP <- BP %>% 
   select(
      SEQN, Systolic1, Systolic2, Systolic3, 
      Diastolic1,Diastolic2, Diastolic3, 
      Pulse1, Pulse2, Pulse3
   )

## ==== 5. Blood Pressure medications ====

# Rename columns 
BP_Qes <- BP_Qes %>% 
   rename (
      Had_BP         = BPQ020,
      Taking_BP_meds = BPQ150
   )

# Selecting interested in columns 
BP_Qes <- BP_Qes %>%
   select(
      SEQN, 
      Had_BP,
      Taking_BP_meds
   )

# As these questions are only applicable for participant with HTN
# Use code "3" to code "Not applicable" group
BP_Qes$Taking_BP_meds [BP_Qes$Had_BP == 2] <- 3 

# Change 9 codes to NA
BP_Qes$Taking_BP_meds [BP_Qes$Had_BP == 2] <- 3 
BP_Qes$Taking_BP_meds [BP_Qes$Taking_BP_meds == 9] <- NA

## ==== 6. Demography  ====

# Rename columns 
Demo <- Demo %>% 
   rename (
      Sex            = RIAGENDR, 
      Age            = RIDAGEYR,
      Race           = RIDRETH3, 
      Marital_status = DMDMARTZ,
      IPR            = INDFMPIR, 
      Education      = DMDEDUC2,
      MEC_weight     = WTMEC2YR,
      Strata         = SDMVSTRA,
      PSU            = SDMVPSU
   )

# Selecting interested in columns 
Demo <- Demo %>% 
   select (
      SEQN, 
      Sex, 
      Age, 
      Race, 
      Marital_status, 
      IPR,
      Education, 
      MEC_weight, 
      Strata, 
      PSU
   )

# Change 7 and 9 codes to NA
Demo$Education [Demo$Education == 9] <- NA

Demo$Marital_status [Demo$Marital_status == 77 | 
                        Demo$Marital_status == 99] <- NA

# Absence of category "5" will lead to problems in interpreting
# results later on in modeling. Its better to eliminate this now
Demo$Race [Demo$Race == 6] <- 5
Demo$Race [Demo$Race == 7] <- 6

## ==== 7. Having Diabetes ====

# Rename columns 
Diabetes <- Diabetes %>% 
   rename (Diabetes = DIQ010)

# Selecting interested in columns 
Diabetes <- Diabetes %>% 
   select(
      SEQN, Diabetes
   )

# Change 9 codes to NA
Diabetes$Diabetes [Diabetes$Diabetes == 9] <- NA

## ==== 8. Glycohemoglobin (HbA1c) ====

# Rename columns 
GHB <- GHB %>% 
   rename (HbA1c = LBXGH)

# Selecting interested in columns 
GHB <- GHB %>% 
   select(
      SEQN, HbA1c
   )

## ==== 9. Insurance ====

# Rename columns 
Insurance <- Insurance %>% 
   rename (Insurance = HIQ011)

# Selecting interested in columns 
Insurance <- Insurance %>% 
   select(
      SEQN, Insurance
   )

# Change 7 & 9 codes to NA
Insurance$Insurance [Insurance$Insurance == 7 |
                        Insurance$Insurance == 9] <- NA

## ==== 10. Pregnancy ====

# Rename columns 
Pregnancy <- Pregnancy %>% 
   rename (Pregnancy = URXPREG)

# Selecting interested in columns 
Pregnancy <- Pregnancy %>% 
   select(
      SEQN, Pregnancy
   )

# Change 3 & 4 codes to NA
Pregnancy$Pregnancy [Pregnancy$Pregnancy == 3 |
                        Pregnancy$Pregnancy == 4] <- NA

# ==== Combine data ====

Data <- Demo  %>% 
   left_join (BP, by = "SEQN") %>%
   left_join (Insurance,by = "SEQN") %>%
   left_join (Body,by = "SEQN") %>%
   left_join (Biochemistry,by = "SEQN") %>%
   left_join (Alb_Cr,by = "SEQN") %>%
   left_join (GHB,by = "SEQN") %>%
   left_join (Diabetes,by = "SEQN") %>%
   left_join (BP_Qes,by = "SEQN") %>%
   left_join (Pregnancy,by = "SEQN")

# ==== Create derived variables ====

## ==== 1. Avg Systolic & Diastolic BP & Pulse ====

# Calculate average for systolic BP, diastolic BP, and Pulse
Data <- Data %>% mutate(
   SBP_avg = rowMeans(
      select(., Systolic1, Systolic2,Systolic3),
      na.rm = T
   ),
   
   DBP_avg = rowMeans(
      select(., Diastolic1, Diastolic2, Diastolic3),
      na.rm = T
   ),
   
   Pulse_avg = rowMeans(
      select(., Pulse1, Pulse2, Pulse3),
      na.rm = T
   )
)

# Convert NaN to NA
Data$SBP_avg[is.nan(Data$SBP_avg)] <- NA
Data$DBP_avg[is.nan(Data$DBP_avg)] <- NA
Data$Pulse_avg[is.nan(Data$Pulse_avg)] <- NA

## ==== 2. Glomerular Filtration Rate (eGFR) ====

# Create a function to calculate eGFR using 2021 CKD-EPI creatinine equation
# eGFR = 142 * (min(SCr/k, 1)^ a) * (max(SCr/k, 1)^ -1.200) *
#        (0.9938^Age) * Sex_factor
# Sex factor = 1 in males and 1.012 in females

GFR_calculator <- function(SCr, Age, Sex){
   
   # Handling missing values
   if (is.na(SCr) | is.na(Age) | is.na(Sex)){
      return(NA)
   }
   
   # Determining the value of k, a, and Sex factor
   # Sex: males = 1, females = 2
   if (Sex == 1) {
      k <-  0.9
      a <- -0.302
      Sex_factor <- 1
      
   } else if (Sex == 2){
      k <-  0.7
      a <- -0.241
      Sex_factor <- 1.012
      
   } else {
      return(NA)
   }
   
   # Calculate eGFR
   GFR <- 142 * (min(SCr/k, 1)^ a) * (max(SCr/k, 1)^ -1.200) *
      (0.9938^Age) * Sex_factor
   
   return(GFR)
}

# Create a column for total weekly moderate physical activity
Data <- Data %>% 
   rowwise() %>%
   mutate (eGFR = GFR_calculator(SCr, Age, Sex)) %>%
   ungroup()

# ==== Exclusion criteria ====

# 1. Exclude < 18 years old
sum (Data$Age < 18)

Data <- Data %>% 
   filter (Age >= 18)

# 2. Exclude pregnant women

## 1 = positive results, 2 = negative results
table(Data$Pregnancy, useNA = "ifany")

Data <- Data %>% 
   filter (Pregnancy == 2 | is.na(Pregnancy))

# 3. Exclude missing BP data
sum(is.na(Data$SBP_avg) | is.na(Data$DBP_avg))

Data <- Data %>% 
   filter (!is.na(SBP_avg) & !is.na(DBP_avg))

# 4. Exclude missing covariates data

## Find number of missing data
map_dfr(
   c(
      "SBP_avg", "DBP_avg", "Pulse_avg", "Age", "Sex", "Race", "Marital_status",
      "Education", "IPR", "Insurance", "BMI", "HbA1c", "WC", "Diabetes", "ACR",
      "eGFR", "Taking_BP_meds"
   ),
   function (var) {
      tibble(
         var  = var,
         Miss = sum(is.na(Data[[var]]))
      )
   }
)

# Exclude missing data
Data <- Data %>% 
   subset(.,
          complete.cases(Data[, 
                              c( 
                                 "Pulse_avg", "Age", "Sex", "Race", "Marital_status", 
                                 "Education", "IPR", "Insurance","BMI", "WC",
                                 "HbA1c",  "Diabetes", "ACR", "eGFR", "Taking_BP_meds"
                              )
          ])
   )

# ==== Adjust categorical data variables ====

Data$Sex <- factor(
   Data$Sex,
   labels = c("Male", "Female"),
   levels = c(1, 2)
)

Data$Race <- factor(
   Data$Race,
   labels = c(
      "Mexican American", "Other Hispanic", "Non Hispanic White", "Non Hispanic Black", 
      "Non Hispanic Asian", "Other Race"
   ),
   levels = c(1, 2, 3, 4, 5, 6)
)

Data$Insurance <- factor(
   Data$Insurance, 
   labels = c("Not-covered", "Covered"),
   levels = c(2, 1)
)

Data$Education <- factor(
   Data$Education, 
   labels = c(
      "Less than 9th grade", 
      "9-11th grade (Includes 12th grade with no diploma)",
      "High school graduate/GED or equivalent",
      "Some college or AA degree", 
      "College graduate or above"
   ),
   levels = c(1, 2, 3, 4, 5)
)

Data$Diabetes <- factor(
   Data$Diabetes,
   labels = c("No", "Yes", "Borderline"),
   levels = c(2, 1, 3)
)

Data$Marital_status <- factor(
   Data$Marital_status,
   labels = c(
      "Never married",
      "Married/Living with partner",
      "Widowed/Divorced/Separated"
   ),
   levels = c(3, 1, 2)
)

Data$Taking_BP_meds <- factor(
   Data$Taking_BP_meds,
   labels = c(
      "Not diagnosed",
      "Not taking",
      "Taking"
   ),
   levels = c(3, 2, 1)
)

# Check on data
glimpse(Data)
