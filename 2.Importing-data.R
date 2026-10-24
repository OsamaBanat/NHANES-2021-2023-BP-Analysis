
# NHANES Blood Pressure Analysis: Predictors of Blood Pressure in US Adults
# NHANES Data: August 2021-August 2023 cycle
###################################################

# ==== Importing Data ====

# Give names for NHANES files
Files <- c(
   Alb_Cr       = "ALB_CR_L.xpt",
   Biochemistry = "BIOPRO_L.xpt",
   Body         = "BMX_L.xpt",
   BP           = "BPXO_L.xpt",
   BP_Qes       = "BPQ_L.xpt",
   Demo         = "DEMO_L.xpt",
   Diabetes     = "DIQ_L.xpt",
   GHB          = "GHB_L.xpt",
   Insurance    = "HIQ_L.xpt",
   Pregnancy    = "UCPREG_L.xpt"
)

# Get data from NHANES website  
Data <- lapply(Files, function(x) {
   read_xpt(
      paste0(
         "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/",
         x
      )
   )
})

# Separate data into variables
names(Data) <- names(Files)

Alb_Cr       <- Data$Alb_Cr
Biochemistry <- Data$Biochemistry
Body         <- Data$Body
BP           <- Data$BP
BP_Qes       <- Data$BP_Qes
Demo         <- Data$Demo
Diabetes     <- Data$Diabetes
GHB          <- Data$GHB
Insurance    <- Data$Insurance
Pregnancy    <- Data$Pregnancy

