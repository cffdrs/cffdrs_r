crown_base_height <- function(FUELTYPE, CBH, SD, SH) {
  # logic originally in fbp() pulled into its own function
CBHs <-c(
"C1" = 2,  
"C2" = 3,  
"C3" = 8,  
"C4" = 4,  
"C5" = 18,  
"C6" = 7,  
"C7" = 10, 
"D1" = 0,  
"M1" = 6,  
"M2" = 6,  
"M3" = 6,  
"M4" = 6,  
"S1" = 0,  
"S2" = 0,  
"S3" = 0,  
"O1A" = 0,  
"O1B" =0)
  
  CBH <- ifelse(
    CBH <= 0 | CBH > 50 | is.na(CBH),
    ifelse(
      FUELTYPE %in% c("C6") & SD > 0 & SH > 0,
      -11.2 + 1.06 * SH + 0.0017 * SD,
      CBHs[FUELTYPE]
    ),
    CBH
  )
  CBH <- ifelse(CBH < 0, 1e-07, CBH)
  return(CBH)
}
