fctRAZ0  <- function(input=NULL, output="Primary") {                                                                                           
  
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  output <- toupper(output)
  #if input does not exist, then set defaults
  if (is.null(input)) {
    input<-data.frame(FUELTYPE="C2",ACCEL=0,DJ=180,D0=0,ELV=0,BUIEFF=1,HR=1,
                      FFMC=90,ISI=0,BUI=60,WS=10,WD=0,GS=0,ASPECT=0,PC=50,
                      PDF=35,CC=80,GFL=0.35,CBH=3,CFL=1,LAT=55,LONG=-120,
                      FMC=0,THETA=0)
    input[, "FUELTYPE"] <- as.character(input[, "FUELTYPE"])
  }
  #set local scope variables from the parameters for simpler to referencing
  names(input) <- toupper(names(input))
  ID <- input$ID
  FUELTYPE <- toupper(input$FUELTYPE)
  FFMC <- input$FFMC
  BUI <- input$BUI
  WS <- input$WS
  WD <- input$WD
  FMC <- input$FMC
  GS <- input$GS
  LAT <- input$LAT
  LONG <- input$LONG
  ELV <- input$ELV
  DJ <- input$DJ
  D0 <- input$D0
  SD <- input$SD
  SH <- input$SH
  HR <- input$HR
  PC <- input$PC
  PDF <- input$PDF
  GFL <- input$GFL
  CC <- input$CC
  THETA <- input$THETA
  ACCEL <- input$ACCEL
  ASPECT <- input$ASPECT
  BUIEFF <- input$BUIEFF
  CBH <- input$CBH
  CFL <- input$CFL
  ISI <- input$ISI
  n0 <- nrow(input)
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  if (!exists("FUELTYPE") | is.null(FUELTYPE)){ 
    warning("FuelType is a required input, default FuelType = C2 is used in the 
            calculation")
    FUELTYPE <- rep("C2", n0)}
  if (!exists("FFMC") | is.null(FFMC)){ 
    warning("FFMC is a required input, default FFMC = 90 is used in the 
            calculation")
    FFMC <- rep(90, n0)}
  if (!exists("BUI") | is.null(BUI)){ 
    warning("BUI is a required input, default BUI = 60 is used in the 
            calculation")
    BUI <- rep(60, n0)}
  if (!exists("WS") | is.null(WS)){ 
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    WS <- rep(10, n0)}
  if (!exists("GS") | is.null(GS)){ 
    warning("GS is a required input,GS = 0 is used in the calculation")
    GS <- rep(0, n0)}
  if (!exists("LAT") | is.null(LAT)){ 
    warning("LAT is a required input, default LAT=55 is used in the 
            calculation")
    LAT <- rep(55, n0)}
  if (!exists("LONG") | is.null(LONG)){ 
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    LONG <- rep(-120, n0)}
  if (!exists("DJ") | is.null(DJ)){ 
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    DJ <- rep(180, n0)}
  if (!exists("ASPECT") | is.null(ASPECT)){ 
    warning("Aspect is a required input, Aspect = 0 is used in the calculation")
    ASPECT <- rep(0, n0)}
  if (!exists("WD") | is.null(WD)) 
    WD <- rep(0, n0)
  if (!exists("FMC") | is.null(FMC)) 
    FMC <- rep(0, n0)
  if (!exists("ELV") | is.null(ELV)) 
    ELV <- rep(0, n0)
  if (!exists("SD") | is.null(SD)) 
    SD <- rep(0, n0)
  if (!exists("SH") | is.null(SH)) 
    SH <- rep(0, n0)
  if (!exists("D0") | is.null(D0)) 
    D0 <- rep(0, n0)
  if (!exists("HR") | is.null(HR)) 
    HR <- rep(1, n0)
  if (!exists("PC") | is.null(PC)) 
    PC <- rep(50, n0)
  if (!exists("PDF") | is.null(PDF)) 
    PDF <- rep(35, n0)
  if (!exists("GFL") | is.null(GFL)) 
    GFL <- rep(0.35, n0)
  if (!exists("CC") | is.null(CC)) 
    CC <- rep(80, n0)
  if (!exists("THETA") | is.null(THETA)) 
    THETA <- rep(0, n0)
  if (!exists("ACCEL") | is.null(ACCEL)) 
    ACCEL <- rep(0, n0)
  if (!exists("BUIEFF") | is.null(BUIEFF)) 
    BUIEFF <- rep(1, n0)
  if (!exists("CBH") | is.null(CBH)) 
    CBH <- rep(0, n0)
  if (!exists("CFL") | is.null(CFL)) 
    CFL <- rep(0, n0)
  if (!exists("ISI") | is.null(ISI)) 
    ISI <- rep(0, n0)
  #Convert Wind Direction from degress to radians
  WD <- WD * pi/180
  #Convert Theta from degress to radians
  THETA <- THETA * pi/180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  #Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi/180
  ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) 
    warning("Input variable Accel is out of range, will be assigned to 1")
  ACCEL <- ifelse(!ACCEL %in% c(0, 1), 1, ACCEL)
  DJ <- ifelse(DJ < 0 | DJ > 366, 0, DJ)
  DJ <- ifelse(is.na(DJ), 180, DJ)
  D0 <- ifelse(is.na(D0) | D0 < 0 | D0 > 366, 0, D0)
  ELV <- ifelse(ELV < 0 | ELV > 10000, 0, ELV)
  ELV <- ifelse(is.na(ELV), 0, ELV)
  BUIEFF <- ifelse(BUIEFF <= 0, 0, 1)
  BUIEFF <- ifelse(is.na(BUIEFF), 1, BUIEFF)
  HR <- ifelse(HR < 0, -HR, HR)
  HR <- ifelse(HR > 366 * 24, 24, HR)
  HR <- ifelse(is.na(HR), 0, HR)
  FFMC <- ifelse(FFMC < 0 | FFMC > 101, 0, FFMC)
  FFMC <- ifelse(is.na(FFMC), 90, FFMC)
  ISI <- ifelse(is.na(ISI) | ISI < 0 | ISI > 300, 0, ISI)
  BUI <- ifelse(BUI < 0 | BUI > 1000, 0, BUI)
  BUI <- ifelse(is.na(BUI), 60, BUI)
  WS <- ifelse(WS < 0 | WS > 300, 0, WS)
  WS <- ifelse(is.na(WS), 10, WS)
  WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi, 
               0, WD)
  GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  CC <- ifelse(is.na(CC), 80, CC)
  GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35, 
                GFL)
  LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  LAT <- ifelse(is.na(LAT), 55, LAT)
  LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  LONG <- ifelse(is.na(LONG), -120, LONG)
  THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA > 
                    2 * pi, 0, THETA)
  SD <- ifelse(SD < 0 | SD > 1e+05, -999, SD)
  SD <- ifelse(is.na(SD), 0, SD)
  SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  SH <- ifelse(is.na(SH), 0, SH)
  
  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  if(length(FUELTYPE[is.na(FUELTYPE)])>0){
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE<-ifelse(is.na(FUELTYPE),"C2",FUELTYPE)}
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  #Convert hours to minutes
  HR <- HR * 60
  #Corrections to reorient Wind Azimuth(WAZ) and Uphill slode azimuth(SAZ)
  WAZ <- WD + pi
  WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)
  SAZ <- ASPECT + pi
  SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  #Any negative longitudes (western hemisphere) are translated to positive 
  #  longitudes
  LONG <- ifelse(LONG < 0, -LONG, LONG)
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Initializing variables
  ############################################################################
  ############################################################################
  #                         START
  # Initializing variables
  ############################################################################
  SFC <- TFC <- HFI <- CFB <- ROS <- 0
  RAZ <- -999
  if (output == "SECONDARY" | output == "ALL" | output == "S" | 
      output == "A") {
    FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <- 
      BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- 0
    TI <- FTI <- BTI <- TTI <- LB <- WSV <- -999
  }
  this <- FUELS[[FUELTYPE]]
  CBH <- .CrownBaseHeight(this, CBH, SD, SH)
  CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), this$CFL, CFL)
  FMC <- ifelse(FMC <= 0 | FMC > 120 | is.na(FMC),
                .FoliarMoistureContent(this, LAT, LONG, ELV, DJ, D0),
                FMC)
  ############################################################################
  #                         END
  ############################################################################
  #Calculate Surface fuel consumption (SFC)
  SFC <- .SurfaceFuelConsumption(this, FFMC, BUI, PC, GFL)
  #Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  SLOPE_ADJUST <- .SlopeAdjust(this, FFMC, BUI, WS, WAZ, GS, SAZ, FMC, SFC, PC, PDF, CC, CBH, ISI)
  #Calculate the net effective windspeed (WSV)
  # WSV0 <- SLOPE_ADJUST[["WSV"]]
  WSV0 <- SLOPE_ADJUST$WSV
  #Calculate the net effective wind direction (RAZ)
  # RAZ0 <- SLOPE_ADJUST[["RAZ"]]
  RAZ0 <- SLOPE_ADJUST$RAZ
  return(RAZ0)
}
test_that("FireBehaviourPrediction_RAZ0", {
  checkData('FireBehaviourPrediction_RAZ0',
            fctOnInput(fctRAZ0),
            FBP_ARGS)
})
