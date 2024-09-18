#' Fire Behaviour Prediction System Calculation (hidden)
#'
#' @description Fire Behavior Prediction System calculations. This is the
#' primary function for calculating FBP for a single timestep. Not all
#' equations are calculated within this function, but have been broken down
#' further.
#'
#' @param input  Data frame of required and optional information needed to
#' calculate FBP function. View the arguments section of the fbp manual
#' (fbp.Rd) under "input" for the full listing of the required and optional
#' inputs.
#' @param output What fbp outputs to return to the user. Options are "Primary",
#' "Secondary" and "All". _Default:_ "Primary"
#'
#' @return output: Either Primary, Secondary, or all FBP outputs in a
#' data.frame
#'
#' @noRd

fire_behaviour_prediction <- function(
    input = NULL,
    output = "Primary") {
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  # print(input)
  # print(input$ID)
  output <- toupper(output)
  # if input does not exist, then set defaults
  if (is.null(input)) {
    input <- data.frame(
      FUELTYPE = "C2", ACCEL = 0, DJ = 180, D0 = 0, ELV = 0, BUIEFF = 1,
      HR = 1, FFMC = 90, ISI = 0, BUI = 60, WS = 10, WD = 0, GS = 0,
      ASPECT = 0, PC = 50, PDF = 35, CC = 80, GFL = 0.35, CBH = 3, CFL = 1,
      LAT = 55, LONG = -120, FMC = 0, THETA = 0
    )
    input[, "FUELTYPE"] <- as.character(input[, "FUELTYPE"])
  }
  # set local scope variables from the parameters for simpler to referencing
  names(input) <- toupper(names(input))
  ID <- input$ID
  FUELTYPE <- input$FUELTYPE
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
  if (!exists("FUELTYPE") | is.null(FUELTYPE)) {
    warning(
      paste0(
        "FuelType is a required input,",
        " default FuelType = C2 is used in the calculation"
      )
    )
    FUELTYPE <- rep("C2", n0)
  }
  FUELTYPE <- toupper(FUELTYPE)
  if (!exists("FFMC") | is.null(FFMC)) {
    warning(
      paste0(
        "FFMC is a required input, default FFMC = 90 is used in the",
        " calculation"
      )
    )
    FFMC <- rep(90, n0)
  }
  if (!exists("BUI") | is.null(BUI)) {
    warning(
      "BUI is a required input, default BUI = 60 is used in the calculation"
    )
    BUI <- rep(60, n0)
  }
  if (!exists("WS") | is.null(WS)) {
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    WS <- rep(10, n0)
  }
  if (!exists("GS") | is.null(GS)) {
    warning("GS is a required input,GS = 0 is used in the calculation")
    GS <- rep(0, n0)
  }
  if (!exists("LAT") | is.null(LAT)) {
    warning(
      "LAT is a required input, default LAT=55 is used in the calculation"
    )
    LAT <- rep(55, n0)
  }
  if (!exists("LONG") | is.null(LONG)) {
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    LONG <- rep(-120, n0)
  }
  if (!exists("DJ") | is.null(DJ)) {
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    DJ <- rep(180, n0)
  }
  if (!exists("ASPECT") | is.null(ASPECT)) {
    warning(
      "Aspect is a required input, Aspect = 0 is used in the calculation"
    )
    ASPECT <- rep(0, n0)
  }
  if (!exists("WD") | is.null(WD)) {
    WD <- rep(0, n0)
  }
  if (!exists("FMC") | is.null(FMC)) {
    FMC <- rep(0, n0)
  }
  if (!exists("ELV") | is.null(ELV)) {
    ELV <- rep(0, n0)
  }
  if (!exists("SD") | is.null(SD)) {
    SD <- rep(0, n0)
  }
  if (!exists("SH") | is.null(SH)) {
    SH <- rep(0, n0)
  }
  if (!exists("D0") | is.null(D0)) {
    D0 <- rep(0, n0)
  }
  if (!exists("HR") | is.null(HR)) {
    HR <- rep(1, n0)
  }
  if (!exists("PC") | is.null(PC)) {
    PC <- rep(50, n0)
  }
  if (!exists("PDF") | is.null(PDF)) {
    PDF <- rep(35, n0)
  }
  if (!exists("GFL") | is.null(GFL)) {
    GFL <- rep(0.35, n0)
  }
  if (!exists("CC") | is.null(CC)) {
    CC <- rep(80, n0)
  }
  if (!exists("THETA") | is.null(THETA)) {
    THETA <- rep(0, n0)
  }
  if (!exists("ACCEL") | is.null(ACCEL)) {
    ACCEL <- rep(0, n0)
  }
  if (!exists("BUIEFF") | is.null(BUIEFF)) {
    BUIEFF <- rep(1, n0)
  }
  if (!exists("CBH") | is.null(CBH)) {
    CBH <- rep(0, n0)
  }
  if (!exists("CFL") | is.null(CFL)) {
    CFL <- rep(0, n0)
  }
  if (!exists("ISI") | is.null(ISI)) {
    ISI <- rep(0, n0)
  }
  # Convert Wind Direction from degress to radians
  WD <- WD * pi / 180
  # Convert Theta from degress to radians
  THETA <- THETA * pi / 180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi / 180
  ACCEL <- ifelse(is.na(ACCEL) | ACCEL < 0, 0, ACCEL)
  if (length(ACCEL[!ACCEL %in% c(0, 1)]) > 0) {
    warning("Input variable Accel is out of range, will be assigned to 1")
  }
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
  WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi, 0, WD)
  GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  CC <- ifelse(is.na(CC), 80, CC)
  GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35, GFL)
  LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  LAT <- ifelse(is.na(LAT), 55, LAT)
  LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  LONG <- ifelse(is.na(LONG), -120, LONG)
  THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA > 2 * pi, 0, THETA)
  SD <- ifelse(SD < 0 | SD > 1e+05, -999, SD)
  SD <- ifelse(is.na(SD), 0, SD)
  SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  SH <- ifelse(is.na(SH), 0, SH)

  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  if (length(FUELTYPE[is.na(FUELTYPE)]) > 0) {
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE <- ifelse(is.na(FUELTYPE), "C2", FUELTYPE)
  }
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Corrections
  ############################################################################
  # Convert hours to minutes
  HR <- HR * 60
  # Corrections to reorient Wind Azimuth(WAZ) and Uphill slode azimuth(SAZ)
  WAZ <- WD + pi
  WAZ <- ifelse(WAZ > 2 * pi, WAZ - 2 * pi, WAZ)
  SAZ <- ASPECT + pi
  SAZ <- ifelse(SAZ > 2 * pi, SAZ - 2 * pi, SAZ)
  # Any negative longitudes (western hemisphere) are translated to positive
  #  longitudes
  LONG <- ifelse(LONG < 0, -LONG, LONG)
  ############################################################################
  #                         END
  ############################################################################
  ############################################################################
  #                         START
  # Initializing variables
  ############################################################################
  SFC <- TFC <- HFI <- CFB <- ROS <- rep(0, length(LONG))
  RAZ <- rep(-999, length(LONG))
  validOutTypes <- c("SECONDARY", "ALL", "S", "A")
  # HACK: add options to ensure tests work for now
  validOutTypes <- c(validOutTypes, c("RAZ0", "WSV0"))
  if (output %in% validOutTypes) {
    FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <-
      BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- rep(
        0,
        length(LONG)
      )
    TI <- FTI <- BTI <- TTI <- LB <- WSV <- rep(-999, length(LONG))
  }
  CBH <- crown_base_height(FUELTYPE, CBH, SD, SH)
  CFL <- crown_fuel_load(FUELTYPE, CFL)
  D0 <- ifelse(D0 <= 0, foliar_moisture_content_minimum(LAT, LONG, ELV, DJ, D0), D0)
  FMC <- ifelse(
    FMC <= 0 | FMC > 120 | is.na(FMC),
    foliar_moisture_content(LAT, LONG, ELV, DJ, D0),
    FMC
  )

  FMC <- ifelse(FUELTYPE %in% c("D1", "S1", "S2", "S3", "O1A", "O1B"), 0, FMC)

  ############################################################################
  #                         END
  ############################################################################

  # Calculate Surface fuel consumption (SFC)
  SFC <- surface_fuel_consumption(FUELTYPE, FFMC, BUI, PC, GFL)
  # Disable BUI Effect if necessary
  BUI <- ifelse(BUIEFF != 1, 0, BUI)
  slope_values <- slope_adjustment(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
    FMC, SFC, PC, PDF, CC, CBH, ISI)
  # Calculate the net effective windspeed (WSV)
  WSV0 <- slope_values[["WSV"]]
  if ("WSV0" == output) {
    return(WSV0)
  }
  WSV <- ifelse(GS > 0 & FFMC > 0, WSV0, WS)
  # Calculate the net effective wind direction (RAZ)
  RAZ0 <- slope_values[["RAZ"]]
  if ("RAZ0" == output) {
    return(RAZ0)
  }
  RAZ <- ifelse(GS > 0 & FFMC > 0, RAZ0, WAZ)
  # Calculate or keep Initial Spread Index (ISI)
  ISI <- ifelse(ISI > 0, ISI, initial_spread_index(FFMC, WSV, TRUE))
  # HACK: C6 ROS depends on CFB so do this to not repeat calculations
  ros_vars <- rate_of_spread_extended(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
  ROS <- ros_vars$ROS
  CFB <- ifelse(CFL > 0, ros_vars$CFB, 0)
  CSI <- ros_vars$CSI
  RSO <- ros_vars$RSO
  # Calculate Total Fuel Consumption (TFC)
  TFC <- total_fuel_consumption(FUELTYPE, CFL, CFB, SFC, PC, PDF)
  # Calculate Head Fire Intensity(HFI)
  HFI <- fire_intensity(TFC, ROS)
  # Adjust Crown Fraction Burned
  CFB <- ifelse(HR < 0, -CFB, CFB)
  # Adjust RAZ
  RAZ <- RAZ * 180 / pi
  RAZ <- ifelse(RAZ == 360, 0, RAZ)
  # Calculate Fire Type (S = Surface, C = Crowning, I = Intermittent Crowning)
  FD <- rep("I", length(CFB))
  FD <- ifelse(CFB < 0.1, "S", FD)
  FD <- ifelse(CFB >= 0.9, "C", FD)
  # Calculate Crown Fuel Consumption(CFC)
  CFC <- total_fuel_consumption(
    FUELTYPE, CFL, CFB, SFC, PC, PDF,
    option = "CFC"
  )
  # Calculate the Secondary Outputs
  if (output %in% c("SECONDARY", "ALL", "S", "A")) {
    # Eq. 39 (FCFDG 1992) Calculate Spread Factor (GS is group slope)
    SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
    # Calculate The Buildup Effect
    BE <- buildup_effect(FUELTYPE, BUI)
    # Calculate length to breadth ratio
    LB <- length_to_breadth(FUELTYPE, WSV)
    LBt <- ifelse(
      ACCEL == 0,
      LB,
      length_to_breadth_at_time(FUELTYPE, LB, HR, CFB)
    )
    # Calculate Back fire rate of spread (BROS)
    BROS <- back_rate_of_spread(
      FUELTYPE, FFMC, BUI, WSV, FMC, SFC, PC, PDF, CC, CBH
    )
    # Calculate Flank fire rate of spread (FROS)
    FROS <- flank_rate_of_spread(ROS, BROS, LB)
    # Calculate the eccentricity
    E <- sqrt(1 - 1 / LB / LB)
    # Calculate the rate of spread towards angle theta (TROS)
    TROS <- ROS * (1 - E) / (1 - E * cos(THETA - RAZ))
    # Calculate rate of spread at time t for Flank, Back of fire and at angle
    # theta.
    ROSt <- ifelse(
      ACCEL == 0,
      ROS,
      rate_of_spread_at_time(FUELTYPE, ROS, HR, CFB)
    )
    BROSt <- ifelse(
      ACCEL == 0,
      BROS,
      rate_of_spread_at_time(FUELTYPE, BROS, HR, CFB)
    )
    FROSt <- ifelse(ACCEL == 0, FROS, flank_rate_of_spread(ROSt, BROSt, LBt))
    # Calculate rate of spread towards angle theta at time t (TROSt)
    TROSt <- ifelse(
      ACCEL == 0,
      TROS,
      (ROSt * (1 - sqrt(1 - 1 / LBt / LBt))
        / (1 - sqrt(1 - 1 / LBt / LBt) * cos(THETA - RAZ)))
    )
    # Calculate Crown Fraction Burned for Flank, Back of fire and angle theta.
    FCFB <- ifelse(
      CFL == 0,
      0,
      ifelse(FUELTYPE %in% c("C6"), 0, crown_fraction_burned(FROS, RSO))
    )
    BCFB <- ifelse(
      CFL == 0,
      0,
      ifelse(FUELTYPE %in% c("C6"), 0, crown_fraction_burned(BROS, RSO))
    )
    TCFB <- ifelse(
      CFL == 0,
      0,
      ifelse(FUELTYPE %in% c("C6"), 0, crown_fraction_burned(TROS, RSO))
    )
    # Calculate Total fuel consumption for the Flank fire, Back fire and at
    #  angle theta
    FTFC <- total_fuel_consumption(FUELTYPE, CFL, FCFB, SFC, PC, PDF)
    BTFC <- total_fuel_consumption(FUELTYPE, CFL, BCFB, SFC, PC, PDF)
    TTFC <- total_fuel_consumption(FUELTYPE, CFL, TCFB, SFC, PC, PDF)
    # Calculate the Fire Intensity at the Flank, Back and at angle theta fire
    FFI <- fire_intensity(FTFC, FROS)
    BFI <- fire_intensity(BTFC, BROS)
    TFI <- fire_intensity(TTFC, TROS)
    # Calculate Rate of spread at time t for the Head, Flank, Back of fire and
    #  at angle theta.
    HROSt <- ifelse(HR < 0, -ROSt, ROSt)
    FROSt <- ifelse(HR < 0, -FROSt, FROSt)
    BROSt <- ifelse(HR < 0, -BROSt, BROSt)
    TROSt <- ifelse(HR < 0, -TROSt, TROSt)

    # Calculate the elapsed time to crown fire initiation for Head, Flank, Back
    # fire and at angle theta. The (a# variable is a constant for Head, Flank,
    # Back and at angle theta used in the *TI equations)
    a1 <- 0.115 - (18.8 * CFB^2.5 * exp(-8 * CFB))
    TI <- log(ifelse(1 - RSO / ROS > 0, 1 - RSO / ROS, 1)) / (-a1)
    a2 <- 0.115 - (18.8 * FCFB^2.5 * exp(-8 * FCFB))
    FTI <- log(ifelse(1 - RSO / FROS > 0, 1 - RSO / FROS, 1)) / (-a2)
    a3 <- 0.115 - (18.8 * BCFB^2.5 * exp(-8 * BCFB))
    BTI <- log(ifelse(1 - RSO / BROS > 0, 1 - RSO / BROS, 1)) / (-a3)
    a4 <- 0.115 - (18.8 * TCFB^2.5 * exp(-8 * TCFB))
    TTI <- log(ifelse(1 - RSO / TROS > 0, 1 - RSO / TROS, 1)) / (-a4)

    # Fire spread distance for Head, Back, and Flank of fire
    DH <- ifelse(
      ACCEL == 1,
      distance_at_time(FUELTYPE, ROS, HR, CFB),
      ROS * HR
    )
    DB <- ifelse(
      ACCEL == 1,
      distance_at_time(FUELTYPE, BROS, HR, CFB),
      BROS * HR
    )
    DF <- ifelse(ACCEL == 1, (DH + DB) / (LBt * 2), (DH + DB) / (LB * 2))
  }
  # Create an id field if it does not exist
  if (!exists("ID") || is.null(ID)) {
    ID <- row.names(input)
  }
  # if Primary is selected, wrap the primary outputs into a data frame and
  #  return them
  if (output %in% c("PRIMARY", "P")) {
    FBP <- data.frame(ID, CFB, CFC, FD, HFI, RAZ, ROS, SFC, TFC)
    FBP[, c(2:3, 5:ncol(FBP))] <- apply(
      FBP[, c(2:3, 5:ncol(FBP))],
      2,
      function(.x) {
        ifelse(FUELTYPE %in% c("WA", "NF"), 0, .x)
      }
    )
    FBP[, "FD"] <- as.character(FBP[, "FD"])
    FBP[, "FD"] <- ifelse(FUELTYPE %in% c("WA", "NF"), "NA", FBP[, "FD"])
  } else if (output %in% c("SECONDARY", "S")) {
    # If Secondary is selected, wrap the secondary outputs into a data frame
    #  and return them.
    FBP <- data.frame(
      ID, BE, SF, ISI, FFMC, FMC, D0, RSO,
      CSI, FROS, BROS, HROSt, FROSt, BROSt, FCFB, BCFB,
      FFI, BFI, FTFC, BTFC, TI, FTI, BTI, LB, LBt, WSV,
      DH, DB, DF, TROS, TROSt, TCFB, TFI, TTFC, TTI
    )
    FBP[, 2:ncol(FBP)] <- apply(
      FBP[, 2:ncol(FBP)],
      2,
      function(.x) {
        ifelse(FUELTYPE %in% c("WA", "NF"), 0, .x)
      }
    )
  } else if (output %in% c("ALL", "A")) {
    # If all outputs are selected, then wrap all outputs into a data frame and
    # return it.
    FBP <- data.frame(
      ID, CFB, CFC, FD, HFI, RAZ, ROS, SFC,
      TFC, BE, SF, ISI, FFMC, FMC, D0, RSO, CSI, FROS,
      BROS, HROSt, FROSt, BROSt, FCFB, BCFB, FFI, BFI,
      FTFC, BTFC, TI, FTI, BTI, LB, LBt, WSV, DH, DB, DF,
      TROS, TROSt, TCFB, TFI, TTFC, TTI
    )
    FBP[, c(2:3, 5:ncol(FBP))] <- apply(
      FBP[, c(2:3, 5:ncol(FBP))],
      2,
      function(.x) {
        ifelse(FUELTYPE %in% c("WA", "NF"), 0, .x)
      }
    )
    FBP[, "FD"] <- as.character(FBP[, "FD"])
    FBP[, "FD"] <- ifelse(FUELTYPE %in% c("WA", "NF"), "NA", FBP[, "FD"])
  }
  return(FBP)
}

.FBPcalc <- function(...) {
  .Deprecated("fire_behaviour_prediction")
  return(fire_behaviour_prediction(...))
}
