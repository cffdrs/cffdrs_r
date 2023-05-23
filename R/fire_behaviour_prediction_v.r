library(future.apply)
plan(multisession)

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

FUEL_CROSS <- data.table(
  FUELTYPE0 = sort(c(
    paste("C", 1:7, sep = ""),
    "D1",
    paste("M", 1:4, sep = ""),
    paste("S", 1:3, sep = ""),
    "O1A", "O1B", "WA", "NF"
  )),
  code = 1:19
)

fix_fbp_input <- function(input, output = "Primary") {
  # if (is.null(input)) {
  #   # empty input defaults that differ from base default
  #   input <- data.table()
  #   input$CBH <- 3
  #   input$CFL <- 1
  # }
  input <- copy(input)
  names(input) <- toupper(names(input))
  ############################################################################
  #                         BEGIN
  # Set warnings for missing and required input variables.
  # Set defaults for inputs that are not already set.
  ############################################################################
  if (!("FUELTYPE" %in% names(input))) {
    warning(
      paste0(
        "FuelType is a required input,",
        " default FuelType = C2 is used in the calculation"
      )
    )
    input$FUELTYPE <- 2
  }
  if (!("FFMC" %in% names(input))) {
    warning(
      paste0(
        "FFMC is a required input, default FFMC = 90 is used in the",
        " calculation"
      )
    )
    input$FFMC <- 90
  }
  if (!("BUI" %in% names(input))) {
    warning(
      "BUI is a required input, default BUI = 60 is used in the calculation"
    )
    input$BUI <- 60
  }
  if (!("WS" %in% names(input))) {
    warning("WS is a required input, WS = 10 km/hr is used in the calculation")
    input$WS <- 10
  }
  if (!("GS" %in% names(input))) {
    warning("GS is a required input,GS = 0 is used in the calculation")
    input$GS <- 0
  }
  if (!("LAT" %in% names(input))) {
    warning(
      "LAT is a required input, default LAT=55 is used in the calculation"
    )
    input$LAT <- 55
  }
  if (!("LONG" %in% names(input))) {
    warning("LONG is a required input, LONG = -120 is used in the calculation")
    input$LONG <- -120
  }
  if (!("DJ" %in% names(input))) {
    warning("Dj is a required input, Dj = 180 is used in the calculation")
    input$DJ <- 180
  }
  if (!("ASPECT" %in% names(input))) {
    warning(
      "Aspect is a required input, Aspect = 0 is used in the calculation"
    )
    input$ASPECT <- 0
  }
  if (!("WD" %in% names(input))) {
    input$WD <- 0
  }
  if (!("FMC" %in% names(input))) {
    input$FMC <- 0
  }
  if (!("ELV" %in% names(input))) {
    input$ELV <- 0
  }
  if (!("SD" %in% names(input))) {
    input$SD <- 0
  }
  if (!("SH" %in% names(input))) {
    input$SH <- 0
  }
  if (!exists("D0") | is.null(D0)) {
    input$D0 <- 0
  }
  if (!("HR" %in% names(input))) {
    input$HR <- 1
  }
  if (!("PC" %in% names(input))) {
    input$PC <- 50
  }
  if (!("PDF" %in% names(input))) {
    input$PDF <- 35
  }
  if (!("GFL" %in% names(input))) {
    input$GFL <- 0.35
  }
  if (!("CC" %in% names(input))) {
    input$CC <- 80
  }
  if (!("THETA" %in% names(input))) {
    input$THETA <- 0
  }
  if (!("ACCEL" %in% names(input))) {
    input$ACCEL <- 0
  }
  if (!("BUIEFF" %in% names(input))) {
    input$BUIEFF <- 1
  }
  if (!("CBH" %in% names(input))) {
    input$CBH <- 0
  }
  if (!("CFL" %in% names(input))) {
    input$CFL <- 0
  }
  if (!("ISI" %in% names(input))) {
    input$ISI <- 0
  }
  # print(is.null(ID))
  if (!("ID" %in% names(input))) {
    # ID <- as.character(row.names(input))
    input$ID <- seq_len(ncell(r))
  }

  # ##### MOVE BOUNDS CHECKS FROM POINT FUNCTION ######
  # # print(WD)
  # output <- toupper(output)
  # # Convert Wind Direction from degress to radians
  # WD <- WD * pi / 180
  # # Convert Theta from degress to radians
  # THETA <- THETA * pi / 180
  # ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  # ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # # Convert Aspect from degress to radians
  # ASPECT <- ASPECT * pi / 180
  # if (!(all(ACCEL %in% c(0, 1)))) {
  #   warning("Input variable Accel is out of range, will be assigned to 1")
  #   ACCEL <- ifelse(ACCEL %in% c(0, 1), ACCEL, 1)
  # }
  # DJ <- ifelse(DJ < 0 | DJ > 366, 0, DJ)
  # DJ <- ifelse(is.na(DJ), 180, DJ)
  # D0 <- ifelse(is.na(D0) | D0 < 0 | D0 > 366, 0, D0)
  # ELV <- ifelse(ELV < 0 | ELV > 10000, 0, ELV)
  # ELV <- ifelse(is.na(ELV), 0, ELV)
  # BUIEFF <- ifelse(BUIEFF <= 0, 0, 1)
  # BUIEFF <- ifelse(is.na(BUIEFF), 1, BUIEFF)
  # HR <- ifelse(HR < 0, -HR, HR)
  # HR <- ifelse(HR > 366 * 24, 24, HR)
  # HR <- ifelse(is.na(HR), 0, HR)
  # FFMC <- ifelse(FFMC < 0 | FFMC > 101, 0, FFMC)
  # FFMC <- ifelse(is.na(FFMC), 90, FFMC)
  # ISI <- ifelse(is.na(ISI) | ISI < 0 | ISI > 300, 0, ISI)
  # BUI <- ifelse(BUI < 0 | BUI > 1000, 0, BUI)
  # BUI <- ifelse(is.na(BUI), 60, BUI)
  # WS <- ifelse(WS < 0 | WS > 300, 0, WS)
  # WS <- ifelse(is.na(WS), 10, WS)
  # WD <- ifelse(is.na(WD) | WD < -2 * pi | WD > 2 * pi, 0, WD)
  # GS <- ifelse(is.na(GS) | GS < 0 | GS > 200, 0, GS)
  # GS <- ifelse(ASPECT < -2 * pi | ASPECT > 2 * pi, 0, GS)
  # PC <- ifelse(is.na(PC) | PC < 0 | PC > 100, 50, PC)
  # PDF <- ifelse(is.na(PDF) | PDF < 0 | PDF > 100, 35, PDF)
  # CC <- ifelse(CC <= 0 | CC > 100, 95, CC)
  # CC <- ifelse(is.na(CC), 80, CC)
  # GFL <- ifelse(is.na(GFL) | GFL <= 0 | GFL > 100, 0.35, GFL)
  # LAT <- ifelse(LAT < -90 | LAT > 90, 0, LAT)
  # LAT <- ifelse(is.na(LAT), 55, LAT)
  # LONG <- ifelse(LONG < -180 | LONG > 360, 0, LONG)
  # LONG <- ifelse(is.na(LONG), -120, LONG)
  # THETA <- ifelse(is.na(THETA) | THETA < -2 * pi | THETA > 2 * pi, 0, THETA)
  # SD <- ifelse(SD < 0 | SD > 1e+05, -999, SD)
  # SD <- ifelse(is.na(SD), 0, SD)
  # SH <- ifelse(SH < 0 | SH > 100, -999, SH)
  # SH <- ifelse(is.na(SH), 0, SH)

  # if (NA %in% FUELTYPE) {
  #   warning("FuelType contains NA, using C2 (default) in the calculation")
  #   FUELTYPE <- ifelse(is.na(FUELTYPE), "C2", FUELTYPE)
  # }
  # FUELTYPE <- sub("-", "", FUELTYPE)
  # FUELTYPE <- sub(" ", "", FUELTYPE)
  ############################################################################
  #                         END
  ############################################################################

  ###################################################
  output_code <- ifelse(output %in% c("PRIMARY", "P"),
                        1,
                        ifelse(output %in% c("SECONDARY", "S"),
                                2,
                                ifelse(output %in% c("ALL", "A"),
                                       3,
                                       0)))

  FUELTYPE_CODE <- FUEL_CROSS[match(FUELTYPE, FUEL_CROSS$FUELTYPE0), code]

inp$FUELTYPE_CODE <- FUELTYPE
inp$FUELTYPE <- NULL
inp$output_code <- output_code

  # # print(ID)
  inp <- copy(input)
  inp$FUELTYPE_CODE <- FUELTYPE
  inp$ID <- ID
  # inp$FUELTYPE <- FUELTYPE
  inp$FUELTYPE_CODE <- FUELTYPE_CODE
  inp$ACCEL <- ACCEL
  inp$DJ <- DJ
  inp$D0 <- D0
  inp$ELV <- ELV
  inp$BUIEFF <- BUIEFF
  inp$HR <- HR
  inp$FFMC <- FFMC
  inp$ISI <- ISI
  inp$BUI <- BUI
  inp$WS <- WS
  inp$WD <- WD
  inp$GS <- GS
  inp$ASPECT <- ASPECT
  inp$PC <- PC
  inp$PDF <- PDF
  inp$CC <- CC
  inp$GFL <- GFL
  inp$CBH <- CBH
  inp$CFL <- CFL
  inp$LAT <- LAT
  inp$LONG <- LONG
  inp$FMC <- FMC
  inp$THETA <- THETA
  inp$SD <- SD
  inp$SH <- SH
  inp$output_code <- rep(output_code, n0)

return(inp)
}

fbp_v <- function(input = NULL, output = "Primary") {
  #  Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  # print("Here")
  # print(input)
  if (is.null(input)) {
    # empty input defaults that differ from base default
    input <- data.table()
    input$CBH <- 3
    input$CFL <- 1
  }
  names(input) <- toupper(names(input))
  # print(names(input))
  # HACK: set all of these to NULL so we don't use global variables
  ID <- NULL
  # print(ID)
  FFMC <- NULL
  BUI <- NULL
  WS <- NULL
  WD <- NULL
  FMC <- NULL
  GS <- NULL
  LAT <- NULL
  LONG <- NULL
  ELV <- NULL
  DJ <- NULL
  D0 <- NULL
  SD <- NULL
  SH <- NULL
  HR <- NULL
  PC <- NULL
  PDF <- NULL
  GFL <- NULL
  CC <- NULL
  THETA <- NULL
  ACCEL <- NULL
  ASPECT <- NULL
  BUIEFF <- NULL
  CBH <- NULL
  CFL <- NULL
  ISI <- NULL
  # print(input)
  ID <- input$ID
  # print(ID)
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
  if (!exists("FUELTYPE") | is.null(FUELTYPE)) {
    warning(
      paste0(
        "FuelType is a required input,",
        " default FuelType = C2 is used in the calculation"
      )
    )
    FUELTYPE <- rep("C2", n0)
  }
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
  # print(is.null(ID))
  if (is.null(ID)) {
    # ID <- as.character(row.names(input))
    ID <- row.names(input)
  }

  ##### MOVE BOUNDS CHECKS FROM POINT FUNCTION ######
  # print(WD)
  output <- toupper(output)
  # Convert Wind Direction from degress to radians
  WD <- WD * pi / 180
  # Convert Theta from degress to radians
  THETA <- THETA * pi / 180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi / 180
  if (!(all(ACCEL %in% c(0, 1)))) {
    warning("Input variable Accel is out of range, will be assigned to 1")
    ACCEL <- ifelse(ACCEL %in% c(0, 1), ACCEL, 1)
  }
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

  if (NA %in% FUELTYPE) {
    warning("FuelType contains NA, using C2 (default) in the calculation")
    FUELTYPE <- ifelse(is.na(FUELTYPE), "C2", FUELTYPE)
  }
  FUELTYPE <- sub("-", "", FUELTYPE)
  FUELTYPE <- sub(" ", "", FUELTYPE)
  ############################################################################
  #                         END
  ############################################################################

  ###################################################
  output_code <- ifelse(output %in% c("PRIMARY", "P"),
                        1,
                        ifelse(output %in% c("SECONDARY", "S"),
                                2,
                                ifelse(output %in% c("ALL", "A"),
                                       3,
                                       0)))

  FUELTYPE_CODE <- FUEL_CROSS[match(FUELTYPE, FUEL_CROSS$FUELTYPE0), code]

  # # print(ID)
  inp <- data.table()
  inp$ID <- ID
  # inp$FUELTYPE <- FUELTYPE
  inp$FUELTYPE_CODE <- FUELTYPE_CODE
  inp$ACCEL <- ACCEL
  inp$DJ <- DJ
  inp$D0 <- D0
  inp$ELV <- ELV
  inp$BUIEFF <- BUIEFF
  inp$HR <- HR
  inp$FFMC <- FFMC
  inp$ISI <- ISI
  inp$BUI <- BUI
  inp$WS <- WS
  inp$WD <- WD
  inp$GS <- GS
  inp$ASPECT <- ASPECT
  inp$PC <- PC
  inp$PDF <- PDF
  inp$CC <- CC
  inp$GFL <- GFL
  inp$CBH <- CBH
  inp$CFL <- CFL
  inp$LAT <- LAT
  inp$LONG <- LONG
  inp$FMC <- FMC
  inp$THETA <- THETA
  inp$SD <- SD
  inp$SH <- SH
  inp$output_code <- rep(output_code, n0)


  # r <- NULL
  # for (i in 1:nrow(inp)) {
  #   x <- inp[i, ]
  #   r <- rbind(r,
  #   fire_behaviour_prediction_v(
  #     x$ID, x$FUELTYPE_CODE, x$ACCEL, x$DJ, x$D0, x$ELV, x$BUIEFF, x$HR, x$FFMC,
  #     x$ISI, x$BUI, x$WS, x$WD, x$GS, x$ASPECT, x$PC, x$PDF, x$CC, x$GFL, x$CBH,
  #     x$CFL, x$LAT, x$LONG, x$FMC, x$THETA, x$SD, x$SH, x$output_code))
  # }

  r <- NULL
  for (i in 1:nrow(inp)) {
    x <- inp[i, ]
    r <- rbind(r,
    fire_behaviour_prediction_v(
      x$ID, x$FUELTYPE_CODE, x$ACCEL, x$DJ, x$D0, x$ELV, x$BUIEFF, x$HR, x$FFMC,
      x$ISI, x$BUI, x$WS, x$WD, x$GS, x$ASPECT, x$PC, x$PDF, x$CC, x$GFL, x$CBH,
      x$CFL, x$LAT, x$LONG, x$FMC, x$THETA, x$SD, x$SH, x$output_code))
  }

inp[, fbp_vect(
      ID, FUELTYPE_CODE, ACCEL, DJ, D0, ELV, BUIEFF, HR, FFMC,
      ISI, BUI, WS, WD, GS, ASPECT, PC, PDF, CC, GFL, CBH,
      CFL, LAT, LONG, FMC, THETA, SD, SH, output_code), by=seq_len(nrow(inp))]

  return(r)
  # return(apply(inp, 1, fbp_call))

  # r <- NULL
  # for (i in 1:nrow(inp)) {
  #   x <- as.list(inp[i, ])
  #   # v <- cffdrs:::fire_behaviour_prediction_v(
  #   #     ID=x[1],
  #   #     FUELTYPE_CODE=x[2],
  #   #     ACCEL=x[3],
  #   #     DJ=x[4],
  #   #     D0=x[5],
  #   #     ELV=x[6],
  #   #     BUIEFF=x[7],
  #   #     HR=x[8],
  #   #     FFMC=x[9],
  #   #     ISI=x[10],
  #   #     BUI=x[11],
  #   #     WS=x[12],
  #   #     WD=x[13],
  #   #     GS=x[14],
  #   #     ASPECT=x[15],
  #   #     PC=x[16],
  #   #     PDF=x[17],
  #   #     CC=x[18],
  #   #     GFL=x[19],
  #   #     CBH=x[20],
  #   #     CFL=x[21],
  #   #     LAT=x[22],
  #   #     LONG=x[23],
  #   #     FMC=x[24],
  #   #     THETA=x[25],
  #   #     SD=x[26],
  #   #     SH=x[27],
  #   #     output_code=x[28])
  #   # v <- fire_behaviour_prediction_v(
  #   #   x$ID, x$FUELTYPE_CODE, x$ACCEL, x$DJ, x$D0, x$ELV, x$BUIEFF, x$HR, x$FFMC,
  #   #   x$ISI, x$BUI, x$WS, x$WD, x$GS, x$ASPECT, x$PC, x$PDF, x$CC, x$GFL, x$CBH,
  #   #   x$CFL, x$LAT, x$LONG, x$FMC, x$THETA, x$SD, x$SH, x$output_code)
  #   # r <- rbind(r, v)
  #   r <- rbind(r,
  #   fire_behaviour_prediction_v(
  #     x$ID, x$FUELTYPE_CODE, x$ACCEL, x$DJ, x$D0, x$ELV, x$BUIEFF, x$HR, x$FFMC,
  #     x$ISI, x$BUI, x$WS, x$WD, x$GS, x$ASPECT, x$PC, x$PDF, x$CC, x$GFL, x$CBH,
  #     x$CFL, x$LAT, x$LONG, x$FMC, x$THETA, x$SD, x$SH, x$output_code))
  #   # r <- rbind(r, fbp_call(as.list(inp[i, ])))
  # }
  return(r)
  # fbp_vect <- Vectorize(fire_behaviour_prediction_v, SIMPLIFY=TRUE)

  # fct <- function(x) {
  #   return(fire_behaviour_prediction_v(
  #     x$ID, x$FUELTYPE_CODE, x$ACCEL, x$DJ, x$D0, x$ELV, x$BUIEFF, x$HR, x$FFMC,
  #     x$ISI, x$BUI, x$WS, x$WD, x$GS, x$ASPECT, x$PC, x$PDF, x$CC, x$GFL, x$CBH,
  #     x$CFL, x$LAT, x$LONG, x$FMC, x$THETA, x$SD, x$SH, output_code))
  # }
  # fct <- function(x) { do.call(fbp_vect, x)}
  # future_apply(inp, 1, fct, future.packages="data.table")
  # future_apply(inp, 1, fbp_vect, future.packages="data.table")

  # future_apply(inp, 1, fbp_call, future.packages=c("cffdrs", "data.table"))

  # r <- future_apply(inp, 1, fire_behaviour_prediction_v, future.packages="data.table")


  # # inp$output <- output
  # inp[, ..I := .I]
  # r <- inp[, as.list(do.call(fire_behaviour_prediction_v, .SD)), by=..I]
  # r$..I <- NULL
  # apply(inp, 1, fire_behaviour_prediction_v)
  # r <- inp[, Map(fire_behaviour_prediction_v,
  #   ID,
  #   FUELTYPE_CODE,
  #   ACCEL,
  #   DJ,
  #   D0,
  #   ELV,
  #   BUIEFF,
  #   HR,
  #   FFMC,
  #   ISI,
  #   BUI,
  #   WS,
  #   WD,
  #   GS,
  #   ASPECT,
  #   PC,
  #   PDF,
  #   CC,
  #   GFL,
  #   CBH,
  #   CFL,
  #   LAT,
  #   LONG,
  #   FMC,
  #   THETA,
  #   SD,
  #   SH,
  #   output_code
  # ), by=1:nrow(inp)]
  # # r <- mapply(inp, fire_behaviour_prediction)

  # r <- apply(inp, 1, function(x){x[1]})


  # r <- apply(inp,
  # 1,
  # function(x) {
  #   do.call(fire_behaviour_prediction_v,
  #   list(
  #     x["ID"],
  #     x["FUELTYPE_CODE"],
  #     x["ACCEL"],
  #     x["DJ"],
  #     x["D0"],
  #     x["ELV"],
  #     x["BUIEFF"],
  #     x["HR"],
  #     x["FFMC"],
  #     x["ISI"],
  #     x["BUI"],
  #     x["WS"],
  #     x["WD"],
  #     x["GS"],
  #     x["ASPECT"],
  #     x["PC"],
  #     x["PDF"],
  #     x["CC"],
  #     x["GFL"],
  #     x["CBH"],
  #     x["CFL"],
  #     x["LAT"],
  #     x["LONG"],
  #     x["FMC"],
  #     x["THETA"],
  #     x["SD"],
  #     x["SH"],
  #     x["output_code"]
  #   ))
  # })

  # r <- inp[, as.list(do.call(fire_behaviour_prediction_v, .SD)), by=1:nrow(inp)]
  # r$nrow <- NULL
  # r <- apply(inp,
  # 1,
  # function(x) {
  #   do.call(fire_behaviour_prediction_v,
  #   c(
  #     x[1],
  #     x[2],
  #     x[3],
  #     x[4],
  #     x[5],
  #     x[6],
  #     x[7],
  #     x[8],
  #     x[9],
  #     x[10],
  #     x[11],
  #     x[12],
  #     x[13],
  #     x[14],
  #     x[15],
  #     x[16],
  #     x[17],
  #     x[18],
  #     x[19],
  #     x[20],
  #     x[21],
  #     x[22],
  #     x[23],
  #     x[24],
  #     x[25],
  #     x[26],
  #     x[27],
  #     x[28]
  #   ))
  # })

  # r <- fbp_vect(
  #   ID,
  #   FUELTYPE_CODE,
  #   ACCEL,
  #   DJ,
  #   D0,
  #   ELV,
  #   BUIEFF,
  #   HR,
  #   FFMC,
  #   ISI,
  #   BUI,
  #   WS,
  #   WD,
  #   GS,
  #   ASPECT,
  #   PC,
  #   PDF,
  #   CC,
  #   GFL,
  #   CBH,
  #   CFL,
  #   LAT,
  #   LONG,
  #   FMC,
  #   THETA,
  #   SD,
  #   SH,
  #   rep(output_code, n0)
  # )
  # r <- t(r)
  r <- as.data.table(r)
  # # HACK: just do this for now
  # for (col in names(r)) {
  #   r[, c(col)] <- unlist(as.data.frame(r)[col])
  # }
  r$FD <- chartr("123", "SIC", as.character(r$FD))
  # r <- rbindlist(apply(inp, 1, fire_behaviour_prediction_v))
  return(r)
}

fire_behaviour_prediction_v <- function(
    # SD = 0,
    # SH = 0,
    # FUELTYPE = "C2",
    # ACCEL = 0,
    # DJ = 180,
    # D0 = 0,
    # ELV = 0,
    # BUIEFF = 1,
    # HR = 1,
    # FFMC = 90,
    # ISI = 0,
    # BUI = 60,
    # WS = 10,
    # WD = 0,
    # GS = 0,
    # ASPECT = 0,
    # PC = 50,
    # PDF = 35,
    # CC = 80,
    # GFL = 0.35,
    # CBH = 0,
    # CFL = 0,
    # LAT = 55,
    # LONG = -120,
    # FMC = 0,
    # THETA = 0,
  ID,
  FUELTYPE_CODE,
  ACCEL,
  DJ,
  D0,
  ELV,
  BUIEFF,
  HR,
  FFMC,
  ISI,
  BUI,
  WS,
  WD,
  GS,
  ASPECT,
  PC,
  PDF,
  CC,
  GFL,
  CBH,
  CFL,
  LAT,
  LONG,
  FMC,
  THETA,
  SD,
  SH,
  output_code = 1) {
#     print(ID)
# print(FUELTYPE_CODE)
# print(ACCEL)
# print(DJ)
# print(D0)
# print(ELV)
# print(BUIEFF)
# print(HR)
# print(FFMC)
# print(ISI)
# print(BUI)
# print(WS)
# print(WD)
# print(GS)
# print(ASPECT)
# print(PC)
# print(PDF)
# print(CC)
# print(GFL)
# print(CBH)
# print(CFL)
# print(LAT)
# print(LONG)
# print(FMC)
# print(THETA)
# print(SD)
# print(SH)
# print(output_code)
  # TRANSLATE OUTPUT INTO NUMBERS:
  # PRIMARY = 1
  # SECONDARY = 2
  # ALL = 3
  # # print(WD)
  # output <- toupper(output)
  # # Convert Wind Direction from degress to radians
  WD <- WD * pi / 180
  # Convert Theta from degress to radians
  THETA <- THETA * pi / 180
  ASPECT <- ifelse(is.na(ASPECT), 0, ASPECT)
  ASPECT <- ifelse(ASPECT < 0, ASPECT + 360, ASPECT)
  # Convert Aspect from degress to radians
  ASPECT <- ASPECT * pi / 180
  # # if (!(0 == ACCEL || 1 == ACCEL)) {
  # if (!(0 == ACCEL | 1 == ACCEL)) {
  #   warning("Input variable Accel is out of range, will be assigned to 1")
  #   ACCEL <- 1
  # }
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

  # if (is.na(FUELTYPE)) {
  #   warning("FuelType contains NA, using C2 (default) in the calculation")
  #   FUELTYPE <- "C2"
  # }
  # FUELTYPE <- sub("-", "", FUELTYPE)
  # FUELTYPE <- sub(" ", "", FUELTYPE)
  # ############################################################################
  # #                         END
  # ############################################################################
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
  SFC <- TFC <- HFI <- CFB <- ROS <- 0
  RAZ <- -999
  # doesn't matter if we want these outputs, we can set them anyway
  if (output_code > 1) {
  # if (output %in% c("SECONDARY", "ALL", "S", "A")) {
    FROS <- BROS <- TROS <- HROSt <- FROSt <- BROSt <- TROSt <- FCFB <-
      BCFB <- TCFB <- FFI <- BFI <- TFI <- FTFC <- BTFC <- TTFC <- 0
    TI <- FTI <- BTI <- TTI <- LB <- WSV <- -999
  }
  FUELTYPE <- FUEL_CROSS[match(FUELTYPE_CODE, FUEL_CROSS$code), FUELTYPE0]
  # FUEL_CODES <- c(
  #     "C1", "C2", "C3", "C4", "C5", "C6", "C7",
  #     "D1", "M1", "M2", "M3", "M4", "NF",
  #     "O1A", "O1B", "S1", "S2", "S3", "WA"
  # )
  # FUELTYPE <- FUEL_CODES[FUELTYPE_CODE]
  if (FUELTYPE %in% c("WA", "NF")) {
  # if (FUELTYPE_CODE %in% c(13, 19)) {
    FD <- NA
  } else {
    # CBHs <- c(
    #   2, 3, 8, 4, 18, 7, 10,
    #   0, 6, 6, 6, 6, NA,
    #   0, 0, 0, 0, 0, NA
    # )
    # names(CBHs) <- c(
    #   "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    #   "D1", "M1", "M2", "M3", "M4", "NF",
    #   "O1A", "O1B", "S1", "S2", "S3", "WA"
    # )
    CBHs <- c(
      2, 3, 8, 4, 18, 7, 10,
      0, 6, 6, 6, 6, 0, 0, 0, 0, 0
    )
    names(CBHs) <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )

    CBH <- ifelse(
      CBH <= 0 | CBH > 50 | is.na(CBH),
      ifelse(
        FUELTYPE %in% c("C6") & SD > 0 & SH > 0,
        # 6 == FUELTYPE_CODE & SD > 0 & SH > 0,
        -11.2 + 1.06 * SH + 0.0017 * SD,
        CBHs[FUELTYPE]
        # CBHs[FUELTYPE_CODE]
      ),
      CBH
    )
    CBH <- ifelse(CBH < 0, 1e-07, CBH)
    CFLs <- c(
      0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5,
      0, 0.8, 0.8, 0.8, 0.8, 0, 0, 0, 0, 0
    )
    names(CFLs) <- c(
      "C1", "C2", "C3", "C4", "C5", "C6", "C7",
      "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1A", "O1B"
    )
    # CFLs <- c(
    #   0.75, 0.8, 1.15, 1.2, 1.2, 1.8, 0.5,
    #   0, 0.8, 0.8, 0.8, 0.8, NA,
    #   0, 0, 0, 0, 0, NA
    # )
    # names(CFLs) <- c(
    #   "C1", "C2", "C3", "C4", "C5", "C6", "C7",
    #   "D1", "M1", "M2", "M3", "M4", "NF",
    #    "O1A", "O1B", "S1", "S2", "S3", "WA"
    # )
    CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), CFLs[FUELTYPE], CFL)
    # CFL <- ifelse(CFL <= 0 | CFL > 2 | is.na(CFL), CFLs[FUELTYPE_CODE], CFL)
    FMC <- ifelse(
      FMC <= 0 | FMC > 120 | is.na(FMC),
      foliar_moisture_content(LAT, LONG, ELV, DJ, D0),
      FMC
    )
    FMC <- ifelse(FUELTYPE %in% c("D1", "S1", "S2", "S3", "O1A", "O1B"), 0, FMC)
    # FMC <- ifelse(FUELTYPE_CODE %in% c(8, 16, 17, 18, 14, 15), 0, FMC)
    ############################################################################
    #                         END
    ############################################################################

    # Calculate Surface fuel consumption (SFC)
    SFC <- surface_fuel_consumption(FUELTYPE, FFMC, BUI, PC, GFL)
    # Disable BUI Effect if necessary
    BUI <- ifelse(BUIEFF != 1, 0, BUI)
    # Calculate the net effective windspeed (WSV)
    WSV0 <- .Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
      FMC, SFC, PC, PDF, CC, CBH, ISI,
      output = "WSV"
    )
    WSV <- ifelse(GS > 0 & FFMC > 0, WSV0, WS)
    # Calculate the net effective wind direction (RAZ)
    RAZ0 <- .Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS, SAZ,
      FMC, SFC, PC, PDF, CC, CBH, ISI,
      output = "RAZ"
    )
    RAZ <- ifelse(GS > 0 & FFMC > 0, RAZ0, WAZ)
    # Calculate or keep Initial Spread Index (ISI)
    ISI <- ifelse(ISI > 0, ISI, initial_spread_index(FFMC, WSV, TRUE))
    # Calculate the Rate of Spread (ROS), C6 has different calculations
    ROS <- ifelse(
      FUELTYPE %in% c("C6"),
      .C6calc(FUELTYPE, ISI, BUI, FMC, SFC, CBH, option = "ROS"),
      rate_of_spread(FUELTYPE, ISI, BUI, FMC, SFC, PC, PDF, CC, CBH)
    )
    # Calculate Crown Fraction Burned (CFB), C6 has different calculations
    CFB <- ifelse(
      FUELTYPE %in% c("C6"),
      .C6calc(FUELTYPE, ISI, BUI, FMC, SFC, CBH, option = "CFB"),
      ifelse(CFL > 0, .CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH), 0)
    )
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
    # FBP$FD <- as.integer(chartr("SIC", "123", FBP$FD))
    FD <- 2
    FD <- ifelse(CFB < 0.1, 1, FD)
    FD <- ifelse(CFB >= 0.9, 3, FD)
    # FD <- "I"
    # FD <- ifelse(CFB < 0.1, "S", FD)
    # FD <- ifelse(CFB >= 0.9, "C", FD)
    # Calculate Crown Fuel Consumption(CFC)
    CFC <- total_fuel_consumption(
      FUELTYPE, CFL, CFB, SFC, PC, PDF,
      option = "CFC"
    )
    # Calculate the Secondary Outputs
    if (1 < output_code) {
      # Eq. 39 (FCFDG 1992) Calculate Spread Factor (GS is group slope)
      SF <- ifelse(GS >= 70, 10, exp(3.533 * (GS / 100)^1.2))
      # Calculate Critical Surface Intensity
      CSI <- .CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, option = "CSI")
      # Calculate Surface fire rate of spread (m/min)
      RSO <- .CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH, option = "RSO")
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
        ifelse(FUELTYPE %in% c("C6"), 0, .CFBcalc(FUELTYPE, FMC, SFC, FROS, CBH))
      )
      BCFB <- ifelse(
        CFL == 0,
        0,
        ifelse(FUELTYPE %in% c("C6"), 0, .CFBcalc(FUELTYPE, FMC, SFC, BROS, CBH))
      )
      TCFB <- ifelse(
        CFL == 0,
        0,
        ifelse(FUELTYPE %in% c("C6"), 0, .CFBcalc(FUELTYPE, FMC, SFC, TROS, CBH))
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
  }
  # FD <- as.character(FD)
#   if (FUELTYPE %in% c("WA", "NF"))
#   {
#     CFB <- 0.0
#     CFC <- 0.0
#     FD <- "NA"
#     HFI <- 0.0
#     RAZ <- 0.0
#     ROS <- 0.0
#     SFC <- 0.0
#     TFC <- 0.0
#     BE <- 0.0
#     SF <- 0.0
#     ISI <- 0.0
#     FFMC <- 0.0
#     FMC <- 0.0
#     D0 <- 0
#     RSO <- 0.0
#     CSI <- 0.0
#     FROS <- 0.0
#     BROS <- 0.0
#     HROSt <- 0.0
#     FROSt <- 0.0
#     BROSt <- 0.0
#     FCFB <- 0.0
#     BCFB <- 0.0
#     FFI <- 0.0
#     BFI <- 0.0
#     FTFC <- 0.0
#     BTFC <- 0.0
#     TI <- 0.0
#     FTI <- 0.0
#     BTI <- 0.0
#     LB <- 0.0
#     LBt <- 0.0
#     WSV <- 0.0
#     DH <- 0.0
#     DB <- 0.0
#     DF <- 0.0
#     TROS <- 0.0
#     TROSt <- 0.0
#     TCFB <- 0.0
#     TFI <- 0.0
#     TTFC <- 0.0
#     TTI <- 0.0
# }
  # if Primary is selected, wrap the primary outputs into a data frame and
  #  return them

  FBP <- c(
    ID=ID,
    CFB=CFB
  )
  # FBP <- t(FBP)

  # if (1 == output_code) {
  #   FBP <- c(
  #     "ID"=ID,
  #     "CFB"=CFB,
  #     "CFC"=CFC,
  #     "FD"=FD,
  #     "HFI"=HFI,
  #     "RAZ"=RAZ,
  #     "ROS"=ROS,
  #     "SFC"=SFC,
  #     "TFC"=TFC
  #     )
  # } else if (2 == output_code) {
  #   # If Secondary is selected, wrap the secondary outputs into a data frame
  #   #  and return them.
  #   FBP <- c(
  #     "ID"=ID,
  #     "BE"=BE,
  #     "SF"=SF,
  #     "ISI"=ISI,
  #     "FFMC"=FFMC,
  #     "FMC"=FMC,
  #     "D0"=D0,
  #     "RSO"=RSO,
  #     "CSI"=CSI,
  #     "FROS"=FROS,
  #     "BROS"=BROS,
  #     "HROSt"=HROSt,
  #     "FROSt"=FROSt,
  #     "BROSt"=BROSt,
  #     "FCFB"=FCFB,
  #     "BCFB"=BCFB,
  #     "FFI"=FFI,
  #     "BFI"=BFI,
  #     "FTFC"=FTFC,
  #     "BTFC"=BTFC,
  #     "TI"=TI,
  #     "FTI"=FTI,
  #     "BTI"=BTI,
  #     "LB"=LB,
  #     "LBt"=LBt,
  #     "WSV"=WSV,
  #     "DH"=DH,
  #     "DB"=DB,
  #     "DF"=DF,
  #     "TROS"=TROS,
  #     "TROSt"=TROSt,
  #     "TCFB"=TCFB,
  #     "TFI"=TFI,
  #     "TTFC"=TTFC,
  #     "TTI"=TTI
  #   )
  # } else if (3 == output_code) {
  #   FBP <- c(
  #     ID=ID,
  #     CFB=CFB,
  #     CFC=CFC,
  #     FD=FD,
  #     HFI=HFI,
  #     RAZ=RAZ,
  #     ROS=ROS,
  #     SFC=SFC,
  #     TFC=TFC,
  #     BE=BE,
  #     SF=SF,
  #     ISI=ISI,
  #     FFMC=FFMC,
  #     FMC=FMC,
  #     D0=D0,
  #     RSO=RSO,
  #     CSI=CSI,
  #     FROS=FROS,
  #     BROS=BROS,
  #     HROSt=HROSt,
  #     FROSt=FROSt,
  #     BROSt=BROSt,
  #     FCFB=FCFB,
  #     BCFB=BCFB,
  #     FFI=FFI,
  #     BFI=BFI,
  #     FTFC=FTFC,
  #     BTFC=BTFC,
  #     TI=TI,
  #     FTI=FTI,
  #     BTI=BTI,
  #     LB=LB,
  #     LBt=LBt,
  #     WSV=WSV,
  #     DH=DH,
  #     DB=DB,
  #     DF=DF,
  #     TROS=TROS,
  #     TROSt=TROSt,
  #     TCFB=TCFB,
  #     TFI=TFI,
  #     TTFC=TTFC,
  #     TTI=TTI
  #   )
  # }
  # FBP <- unlist(as.list(FBP))
  # FBP <- data.table(FBP)
  # FBP <- t(FBP)
  # # HACK: just do this for now
  # FBP$ID <- as.character(FBP$ID)
  return(FBP)
}

fbp_vect <- Vectorize(fire_behaviour_prediction_v)
fbp_call <- function(x) {
  return(cffdrs:::fire_behaviour_prediction_v(
  # return(cffdrs:::fbp_vect(
    ID=x[1],
    FUELTYPE_CODE=x[2],
    ACCEL=x[3],
    DJ=x[4],
    D0=x[5],
    ELV=x[6],
    BUIEFF=x[7],
    HR=x[8],
    FFMC=x[9],
    ISI=x[10],
    BUI=x[11],
    WS=x[12],
    WD=x[13],
    GS=x[14],
    ASPECT=x[15],
    PC=x[16],
    PDF=x[17],
    CC=x[18],
    GFL=x[19],
    CBH=x[20],
    CFL=x[21],
    LAT=x[22],
    LONG=x[23],
    FMC=x[24],
    THETA=x[25],
    SD=x[26],
    SH=x[27],
    output_code=x[28]))
}
