#' @title Grass Fuel Moisture Raster Calculation
#' 
#' @description Calculation of the Grass Fuel Moisture Code. This calculates the
#' moisture content of both the surface of a fully cured matted grass layer and 
#' also an equivalent Grass Fuel Moisture Code. All equations come from Wotton 
#' (2009) as cited below unless otherwise specified.
#' 
#' @references 
#' Wotton, B.M. 2009. A grass moisture model for the Canadian
#' Forest Fire Danger Rating System. In: Proceedings 8th Fire and
#' Forest Meteorology Symposium, Kalispell, MT Oct 13-15, 2009.
#' Paper 3-2. \url{https://ams.confex.com/ams/pdfpapers/155930.pdf}
#' 
#' @param input [raster stack]
#' \tabular{lll}{
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh}   \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws}   \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr
#' \var{isol} \tab (required) \tab Solar radiation (kW/m^2)\cr }
#' @param GFMCold    GFMC from yesterday (double, default=85)
#' @param time.step  The hourly time steps (integer hour, default=1)
#' @param roFL       Nominal fuel load of the fine fuel layer (kg/m^2 double, default=0.3)
#' @param out        Output format (GFMCandMC/MC/GFMC/ALL, default=GFMCandMC)
#' 
#' @return Returns a raster stack of either MC, GMFC, GFMC and MC or All
#' 
#' @export gfmcRaster
#' 


gfmcRaster <- function(input, GFMCold = 85, time.step = 1, roFL = 0.3,
                 out = "GFMCandMC") {

  t0 <- time.step ## Currently locked at 1
  names(input) <- tolower(names(input))
  #Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    warning("Attached dataset 'input' is being detached to use fbp() function.")
    detach(input)
  }
  #set local scope variables
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  isol <- input$isol

  #show warnings when inputs are missing
  if (!exists("temp") | is.null(temp))
    warning("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec))
    warning("precipitation (prec) is missing!")
  if (!exists("ws") | is.null(ws))
    warning("wind speed (ws) is missing!")
  if (!exists("rh") | is.null(rh))
    warning("relative humidity (rh) is missing!")
  if (!exists("isol") | is.null(isol))
    warning("ISOL is missing!")

  if (is.numeric(GFMCold) & length(GFMCold) == 1){
    warning("Single GFMCold value for grid is applied to the whole grid")
    GFMCold <- setValues(temp, GFMCold)
  }

  validOutTypes = c("GFMCandMC", "MC", "GFMC", "ALL")
  if(!(out %in% validOutTypes)){
    stop(paste("'",out, "' is an invalid 'out' type.", sep=""))
  }

  #get the length of the data stream

  GFMC <- NULL
  MC <- NULL
  #iterate through timesteps
  #Eq. 13 - Calculate previous moisture code
  MCold <- 147.2772 * ((101 - GFMCold) / (59.5 + GFMCold))
  #Eq. 11 - Calculate the moisture content of the layer in % after rainfall
  MCr <- MCold
  MCr[prec>0] <- MCold[prec>0] + 100 * (prec[prec>0] / roFL)
  #Constrain to 250
  MCr[MCr > 250] <- 250
  MCold <- MCr
  #Eq. 2 - Calculate Fuel temperature
  Tf <- temp + 35.07 * isol * exp(-0.06215 * ws)
  #Eq. 3 - Calculate Saturation Vapour Pressure (Baumgartner et a. 1982)
  eS.T <- 6.107 * 10^(7.5 * temp / (237 + temp))
  #Eq. 3 for Fuel temperature
  eS.Tf <- 6.107 * 10^(7.5 * Tf / (237 + Tf))
  #Eq. 4 - Calculate Fuel Level Relative Humidity
  RH.f <- rh * (eS.T / eS.Tf)
  #Eq. 7 - Calculate Equilibrium Moisture Content for Drying phase
  EMC.D <- (1.62 * RH.f^0.532 + 13.7 * exp((RH.f - 100) / 13.0)) +
   0.27 * (26.7 - Tf) * (1 - exp(-0.115 * RH.f))
  #Eq. 7 - Calculate Equilibrium Moisture Content for Wetting phase
  EMC.W <- (1.42 * RH.f^0.512 + 12.0 * exp((RH.f - 100) / 18.0)) +
   0.27 * (26.7 - Tf) * (1 - exp(-0.115 * RH.f))
  #RH in terms of RH/100 for desorption
  Rf <- rh
  Rf[MCold > EMC.D] <- RH.f[MCold > EMC.D] / 100
  #RH in terms of 1-RH/100 for absorption
  Rf[MCold < EMC.W] <- (100 - RH.f[MCold < EMC.W]) / 100
  #Eq. 10 - Calculate Inverse Response time of grass (hours)
  K.GRASS <- 0.389633 * exp(0.0365 * Tf) * (0.424 * (1 - Rf^1.7) + 0.0694 *
                                             sqrt(ws) * (1 - Rf^8))
  #Fuel is drying, calculate Moisture Content
  MC0 <- MCold
  MC0[MCold > EMC.D] <- EMC.D[MCold > EMC.D] + (MCold[MCold > EMC.D] - EMC.D[MCold > EMC.D]) * exp(-1.0 * log(10.0) * K.GRASS[MCold > EMC.D] * t0)

  #Fuel is wetting, calculate moisture content
  MC0[MCold < EMC.W] <- EMC.W[MCold < EMC.W] + (MCold[MCold < EMC.W] - EMC.W[MCold < EMC.W]) * exp(-1.0 * log(10.0) * K.GRASS[MCold < EMC.W] * t0)

  #Eq. 12 - Calculate GFMC
  GFMC0 <- 59.5 * ((250 - MC0) / (147.2772 + MC0))
  #Keep current and old GFMC
  GFMC <- GFMC0
  names(GFMC) <- "GFMC"
  #Same for moisture content
  MC <- MC0
  names(MC) <- "MC"
  #Reset vars
  GFMCold <- GFMC0
  MCold <- MC0

  #Return requested 'out' type
  if (out=="ALL"){
    return(stack(input, GFMC, MC))
  } else if(out == "GFMC"){
    return(GFMC)
  } else if (out == "MC"){
    return(MC)
  } else { #GFMCandMC
    return(stack( GFMC, MC))
  }
}

