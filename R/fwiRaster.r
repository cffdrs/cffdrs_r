#' Raster-based Fire Weather Index System
#' 
#' \code{fwiRaster} is used to calculate the outputs of the Canadian Forest
#' Fire Weather Index (FWI) System for one day based on noon local standard
#' time (LST) weather observations of temperature, relative humidity, wind
#' speed, and 24-hour rainfall, as well as the previous day's fuel moisture
#' conditions. This function takes rasterized input and generates raster maps
#' as outputs.
#' 
#' The Canadian Forest Fire Weather Index (FWI) System is a major subsystem of
#' the Canadian Forest Fire Danger Rating System, which also includes Canadian
#' Forest Fire Behavior Prediction (FBP) System. The modern FWI System was
#' first issued in 1970 and is the result of work by numerous researchers from
#' across Canada. It evolved from field research which began in the 1930's and
#' regional fire hazard and fire danger tables developed from that early
#' research.
#' 
#' The modern System (Van Wagner 1987) provides six output indices which
#' represent fuel moisture and potential fire behavior in a standard pine
#' forest fuel type. Inputs are a daily noon observation of fire weather, which
#' consists of screen-level air temperature and relative humidity, 10 meter
#' open wind speed and 24 accumulated precipitation.
#' 
#' The first three outputs of the system (the Fire Fuel Moisture Code, the Duff
#' Moisture Code, and the Drought Code) track moisture in different layers of
#' the fuel making up the forest floor. Their calculation relies on the daily
#' fire weather observation and also, importantly, the code value from the
#' previous day as they are in essence bookkeeping systems tracking the amount
#' of moisture (water) in to and out of the layer.  It is therefore important
#' that when calculating FWI System outputs over an entire fire season, an
#' uninterrupted daily weather stream is provided; one day is the assumed time
#' step in the models and thus missing data must be filled in.
#' 
#' The next three outputs of the System are relative (unitless) indicators of
#' aspects of fire behavior potential: spread rate (the Initial Spread Index),
#' fuel consumption (the Build-up Index) and fire intensity per unit length of
#' fire front (the Fire Weather Index).  This final index, the fwi, is the
#' component of the System used to establish the daily fire danger level for a
#' region and communicated to the public.  This final index can be transformed
#' to the Daily Severity Rating (dsr) to provide a more reasonably-scaled
#' estimate of fire control difficulty.
#' 
#' Both the Duff Moisture Code (dmc) and Drought Code (dc) are influenced by
#' day length (see Van Wagner, 1987). Day length adjustments for different
#' ranges in latitude can be used (as described in Lawson and Armitage 2008
#' (\url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf})) and are included
#' in this R function; latitude must be positive in the northern hemisphere and
#' negative in the southern hemisphere.
#' 
#' The default initial (i.e., "start-up") fuel moisture code values (FFMC=85,
#' DMC=6, DC=15) provide a reasonable set of conditions for most springtime
#' conditions in Canada, the Northern U.S., and Alaska. They are not suitable
#' for particularly dry winters and are presumably not appropriate for
#' different parts of the world.
#' 
#' @param input A stack or brick containing rasterized daily weather
#' observations taken at noon LST. Variable names have to be the same as in the
#' following list, but they are case insensitive. The order in which the inputs
#' are entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{lat} \tab (recommended) \tab Latitude (decimal degree,
#' __default=55__)\cr 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab
#' (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required)
#' \tab 24-hour rainfall (mm)\cr }
#' @param init A vector that contains the initial values for FFMC, DMC, and DC
#' or a stack that contains raster maps of the three moisture codes calculated
#' for the previous day, which will be used for the current day's calculation.
#' Defaults are the standard initial values for FFMC, DMC, and DC defined as
#' the following: 
#' 
#' \tabular{lll}{ 
#' \bold{Variable} \tab \bold{Description} \tab \bold{Default} \cr
#' \var{ffmc} \tab Previous day Fine Fuel Moisture Code (FFMC; unitless) \tab 85 \cr
#' \var{dmc} \tab Previous day Duff Moisture Code (DMC; unitless)\tab 6 \cr
#' \var{dc} \tab Previous Day Drought Code (DC; unitless) \tab 15\cr
#' \var{lat} \tab Latitude of the weather station (\emph{Optional})\tab 55 \cr}
#' 
#' @param mon Month of the year (integer 1~12, default=7). Month is used in
#' latitude adjustment (\code{lat.adjust}), it is therefore recommended when
#' \code{lat.adjust=TRUE} was chosen.
#' @param out The function offers two output options, \code{out="all"} will
#' produce a raster stack include both the input and the FWI System outputs;
#' \code{out="fwi"} will generate a stack with only the FWI system components.
#' @param lat.adjust The function offers options for whether latitude
#' adjustments to day lengths should be applied to the calculations. The
#' default value is "TRUE".
#' @param uppercase Output in upper cases or lower cases would be decided by
#' this argument. Default is TRUE.
#' @return By default, \code{fwi} returns a raster stack which includes both
#' the input and the FWI System variables, as describe below: \item{Inputs
#' }{Including \code{temp}, \code{rh}, \code{ws}, and \code{prec} with
#' \code{lat} as optional.} \item{ffmc }{Fine Fuel Moisture Code} \item{dmc
#' }{Duff Moisture Code} \item{dc }{Drought Code} \item{isi }{Initial Spread
#' Index} \item{bui }{Buildup Index} \item{fwi }{Fire Weather Index} \item{dsr
#' }{Daily Severity Rating}
#' 
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' 
#' @seealso \code{\link{fbp}}, \code{\link{fbpRaster}}, \code{\link{fwi}},
#' \code{\link{hffmc}}, \code{\link{hffmcRaster}}
#' 
#' @references 1. Van Wagner, C.E. and T.L. Pickett. 1985. Equations and
#' FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For.
#' Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19973.pdf}
#' 
#' 2. Van Wagner, C.E. 1987. Development and structure of the Canadian forest
#' fire weather index system. Forest Technology Report 35. (Canadian Forestry
#' Service: Ottawa). \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/19927.pdf}
#' 
#' 3.  Lawson, B.D. and O.B. Armitage. 2008. Weather guide for the Canadian
#' Forest Fire Danger Rating System. Nat. Resour. Can., Can. For. Serv., North.
#' For. Cent., Edmonton, AB.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf}
#' 
#' @keywords methods
#' 
#' @examples
#' 
#' library(cffdrs)
#' require(raster)
#' # The test data is a stack with four input variables including 
#' # daily noon temp, rh, ws, and prec (we recommend tif format):
#' day01src <- system.file("extdata","test_rast_day01.tif",package="cffdrs")
#' day01 <- stack(day01src)
#' day01 <- crop(day01,c(250,255,47,51))
#' # assign variable names:
#' names(day01)<-c("temp","rh","ws","prec")
#' # (1) use the initial values
#' foo<-fwiRaster(day01)
#' plot(foo)
#' ### Additional, longer running examples ###
#' # (2) use initial values with larger raster
#' day01 <- stack(day01src)
#' names(day01)<-c("temp","rh","ws","prec")
#' \donttest{foo<-fwiRaster(day01)}
#' plot(foo)
#' 
#' @export fwiRaster
fwiRaster <- function(input, init = c(ffmc = 85, dmc = 6, dc = 15), mon = 7,
                      out = "all", lat.adjust = TRUE, uppercase = TRUE) {

  #Reference latitude for DMC day length adjustment
  #46N: Canadian standard, latitude >= 30N   (Van Wagner 1987)
  ell01 <- c(6.5, 7.5, 9, 12.8, 13.9, 13.9, 12.4, 10.9, 9.4, 8, 7, 6)
  #20N: For 30 > latitude >= 10
  ell02 <- c(7.9, 8.4, 8.9, 9.5, 9.9, 10.2, 10.1, 9.7, 9.1,8.6, 8.1, 7.8)
  #20S: For -10 > latitude >= -30  
  ell03 <- c(10.1, 9.6, 9.1, 8.5, 8.1, 7.8, 7.9, 8.3, 8.9, 9.4, 9.9, 10.2)
  #40S: For -30 > latitude
  ell04 <- c(11.5, 10.5, 9.2, 7.9, 6.8, 6.2, 6.5, 7.4, 8.7, 10, 11.2, 11.8)
  #For latitude near the equator, we simple use a factor of 9 for all months
  
  #Day length factor for DC Calculations
  #20N: North of 20 degrees N
  fl01 <- c(-1.6, -1.6, -1.6, 0.9, 3.8, 5.8, 6.4, 5, 2.4, 0.4, -1.6, -1.6)
  #20S: South of 20 degrees S
  fl02 <- c(6.4, 5, 2.4, 0.4, -1.6, -1.6, -1.6, -1.6, -1.6, 0.9, 3.8, 5.8)
  #Near the equator, we just use 1.4 for all months.
  
  #Quite often users will have a data frame called "input" already attached
  #  to the workspace. To mitigate this, we remove that if it exists, and warn
  #  the user of this case.
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  names(input) <- tolower(names(input))
  temp <- input$temp
  prec <- input$prec
  ws <- input$ws
  rh <- input$rh
  if ("lat" %in% names(input)) {
    lat <- input$lat
  }else {
    lat <- temp
    values(lat) <- 55
  }
  
  if (!exists("temp") | is.null(temp)) 
    stop("temperature (temp) is missing!")
  if (!exists("prec") | is.null(prec)) 
    stop("precipitation (prec) is missing!")
  if (!is.null(prec[prec < 0]))
    stop("precipiation (prec) cannot be negative!")
  if (!exists("ws") | is.null(ws)) 
    stop("wind speed (ws) is missing!")
  if (!is.null(ws[ws < 0]))
    stop("wind speed (ws) cannot be negative!")
  if (!exists("rh") | is.null(rh)) 
    stop("relative humidity (rh) is missing!")
  if (!is.null(rh[rh < 0]))
    stop("relative humidity (rh) cannot be negative!")

  names(init) <- tolower(names(init))

  #Assign values for initializing variables
  if (is.numeric(init)){
    if (is.null(names(init))){
      names(init)<-c('ffmc', 'dmc', 'dc')
    }
    ffmc_yda <- dmc_yda <- dc_yda <- temp
    values(ffmc_yda) <- init[['ffmc']]
    values(dmc_yda) <- init[['dmc']]
    values(dc_yda) <- init[['dc']]
  } else {
    ffmc_yda <- init$ffmc
    dmc_yda  <- init$dmc
    dc_yda   <- init$dc
  }
  #constrain relative humidity
  rh[rh>=100]<- 99.9999
  ###########################################################################
  #                    Fine Fuel Moisture Code (FFMC)
  ###########################################################################
  #Eq. 1
  wmo <- 147.2 * (101 - ffmc_yda)/(59.5 + ffmc_yda)
  #Eq. 2 Rain reduction to allow for loss in overhead canopy
  ra1 <- prec
  ra1[ra1 <= 0.5] <- NA
  ra1 <- ra1-0.5
  ra2 <- prec
  ra2[ra2 > 0.5] <- NA
  ra <- cover(ra1, ra2)
  #masking values
  wmo1 <- mask(wmo, ra1)
  wmo2 <- mask(wmo,ra2)
  wmo11 <- wmo1
  wmo11[wmo11 <= 150] <- NA
  ra11 <- ra1
  ra11[wmo1 <= 150] <- NA
  #Eqs. 3a & 3b
  wmo11 <- wmo11 + 0.0015 * (wmo11 - 150) * (wmo11 - 150) * sqrt(ra11) + 42.5 * 
           ra11 * exp(-100 / (251 - wmo11)) * (1 - exp(-6.93 / ra11))
  wmo12 <- wmo1
  wmo12[wmo12 > 150] <- NA
  ra12 <- ra1
  ra12[wmo1 > 150] <- NA
  wmo12 <- wmo12 + 42.5 * ra12 * exp(-100 / (251 - wmo12)) * 
          (1 - exp(-6.93 / ra12))
  wmo1 <- cover(wmo11, wmo12)
  wmo <- cover(wmo1, wmo2)
  #The real moisture content of pine litter ranges up to about 250 percent,
  # so we cap it at 250
  wmo[wmo > 250] <- 250
  #cleanup intermediate values
  rm(ra1, ra11, ra12, ra2, wmo1, wmo2, wmo11, wmo12)
  #Eq. 4 Equilibrium moisture content from drying
  ed <- 0.942 * (rh^0.679) + (11 * exp((rh - 100)/10)) + 0.18 * 
    (21.1 - temp) * (1 - 1/exp(rh * 0.115))
  #Eq. 5 Equilibrium moisture content from wetting
  ew <- 0.618 * (rh^0.753) + (10 * exp((rh - 100)/10)) + 0.18 * 
    (21.1 - temp) * (1 - 1/exp(rh * 0.115))
  #Create a new raster object based on wmo, ed, and ew
  z0 <- overlay(wmo, ed, ew, fun = function(a, b, c){ return(a < b & a < c) })
  #Create new rasters and mask out missing values
  z0[z0 == 0] <- NA
  rh0 <- mask(rh, z0)
  ws0 <- mask(ws, z0)
  #Eq. 6a (ko) Log drying rate at the normal termperature of 21.1 C
  z <- 0.424 * (1 - (((100 - rh0)/100)^1.7)) + 0.0694 * sqrt(ws0) * (1 - ((100 - rh0)/100)^8)
  # Assigning to 0 instead of NA, as 0 makes more sense
  z[is.na(z)] <- 0
  # Mask missing temp values
  z <- mask(z, temp)
  rm(rh0, ws0, z0)
  #Eq. 6b Affect of temperature on  drying rate
  x <- z * 0.581 * exp(0.0365 * temp)
  #Create a new raster object based on wmo, ed, and ew
  z0 <- overlay(wmo, ed, ew, fun = function(a, b, c){ return(a < b & a < c) })
  #Create new rasters and mask out missing values
  z0[z0 == 0] <- NA
  ew0 <- mask(ew, z0)
  x0 <- mask(x, z0)
  wmo0 <- mask(wmo, z0)
  #Eq. 8
  wmo1 <- ew0 - (ew0 - wmo0) / (10^x0)
  wmo2 <- wmo
  wmo2[!is.na(wmo0)] <- NA
  wm <- cover(wmo1, wmo2)
  rm(z0, ew0, x0, wmo0, wmo1, wmo2)
  #Create a new raster object based on wmo, and ed
  z0 <- overlay(wmo, ed, fun = function(a, b){ return(a > b) }) 
  #Create new rasters and mask out missing values
  z0[z0 == 0] <- NA
  rh0 <- mask(rh, z0)
  ws0 <- mask(ws, z0)
  #Eq. 7a (ko) Log wetting rate at the normal termperature of 21.1 C   
  z0 <- 0.424 * (1 - (rh0 / 100)^1.7) + 0.0694 * sqrt(ws0) * (1 - (rh0 / 100)^8)
  z1 <- z
  z1[!is.na(z0)] <- NA
  z <- cover(z0, z1)
  rm(rh0, ws0)
  #Eq. 7b Affect of temperature on  wetting rate
  x <- z * 0.581 * exp(0.0365 * temp)
  ed0 <- mask(ed,z0)
  wmo0 <- mask(wmo,z0)
  x0 <- mask(x,z0)
  #Eq. 9
  wm0 <- ed0 + (wmo0 - ed0)/(10^x0)
  wm1 <- mask(wm, z1)
  wm <- cover(wm0, wm1)
  rm(ed0, x0, wm0, wm1, wmo0)
  #Eq. 10 Final FFMC calculation
  ffmc <- (59.5 * (250 - wm))/(147.2 + wm)
  #Constraints
  ffmc[ffmc>101] <- 101 
  ffmc[ffmc<0] <- 0

  
  ###########################################################################
  #                        Duff Moisture Code (DMC)
  ###########################################################################
  t0 <- temp
  #constrain low end of temperature
  t0[t0 < -1.1] <- -1.1
  #Eq. 16 - The log drying rate
  rk <- 1.894 * (t0 + 1.1) * (100 - rh) * ell01[mon] * 1e-04
  #Adjust the day length  and thus the drying r, based on latitude and month
  if (lat.adjust) {
    rk[lat <= 30 & lat > 10] <- 1.894 * (t0[lat <= 30 & lat > 10] + 1.1) * 
      (100 - rh[lat <= 30 & lat > 10]) * ell02[mon] * 1e-04
    rk[lat <= -10 & lat > -30] <- 1.894 * (t0[lat <= -10 & lat > -30] + 1.1) * 
      (100 - rh[lat <= -10 & lat > -30]) * ell03[mon] * 1e-04
    rk[lat <= -30 & lat >= -90] <- 1.894 * (t0[lat <= -30 & lat >= -90] + 1.1) *
      (100 - rh[lat <= -30 & lat >= -90]) * ell04[mon] * 1e-04
    rk[lat <= 10 & lat > -10] <- 1.894 * (t0[lat <= 10 & lat > -10] + 1.1) * 
      (100 - rh[lat <= 10 & lat > -10]) * 9 * 1e-04
  }
  ra <- prec
  #Eq. 11 - Net rain amount
  rw <- 0.92 * ra - 1.27
  #Alteration to Eq. 12 to calculate more accurately
  wmi <- 20 + 280 / exp(0.023 * dmc_yda)
  #Eqs. 13a, 13b, 13c
  b <- dmc_yda
  b[dmc_yda <= 33] <- 100 / (0.5 + 0.3 * dmc_yda[dmc_yda <= 33])
  if (!is.null(dmc_yda[dmc_yda > 33 & dmc_yda <= 65])){
    b[dmc_yda > 33 & dmc_yda <= 65] <- 
      14 - 1.3 * log(dmc_yda[dmc_yda > 33 & dmc_yda <= 65])
  }
  if(!is.null(dmc_yda[dmc_yda > 65])){
    b[dmc_yda > 65] <- 
      6.2 * log(dmc_yda[dmc_yda > 65]) - 17.2
  }
  #Eq. 14 - Moisture content after rain
  wmr <- wmi + 1000 * rw / (48.77 + b * rw)
  #Alteration to Eq. 15 to calculate more accurately
  pr0 <- 43.43 * (5.6348 - log(wmr - 20))

  pr<-pr0
  #Constrain P
  pr[prec <= 1.5] <-dmc_yda[prec <= 1.5]
  pr[pr < 0] <- 0
  #Calculate final P (DMC)
  dmc <- pr + rk
  dmc[dmc < 0] <- 0 
  ###########################################################################
  #                             Drought Code (DC)
  ###########################################################################
  #Constrain temperature
  t0[temp< (-2.8)] <- -2.8
  #Eq. 22 - Potential Evapotranspiration
  pe <- (0.36 * (t0 + 2.8) + fl01[mon])/2
  #Daylength factor adjustment by latitude for Potential Evapotranspiration
  if (lat.adjust) {
    pe[lat <= -10] <- (0.36 * (t0[lat <= -10] + 2.8) + fl02[mon]) / 2
    pe[lat > -10 & lat <= 10] <- (0.36 * (t0[lat > -10 & lat <= 10] + 2.8) + 1.4) / 2
  }
  ra <- prec
  #Eq. 18 - Effective Rainfall
  rw <- 0.83 * ra - 1.27
  #Eq. 19
  smi <- 800 * exp(-1 * dc_yda / 400)
  #Alteration to Eq. 21
  dr0 <- dc_yda - 400 * log(1 + 3.937 * rw / smi)
  dr0[dr0 < 0] <- 0 
  dr <- dr0
  #if precip is less than 2.8 then use yesterday's DC
  dr[prec <= 2.8] <- dc_yda[prec <= 2.8]
  #Alteration to Eq. 23
  dc <- dr + pe
  dc[dc < 0] <- 0
  
  ###########################################################################
  #                    Initial Spread Index (ISI)
  ###########################################################################
  #Eq. 24 - Wind Effect
  fW <- exp(0.05039 * ws)
  #Eq. 10 - Moisture content
  fm <- 147.2 * (101 - ffmc) / (59.5 + ffmc)
  #Eq. 25 - Fine Fuel Moisture
  fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31) / 49300000)
  #Eq. 26 - Spread Index Equation
  isi <- 0.208 * fW * fF
  
  ###########################################################################
  #                       Buildup Index (BUI)
  ###########################################################################
  #Eq. 27a
  bui <- 0.8 * dc * dmc/(dmc + 0.4 * dc)
  bui[dmc == 0 & dc == 0] <- 0
  #Eq. 27b - next 4 lines
  p <- (dmc - bui)/dmc
  p[dmc == 0] <- 0
  cc <- 0.92 + ((0.0114 * dmc)^1.7)
  bui0 <- dmc - cc * p
  #Constraints
  bui0[bui0 < 0] <- 0
  bui[bui < dmc] <- bui0[bui < dmc]
  
  ###########################################################################
  #                     Fire Weather Index (FWI)
  ###########################################################################
  #Eqs. 28b, 28a, 29
  bb <-0.1 * isi * (0.626 * (bui^0.809) + 2)
  bb[bui > 80] <- 0.1 * isi[bui > 80] * (1000 / (25 + 108.64 / exp(0.023 * bui[bui > 80])))
  #Eqs. 30b, 30a
  fwi <-exp(2.72 * ((0.434 * log(bb))^0.647))
  #Constraint
  fwi[bb <= 1] <- bb[bb <= 1]
  ###########################################################################
  #                   Daily Severity Rating (DSR)
  ###########################################################################
  #Eq. 31
  dsr <- 0.0272 * (fwi^1.77)

  #If output specified is "fwi", then return only the FWI variables
  if (out == "fwi") {
    #Creating a raster stack of FWI variables to return
    new_FWI <- stack(ffmc, dmc, dc, isi, bui, fwi, dsr)
    names(new_FWI) <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr")
    if (uppercase){
      names(new_FWI) <- toupper(names(new_FWI))
    }
    #If output specified is "all", then return both FWI and input weather vars
  } else {
    if (out == "all") {
      #Create a raster stack of input and FWI variables
      new_FWI <- stack(input, ffmc, dmc, dc, isi, bui, fwi, dsr)
      names(new_FWI) <- c(names(input),"ffmc", "dmc", "dc", "isi", "bui", "fwi", "dsr")
      if (uppercase){
        names(new_FWI) <- toupper(names(new_FWI))
      }
    }
  }
  return(new_FWI)
}

