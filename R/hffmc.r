#' Hourly Fine Fuel Moisture Code
#' 
#' \code{hffmc} is used to calculate hourly Fine Fuel Moisture Code (FFMC) and
#' is based on a calculation routine first described in detail by Van Wagner
#' (1977) and which has been updated in minor ways by the Canadian Forest
#' Service to have it agree with the calculation methodology for the daily FFMC
#' (see \code{\link{fwi}}).  In its simplest typical use this current routine
#' calculates a value of FFMC based on a series of uninterrupted hourly weather
#' observations of screen level (~1.4 m) temperature, relative humidity, 10 m
#' wind speed, and 1-hour rainfall. This implementation of the function
#' includes an optional time.step input which is defaulted to one hour, but can
#' be reduced if sub-hourly calculation of the code is needed.  The FFMC is in
#' essence a bookkeeping system for moisture content and thus it needs to use
#' the last time.step's value of FFMC in its calculation as well.  This
#' function could be used for either one weather station or for multiple
#' weather stations.
#' 
#' The hourly FFMC is very similar in its structure and calculation to the
#' Canadian Forest Fire Weather Index System's daily FFMC (\code{\link{fwi}})
#' but has an altered drying and wetting rate which more realistically reflects
#' the drying and wetting of a pine needle litter layer sitting on a decaying
#' organic layer.  This particular implementation of the Canadian Forest Fire
#' Danger Rating System's hourly FFMC provides for a flexible time step; that
#' is, the data need not necessarily be in time increments of one hour.  This
#' flexibility has been added for some users who use this method with data
#' sampled more frequently that one hour.  We do not recommend using a time
#' step much greater than one hour. An important and implicit assumption in
#' this calculation is that the input weather is constant over the time step of
#' each calculation (e.g., typically over the previous hour).  This is a
#' reasonable assumption for an hour; however it can become problematic for
#' longer periods.  For brevity we have referred to this routine throughout
#' this description as the hourly FFMC.
#' 
#' Because of the shortened time step, which can lead to more frequent
#' calculations and conversion between moisture content and the code value
#' itself, we have increased the precision of one of the constants in the
#' simple formula that converts litter moisture content to the 'Code' value.
#' This is necessary to avoid a potential bias that gets introduced during
#' extremely dry conditions.  This is simply a change in the precision at which
#' this constant is used in the equation and is not a change to the standard
#' FFMC conversion between moisture and code value (which is referred to as the
#' FF-scale).
#' 
#' The calculation requires the previous hour's FFMC as an input to the
#' calculation of the current hour's FFMC; this is because the routine can be
#' thought of as a bookkeeping system and needs to know the amount of moisture
#' being held in the fuel prior to any drying or wetting in the current period.
#' After each hour's calculation that newly calculated FFMC simply becomes the
#' starting FFMC in the next hour's calculation.  At the beginning of the
#' calculations at a station this previous hours FFMC must be estimated. It is
#' typical to use a value of 85 when this value cannot be estimated more
#' accurately; this code value corresponds to a moisture content of about 16\%
#' in typical pine litter fuels.
#' 
#' @param weatherstream A dataframe containing input variables of hourly
#' weather observations. It is important that variable names have to be the
#' same as in the following list, but they are case insensitive. The order in
#' which the input variables are entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr 
#' \var{hr} \tab (optional) \tab Hourly value to calculate sub-hourly ffmc \cr 
#' \var{bui} \tab (optional) \tab Daily BUI value for the computation of hourly 
#' FWI. It is required when \code{hourlyFWI=TRUE}.\cr } 
#' Typically this dataframe also contains date and
#' hour fields so outputs can be associated with a specific day and time,
#' however these fields are not used in the calculations.  If multiple weather
#' stations are being used, a weather station ID field is typically included as
#' well, though this is simply for bookkeeping purposes and does not affect the
#' calculation.
#' @param ffmc_old Initial FFMC. At the start of calculations at a particular
#' station there is a need to provide an estimate of the FFMC in the previous
#' timestep; this is because the FFMC is, in essence, a bookkeeping system for
#' moisture.  If no estimate of previous hour's FFMC is available the function
#' will use default value, \code{ffmc_old=85}. When using the routine to
#' calculate hourly FFMC at multiple stations the \code{ffmc_old} argument can
#' also accept a vector with the same number of weather stations.
#' @param time.step Is the time (in hours) between the previous value of FFMC
#' and the current time at which we want to calculate a new value of the FFMC.
#' When not specified it will take on a default value of \code{time.step=1}.
#' @param calc.step Optional for whether time step between two observations is
#' calculated. Default is FALSE, no calculations. This is used when time
#' intervals are not uniform in the input.
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate hourly or
#' sub-hourly FFMC for one weather station over a period of time iteratively.
#' If multiple weather stations are processed, an additional "id" column is
#' required in the input weatherstream to label different stations, and the
#' data needs to be sorted by date/time and "id".  If \code{batch=FALSE}, the
#' function calculates only one time step base on either the previous hourly
#' FFMC or the initial start value.
#' @param hourlyFWI Optional for the computation of hourly ISI, FWI, and DSR.
#' Default is FALSE. While \code{hourlyFWI=TRUE}, daily BUI is required for the
#' computation of FWI.
#' @return \code{hffmc} returns a vector of hourly or sub-hourly FFMC values,
#' which may contain 1 or multiple elements. Optionally when
#' \code{hourlyFWI=TRUE}, the function also output a data.frame contains input
#' weatherstream as well as the hourly or sub-hourly FFMC, ISI, FWI, and DSR.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, Brett Moore, and Mike
#' Flannigan
#' @seealso \code{\link{fbp}}, \code{\link{fwi}}, \code{\link{hffmcRaster}}
#' @references Van Wagner, C.E. 1977. A method of computing fine fuel moisture
#' content throughout the diurnal cycle. Environment Canada, Canadian Forestry
#' Service, Petawawa Forest Experiment Station, Chalk River, Ontario.
#' Information Report PS-X-69.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/25591.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' data("test_hffmc")
#' # show the data format:
#' head(test_hffmc)
#' # (1)hffmc default: 
#' # Re-order the data by year, month, day, and hour:
#' test_hffmc<-test_hffmc[with(test_hffmc, order(yr,mon,day,hr)),]
#' # Because the test data has 24 hours input variables 
#' # it is possible to calculate the hourly FFMC chronically 
#' # through multiple days(with the default initial ffmc_old=85):
#' test_hffmc$ffmc_default<-hffmc(test_hffmc) 
#' # (2) Calculate FFMC for multiple stations:
#' # Calculate hourly FFMC with only one initial 
#' # value (ffmc_old=85), but multiple weather stations. 
#' # Sort the input by date/time and the station id:
#' test_hffmc<-test_hffmc[with(test_hffmc,order(yr,mon,hr)),]
#' # Add weather station id:
#' test_hffmc$id<-rep(1:10,nrow(test_hffmc)/10)
#' #check the data:
#' head(test_hffmc)
#' test_hffmc$ffmc01<-hffmc(test_hffmc,batch=TRUE)
#' # With multiple initial FFMC (ffmc_old) as a vector: 
#' test_hffmc$ffmc02<- hffmc(test_hffmc,ffmc_old = sample(70:100,10, replace=TRUE),batch=TRUE)
#' # One time step assuming all records are from different 
#' # weather stations: 
#'      foo<-hffmc(test_hffmc,batch=FALSE)
#' # (3) output all hourly FWI System variables:
#' test_hffmc$id<-NULL
#' test_hffmc<-test_hffmc[with(test_hffmc,    order(yr,mon,day,hr)),]
#' foo<-hffmc(test_hffmc,hourlyFWI=TRUE)
#' # this will not run: warning message requesting for daily BUI
#' test_hffmc$bui<-100
#' foo<-hffmc(test_hffmc,hourlyFWI=TRUE)
#' # (4) Calculate time steps in case the time intervals are 
#' # not uniform:
#' dat0<-test_hffmc[sample(1:30,20),]
#' dat0<-dat0[with(dat0,order(yr,mon,day,hr)),]
#' # with or without calc.step, hffmc is going to generate
#' # different FFMC values.
#' # without calculating time step (default):
#' hffmc(dat0,time.step=1)
#' # with calc.step=TRUE, time.step=1 is applied to 
#' # only the first record, the rests would be calculated:
#' hffmc(dat0,time.step=1,calc.step=TRUE)
#' 
#' @export hffmc
hffmc <- function(weatherstream, ffmc_old = 85, time.step = 1, 
                  calc.step = FALSE, batch = TRUE, hourlyFWI = FALSE) {

  t0 <- time.step
  names(weatherstream) <- tolower(names(weatherstream))
  #set up number of stations
  if (batch){
    if ("id" %in% names(weatherstream)) { 
      n <- length(unique(weatherstream$id))
      if(length(unique(weatherstream[1:n,"id"])) != n){
        stop("Multiple stations have to start and end at the same dates/time, 
             and the data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  }else{
    n <- nrow(weatherstream)
  }
  
  if (length(ffmc_old) == 1 & n > 1){
    Fo <- rep(ffmc_old, n)
  } else {
    Fo <- ffmc_old
  }
  
  #set some local scope variables
  Tp <- weatherstream$temp
  H  <- weatherstream$rh
  W  <- weatherstream$ws
  ro <- weatherstream$prec

  #Check that the parameters are correct
  if (calc.step){
    hr <- weatherstream$hr
    if (!exists("hr") | is.null(hr)) 
      warning("hour value is missing!")
  }
  if (!exists("Tp") | is.null(Tp)) 
    warning("temperature (temp) is missing!")
  if (!exists("ro") | is.null(ro)) 
    warning("precipitation (prec) is missing!")
  if (!exists("W") | is.null(W)) 
    warning("wind speed (ws) is missing!")
  if (!exists("H") | is.null(H)) 
    warning("relative humidity (rh) is missing!")
  if (length(H)%%n != 0)
    warning("Weatherstream do not match with number of weather stations")
  #Length of weather run
  n0 <- length(H) / n
  f <- NULL
  #For each day in the run
  for (i in 1:n0){
    #k is the data for all stations by day
    k <- ((i - 1) * n + 1):(i * n)
    if (calc.step & i > 1) {
      t0 <- ifelse(n0 > 1, hr[k] - hr[k-n], t0)
      t0 <- ifelse(t0 == -23, 1, t0)
      t0 <- ifelse(t0 < 0, -1 * t0, t0)
    }
    #Eq. 1 (with a more precise multiplier than the daily)
    mo <- 147.27723 * (101 - Fo)/(59.5 + Fo)
    rf <- ro[k]
    #Eqs. 3a & 3b (Van Wagner & Pickett 1985)
    mr <- ifelse(mo <= 150, 
            mo + 42.5 * rf * exp(-100 / (251 - mo)) * (1 - exp(-6.93 / rf)),
            mo + 42.5 * rf * exp(-100 / (251 - mo)) * (1 - exp(-6.93 / rf)) + 
              0.0015 * ((mo - 150)^2) * (rf^0.5))
    #The real moisture content of pine litter ranges up to about 250 percent,
    # so we cap it at 250
    mr <- ifelse(mr > 250, 250, mr)
    mo <- ifelse(ro[k] > 0.0, mr, mo)
    #Eq. 2a Equilibrium moisture content from drying
    Ed <- 0.942 * (H[k]^0.679) + 11 * exp((H[k] - 100) / 10) + 0.18 * 
          (21.1 - Tp[k]) * (1 - exp(-0.115 * H[k]))
    #Eq. 3a Log drying rate at the normal temperature of 21.1C
    ko <- 0.424 * (1 - (H[k] / 100)^1.7) + 0.0694 * (W[k]^0.5) * 
          (1 - (H[k] / 100)^8)
    #Eq. 3b
    kd <- ko * 0.0579 * exp(0.0365 * Tp[k])
    #Eq. 8 (Van Wagner & Pickett 1985)
    md <- Ed + (mo - Ed) * (10^(-kd * t0))
    #Eq. 2b Equilibrium moisture content from wetting
    Ew <- 0.618 * (H[k]^0.753) + 10 * exp((H[k] - 100) / 10) + 0.18 * 
          (21.1 - Tp[k]) * (1 - exp(-0.115 * H[k]))
    #Eq. 7a Log wetting rate at the normal temperature of 21.1 C    
    k1 <- 0.424 * (1 - ((100 - H[k]) / 100)^1.7) + 0.0694 * 
      (W[k]^0.5) * (1 - ((100 - H[k]) / 100)^8)
    #Eq. 4b 
    kw <- k1 * 0.0579 * exp(0.0365 * Tp[k])
    #Eq. 8 (Van Wagner & Pickett 1985)
    mw <- Ew - (Ew - mo) * (10^(-kw * t0))
    #Constraints
    m <- ifelse(mo > Ed, md, mw)
    m <- ifelse(Ed >= mo & mo >= Ew, mo, m)
    #Eq. 6 - Final hffmc calculation (modified 3rd constant to 147.27723)
    Fo <- 59.5 * (250 - m) / (147.27723 + m)
    Fo <- ifelse(Fo <=0, 0, Fo)
    f <- c(f, Fo)
  }
  #Calculate hourly isi and fwi
  if (hourlyFWI){
    bui <- weatherstream$bui
    if (!exists("bui") | is.null(bui)){ 
      warning("Daily BUI is required to calculate hourly FWI")
    } else {
      #Calculate ISI
      isi <- .ISIcalc(f, W, FALSE)
      #Calculate FWI
      fwi <- .fwiCalc(isi, bui)
      #Calculate DSR
      dsr <- 0.0272 * (fwi^1.77)
      #Put all data into a data.frame to return
      output <- cbind(weatherstream, 
                      data.frame(ffmc = f, isi = isi, fwi = fwi, dsr = dsr))
      return(output)
    }
    #otherwise just return hffmc
  } else {
    return(f)
  }
}
