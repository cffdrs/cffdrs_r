#' Raster-based Hourly Fine Fuel Moisture Code
#' 
#' \code{hffmcRaster} is used to calculate hourly Fine Fuel Moisture Code
#' (FFMC) based on hourly weather observations of screen level (~1.4 m)
#' temperature, relative humidity, 10 m wind speed, and 1-hour rainfall. This
#' implementation of the function includes an optional timestep input which is
#' defaulted to one hour, but can be reduced if sub-hourly calculation of the
#' code is needed.  The FFMC is in essence a bookkeeping system for moisture
#' content and thus it needs to use the last timestep's value of FFMC in its
#' calculation. \code{hffmcRaster} takes rasterized inputs and generates raster
#' maps as outputs.
#' 
#' The hourly FFMC is very similar in its structure and calculation to the
#' Canadian Forest Fire Weather Index System's daily FFMC (\code{\link{fwi}})
#' but has an altered drying and wetting rate which more realistically reflects
#' the drying and wetting of a pine needle litter layer sitting on a decaying
#' organic layer.  This particular implementation of the Canadian Forest Fire
#' Danger Rating System's hourly FFMC provides for a flexible timestep; that
#' is, the data need not necessarily be in time increments of one hour.  This
#' flexibility has been added for some users who use this method with data
#' sampled more frequently that one hour.  We do not recommend using a timestep
#' much greater than one hour. An important and implicit assumption in this
#' calculation is that the input weather is constant over the timestep of each
#' calculation (e.g., typically over the previous hour).  This is a reasonable
#' assumption for an hour; however it can become problematic for longer
#' periods.  For brevity we have referred to this routine throughout this
#' description as the hourly FFMC.
#' 
#' Because of the shortened timestep, which can lead to more frequent
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
#' @param weatherstream A raster stack or brick containing hourly weather
#' observations. Variable names have to be the same as in the following list,
#' but they are case insensitive. The order in which the input variables are
#' entered is not required.
#' 
#' \tabular{lll}{ 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr 
#' \var{bui} \tab (optional) 
#' \tab Daily BUI value for the computation of hourly FWI.\cr
#' \tab\tab It is required when \code{hourlyFWI=TRUE}.\cr }
#' @param ffmc_old A single value of FFMC or a raster of FFMC for the previous
#' hour which will be used for the current hour's calculation. In some
#' situations, there are no previous-hourly FFMC values to calculate the
#' current hourly FFMC, the function will use a default value,
#' \code{ffmc_old=84}.
#' @param time.step timestep in hours. Default is 1 hour, set for standard
#' hourly FFMC calculation. While \code{time.step} is set to values with
#' decimal places, sub-hourly FFMC would be calculated.
#' @param hourlyFWI Optional for the computation of hourly ISI, FWI, and DSR.
#' Default is FALSE. While \code{hourlyFWI=TRUE}, daily BUI is required for the
#' computation of FWI.
#' @return \code{hffmcRaster} returns a vector of hourly or sub-hourly FFMC
#' values, which may contain 1 or multiple elements. Optionally when
#' \code{hourlyFWI=TRUE}, the function also output a data.frame contains input
#' weatherstream as well as the hourly or sub-hourly FFMC, ISI, FWI, and DSR.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, Brett Moore, and Mike
#' Flannigan
#' @seealso \code{\link{fbp}}, \code{\link{fwi}}, \code{\link{hffmc}}
#' @references Van Wagner, C.E. 1977. A method of computing fine fuel moisture
#' content throughout the diurnal cycle. Environment Canada, Canadian Forestry
#' Service, Petawawa Forest Experiment Station, Chalk River, Ontario.
#' Information Report PS-X-69.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/25591.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' require(raster)
#' ## load the test data for the first hour, namely hour01:
#' hour01src <- system.file("extdata","test_rast_hour01.tif",package="cffdrs")
#' hour01 <- stack(hour01src)
#' # Assign names to the layers:
#' names(hour01)<-c("temp","rh","ws","prec")
#' # (1) Default, based on the initial value: 
#' foo<-hffmcRaster(hour01)
#' plot(foo)
#' ### Additional, longer running examples ###
#' # (2) Based on previous day's hffmc:
#' # load the test data for the second hour, namely hour02:
#' hour02src <- system.file("extdata","test_rast_hour02.tif",package="cffdrs")
#' hour02 <- stack(hour02src)
#' # Assign variable names to the layers:
#' names(hour02)<-c("temp","rh","ws","prec")
#' \donttest{foo1<-hffmcRaster(hour02,ffmc_old=foo)}
#' \donttest{plot(foo1)}
#' # (3) Calculate other hourly FWI components (ISI, FWI, and DSR):
#' # Need BUI layer, 
#' bui<-hour02$temp
#' values(bui)<-50
#' hour02<-stack(hour02,bui)
#' # Re-assign variable names to the layers:
#' names(hour02)<-c("temp","rh","ws","prec","bui")
#' # Calculate all the variables:
#' \donttest{foo2<-hffmcRaster(hour02,ffmc_old=foo,hourlyFWI=TRUE)}
#' # Visualize the maps:
#' \donttest{plot(foo2)}
#' 
#' @export hffmcRaster
hffmcRaster <- function(weatherstream, ffmc_old = 85, time.step = 1, 
                        hourlyFWI = FALSE) {
 
  names(weatherstream) <- tolower(names(weatherstream))
  #local scope variables
  Tp <- weatherstream$temp
  H  <- weatherstream$rh
  W  <- weatherstream$ws
  ro <- weatherstream$prec
  #Check that the parameters are correct
  if (is.numeric(ffmc_old)){
    Fo <- Tp
    values(Fo)<-ffmc_old
  }else{Fo<-ffmc_old}
  if (!exists("Tp") | is.null(Tp)) 
    warning("temperature (temp) is missing!")
  if (!exists("ro") | is.null(ro)) 
    warning("precipitation (prec) is missing!")
  if (!exists("W") | is.null(W)) 
    warning("wind speed (ws) is missing!")
  if (!exists("H") | is.null(H)) 
    warning("relative humidity (rh) is missing!")
  #Eq. 1 (with a more precise multiplier than the daily)
  mo <- 147.27723 * (101 - Fo) / (59.5 + Fo)
  
  mr1 <- mo
  mr1[mr1 > 150] <- NA
  #masking values
  rf1 <- mask(ro, mr1)
  #Eqs. 3a (Van Wagner & Pickett 1985)
  mr1 <- mr1 + 42.5 * rf1 * exp(-100 / (251 - mr1)) * (1 - exp(-6.93 / rf1))
  mr2 <- mo
  mr2[mr2 <= 150] <- NA
  rf2 <- mask(ro, mr2)
  #Eqs. 3b (Van Wagner & Pickett 1985)
  mr2 <- mr2 + 42.5 * rf2 * exp(-100 / (251 - mr2)) *(1 - exp(-6.93 / rf2)) + 
         0.0015 * ((mr2 - 150)^2) * (rf2^0.5)
  mr3 <- cover(mr1,mr2)
  #The real moisture content of pine litter ranges up to about 250 percent,
  # so we cap it at 250
  mr3[mr3 > 250] <- 250
  #raster manipulation to speed up processing
  r1 <- ro
  r1[r1 <= 0] <- NA
  mr<-mask(mr3, r1)
  r1 <- ro
  r1[r1 > 0] <- NA
  mo1 <- mask(mo, r1)
  mo <- cover(mo1, mr)
  #Eq. 2a Equilibrium moisture content from drying
  Ed <- 0.942 * (H^0.679) + 11 * exp((H - 100)/10) + 0.18 * (21.1 - Tp) * 
        (1 - exp(-0.115 * H))
  #Eq. 3a Log drying rate at the normal temperature of 21.1C
  ko <- 0.424 * (1 - (H / 100)^1.7) + 0.0694 * (W^0.5) * (1 - (H / 100)^8)
  #Eq. 3b
  kd <- ko * 0.0579 * exp(0.0365 * Tp)
  #Eq. 8 (Van Wagner & Pickett 1985)
  md <- Ed + (mo - Ed) * (10^(-1*kd*time.step))
  #Eq. 2b Equilibrium moisture content from wetting  
  Ew <- 0.618 * (H^0.753) + 10 * exp((H - 100) / 10) + 0.18 * (21.1 - Tp) * 
        (1 - exp(-0.115 * H))
  #Eq. 7a Log wetting rate at the normal temperature of 21.1 C 
  k1 <- 0.424 * (1 - ((100 - H)/100)^1.7) + 0.0694 * (W^0.5) * 
        (1 - ((100 - H) / 100)^8)
  #Eq. 4b
  kw <- k1 * 0.0579 * exp(0.0365 * Tp)
  #Eq. 8 (Van Wagner & Pickett 1985)
  mw <- Ew - (Ew - mo) * (10^(-1*kw*time.step)) 
  #Constraints using raster manipulation
  m0 <- overlay(mo, Ed, fun = function(a, b){ return(a > b) })
  md[m0 == 0] <- NA
  mw[m0 == 1] <- NA
  m <- cover(md, mw)

  m1 <- overlay(Ed, mo, Ew, fun = function(a, b, c) return(a >= b & b >= c))
  mo[m1 == 0] <- NA
  m[m1 == 1] <- NA
  m <- cover(mo, m)
  #Eq. 6 - Final hffmc calculation (modified 3rd constant to 147.27723)
  fo <- 59.5 * (250 - m) / (147.27723 + m)
  fo[fo <= 0] <- 0
  #Calculate hourly isi and fwi
  if (hourlyFWI){
    if ("bui" %in% names(weatherstream)){
      bui <- weatherstream$bui

      #Calculate ISI
      fW <- exp(0.05039 * W)
      fm <- 147.2 * (101 - fo) / (59.5 + fo)
      fF <- 91.9 * exp(-0.1386 * fm) * (1 + (fm^5.31) / 49300000)
      isi <- 0.208 * fW * fF
      
      #Calculate FWI
      bui1 <- bui
      bui1[bui1 <= 80] <- NA
      bui1 <- 0.1 * isi * (1000 / (25 + 108.64 / exp(0.023 * bui1)))
      
      bui2 <- bui
      bui2[bui1 > 80] <- NA
      bui2 <- 0.1 * isi * (0.626 * (bui2^0.809) + 2)
      bb <- cover(bui1, bui2)
      
      bb1 <- bb
      bb1[bb > 1] <- NA
      
      bb2 <- bb
      bb2[bb <= 1] <- NA
      bb2 <- exp(2.72 * ((0.434 * log(bb2))^0.647))
      fwi <- cover(bb1, bb2)
      #Calculate DSR
      dsr <- 0.0272 * (fwi^1.77)
      #Create Raster Stack for the ouput
      output <- stack(fo, isi, fwi, dsr)
      names(output) <- c("hffmc", "hisi", "hfwi", "hdsr")
      return(output)
    } else {
      warning("Daily BUI is required to calculate hourly FWI")
    }
  } else {
    names(fo) <- "hffmc"
    return(fo)
  }
}

