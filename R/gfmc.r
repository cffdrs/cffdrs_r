#' Grass Fuel Moisture Code
#' 
#' \code{gfmc} calculates both the moisture content of the surface of a fully
#' cured matted grass layer and also an equivalent Grass Fuel Moisture Code
#' (gfmc) (Wotton, 2009) to create a parallel with the hourly ffmc (see the
#' \code{\link{fwi}} and \code{\link{hffmc}}functions). The calculation is
#' based on hourly (or sub-hourly) weather observations of temperature,
#' relative humidity, wind speed, rainfall, and solar radiation. The user must
#' also estimate an initial value of the gfmc for the layer. This function
#' could be used for either one weather station or multiple weather stations.
#' 
#' The Canadian Forest Fire Danger Rating System (CFFDRS) is used throughout
#' Canada, and in a number of countries throughout the world, for estimating
#' fire potential in wildland fuels. This new Grass Fuel Moisture Code (GFMC)
#' is an addition (Wotton 2009) to the CFFDRS and retains the structure of that
#' System's hourly Fine Fuel Moisture Code (HFFMC) (Van Wagner 1977). It tracks
#' moisture content in the top 5 cm of a fully-cured and fully-matted layer of
#' grass and thus is representative of typical after winter conditions in areas
#' that receive snowfall.  This new moisture calculation method outputs both
#' the actual moisture content of the layer and also the transformed moisture
#' Code value using the FFMC's FF-scale.  In the CFFDRS the moisture codes are
#' in fact relatively simple transformations of actual moisture content such
#' that decreasing moisture content (increasing dryness) is indicated by an
#' increasing Code value. This moisture calculation uses the same input weather
#' observations as the hourly FFMC, but also requires an estimate of solar
#' radiation incident on the fuel.
#' 
#' @param input A dataframe containing input variables of daily noon weather
#' observations. Variable names have to be the same as in the following list,
#' but they are case insensitive. The order in which the input variables are
#' entered is not important.
#' 
#' \tabular{lll}{ 
#' \var{temp} \tab (required) \tab Temperature (centigrade)\cr
#' \var{rh} \tab (required) \tab Relative humidity (\%)\cr 
#' \var{ws} \tab (required) \tab 10-m height wind speed (km/h)\cr 
#' \var{prec} \tab (required) \tab 1-hour rainfall (mm)\cr
#' \var{isol} \tab (required) \tab Solar radiation (kW/m^2)\cr 
#' \var{mon} \tab (recommended) \tab Month of the year (integer' 1-12)\cr 
#' \var{day} \tab (optional) \tab Day of the month (integer)\cr }
#' @param GFMCold Previous value of GFMC (i.e. value calculated at the previous
#' time step)[default is 85 (which corresponds to a moisture content of about
#' 16\%)]. On the first calculation this is the estimate of the GFMC value at
#' the start of the time step. The \code{GFMCold} argument can accept a single
#' initial value for multiple weather stations, and also accept a vector of
#' initial values for multiple weather stations.  NOTE: this input represents
#' the CODE value, not a direct moisture content value. The CODE values in the
#' Canadian FWI System increase within decreasing moisture content. To roughly
#' convert a moisture content value to a CODE value on the FF-scale (used in
#' the FWI Systems FFMC) use \code{GFMCold} =101-gmc (where gmc is moisture
#' content in \%)
#' 
#' @param time.step Time step (hour) [default 1 hour]
#' @param roFL The nominal fuel load of the fine fuel layer, default is 0.3
#' kg/m^2
#' @param batch Whether the computation is iterative or single step, default is
#' TRUE. When \code{batch=TRUE}, the function will calculate hourly or
#' sub-hourly GFMC for one weather station over a period of time iteratively.
#' If multiple weather stations are processed, an additional "id" column is
#' required in the input to label different stations, and the data needs to be
#' sorted by time sequence and "id".  If \code{batch=FALSE}, the function
#' calculates only one time step (1 hour) base on either the previous hourly
#' GFMC or the initial start value.
#' @param out Output format, default is "GFMCandMC", which contains both GFMC
#' and moisture content (MC) in a data.frame format. Other choices include:
#' "GFMC", "MC", and "ALL", which include both the input and GFMC and MC.
#' @return \code{gfmc} returns GFMC and moisture content (MC) values
#' collectively (default) or separately.
#' @author Xianli Wang, Mike Wotton, Alan Cantin, and Mike Flannigan
#' @seealso \code{\link{fwi}}, \code{\link{hffmc}}
#' @references Wotton, B.M. 2009. A grass moisture model for the Canadian
#' Forest Fire Danger Rating System. In: Proceedings 8th Fire and Forest
#' Meteorology Symposium, Kalispell, MT Oct 13-15, 2009. Paper 3-2.
#' \url{https://ams.confex.com/ams/pdfpapers/155930.pdf}
#' 
#' Van Wagner, C.E. 1977. A method of computing fine fuel moisture content
#' throughout the diurnal cycle. Environment Canada, Canadian Forestry Service,
#' Petawawa Forest Experiment Station, Chalk River, Ontario. Information Report
#' PS-X-69. \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/25591.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' #load the test data
#' data("test_gfmc")
#' # show the data format:
#' head(test_gfmc)
#' #     yr mon day hr temp   rh   ws prec  isol
#' # 1 2006   5  17 10 15.8 54.6  5.0    0 0.340
#' # 2 2006   5  17 11 16.3 52.9  5.0    0 0.380
#' # 3 2006   5  17 12 18.8 45.1  5.0    0 0.626
#' # 4 2006   5  17 13 20.4 40.8  9.5    0 0.656
#' # 5 2006   5  17 14 20.1 41.7  8.7    0 0.657
#' # 6 2006   5  17 15 18.6 45.8 13.5    0 0.629
#' # (1) gfmc default: 
#' # Re-order the data by year, month, day, and hour:
#' dat<-test_gfmc[with(test_gfmc,order(yr,mon,day,hr)),]
#' # Because the test data has 24 hours input variables 
#' # it is possible to calculate the hourly GFMC continuously 
#' # through multiple days(with the default initial GFMCold=85):
#' dat$gfmc_default<-gfmc(dat) 
#' # two variables will be added to the input, GFMC and MC
#' head(dat)
#' # (2) For multiple weather stations:
#' # One time step (1 hour) with default initial value:
#'   foo<-gfmc(dat,batch=FALSE)
#' # Chronical hourly GFMC with only one initial 
#' # value (GFMCold=85), but multiple weather stations. 
#' # Note: data is ordered by date/time and the station id. Subset 
#' # the data by keeping only the first 10 hours of observations 
#' # each day:
#' dat1<-subset(dat,hr%in%c(0:9))
#' #assuming observations were from the same day but with 
#' #9 different weather stations:
#' dat1$day<-NULL
#' dat1<-dat1[with(dat1,order(yr,mon,hr)),]
#' dat1$id<-rep(1:8,nrow(dat1)/8)
#' #check the data:
#' head(dat1)
#' # Calculate GFMC for multiple stations:
#' dat1$gfmc01<-gfmc(dat1,batch=TRUE)
#' # We can provide multiple initial GFMC (GFMCold) as a vector:   
#' dat1$gfmc02<- gfmc(dat1,GFMCold = sample(70:100,8, replace=TRUE),batch=TRUE)
#' # (3)output argument
#' ## include all inputs and outputs:
#' dat0<-dat[with(dat,order(yr,mon,day,hr)),]
#' foo<-gfmc(dat,out="ALL")
#' ## subhourly time step:
#' gfmc(dat0,time.step=1.5)
#' 
#' @export gfmc
gfmc <- function(input, GFMCold = 85, batch = TRUE, time.step = 1, roFL = 0.3,
                 out = "GFMCandMC") {
  #############################################################################
  # Description: Calculation of the Grass Fuel Moisture Code. This calculates
  #              the moisture content of both the surface of a fully cured
  #              matted grass layer and also an equivalent Grass Fuel Moisture
  #              Code. All equations come from Wotton (2009) as cited below
  #              unless otherwise specified.
  # 
  #              Wotton, B.M. 2009. A grass moisture model for the Canadian 
  #              Forest Fire Danger Rating System. In: Proceedings 8th Fire and
  #              Forest Meteorology Symposium, Kalispell, MT Oct 13-15, 2009. 
  #              Paper 3-2. https://ams.confex.com/ams/pdfpapers/155930.pdf
  #           
  # Args: input (data.frame):
  #         temp (required)	    Temperature (centigrade)
  #         rh	 (required)	    Relative humidity (%)
  #         ws	 (required)	    10-m height wind speed (km/h)
  #         prec (required)	    1-hour rainfall (mm)
  #         isol (required)	    Solar radiation (kW/m^2)
  #         mon	 (recommended)	Month of the year (integer 1-12) _Currently not implemented_
  #         day	 (optional)	    Day of the month (integer) _Currently not implemented_
  #       GFMCold:    GFMC from yesterday (double, default=85)
  #       batch:      Compute multiple locations (TRUE/FALSE, default=TRUE)
  #       time.step:  The hourly time steps (integer hour, default=1)
  #       roFL:       Nominal fuel load of the fine fuel layer 
  #                   (kg/m^2 double, default=0.3)
  #       out:        Output format (GFMCandMC/MC/GFMC/ALL, default=GFMCandMC)
  #       
  # Returns: Returns a data.frame of either MC, GMFC, All, or GFMCandMC
  #
  #############################################################################
  t0 <- time.step
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

  #check for issues with batching the function
  if (batch){
    if ("id" %in% names(input)) {
      n <- length(unique(input$id))
      if(length(unique(input[1:n, "id"])) != n){
        stop("Multiple stations have to start and end at the same dates/time, 
             and input data must be sorted by date/time and id")
      }
    } else {
      n <- 1
    }
  } else {n <- nrow(input)}
  
  if (length(temp)%%n != 0)
    warning("Input data do not match with number of weather stations")

  if (length(GFMCold) != n & length(GFMCold) == 1){
    warning("One GFMCold value for multiple weather stations") 
    GFMCold <- rep(GFMCold, n)
  }
  
  if (length(GFMCold) != n & length(GFMCold) > 1)
    stop("Number of GFMCold doesn't match number of wx stations")
  validOutTypes = c("GFMCandMC", "MC", "GFMC", "ALL")
  if(!(out %in% validOutTypes)){
    stop(paste("'",out, "' is an invalid 'out' type.", sep=""))
  }
  
  #get the length of the data stream
  n0 <- length(temp)%/%n
  GFMC <- NULL
  MC <- NULL
  #iterate through timesteps
  for (i in 1:n0){
    #k is the data for all stations by time step
    k <- (n * (i - 1) + 1):(n * i)
    #Eq. 13 - Calculate previous moisture code
    MCold <- 147.2772 * ((101 - GFMCold) / (59.5 + GFMCold))
    #Eq. 11 - Calculate the moisture content of the layer in % after rainfall
    MCr <- ifelse(prec[k] > 0, MCold + 100 * (prec[k] / roFL), MCold)
    #Constrain to 250
    MCr <- ifelse(MCr > 250, 250, MCr)
    MCold <- MCr
    #Eq. 2 - Calculate Fuel temperature
    Tf <- temp[k] + 35.07 * isol[k] * exp(-0.06215 * ws[k])
    #Eq. 3 - Calculate Saturation Vapour Pressure (Baumgartner et a. 1982)
    eS.T <- 6.107 * 10^(7.5 * temp[k] / (237 + temp[k]))
    #Eq. 3 for Fuel temperature
    eS.Tf <- 6.107 * 10^(7.5 * Tf / (237 + Tf))
    #Eq. 4 - Calculate Fuel Level Relative Humidity
    RH.f <- rh[k] * (eS.T / eS.Tf)
    #Eq. 7 - Calculate Equilibrium Moisture Content for Drying phase
    EMC.D <- (1.62 * RH.f^0.532 + 13.7 * exp((RH.f - 100) / 13.0)) + 
              0.27 * (26.7 - Tf) * (1 - exp(-0.115 * RH.f))
    #Eq. 7 - Calculate Equilibrium Moisture Content for Wetting phase
    EMC.W <- (1.42 * RH.f^0.512 + 12.0 * exp((RH.f - 100) / 18.0)) + 
              0.27 * (26.7 - Tf) * (1 - exp(-0.115 * RH.f))
    #RH in terms of RH/100 for desorption
    Rf <- ifelse(MCold > EMC.D, RH.f / 100, rh)
    #RH in terms of 1-RH/100 for absorption
    Rf <- ifelse(MCold < EMC.W, (100 - RH.f) / 100, Rf)
    #Eq. 10 - Calculate Inverse Response time of grass (hours)
    K.GRASS <- 0.389633 * exp(0.0365 * Tf) * (0.424 * (1 - Rf^1.7) + 0.0694 * 
                                              sqrt(ws[k]) * (1 - Rf^8))
    #Fuel is drying, calculate Moisture Content
    MC0 <- ifelse(MCold > EMC.D, EMC.D + (MCold - EMC.D) * 
                  exp(-1.0 * log(10.0) * K.GRASS * t0), MCold)
    #Fuel is wetting, calculate moisture content
    MC0 <- ifelse(MCold < EMC.W, EMC.W + (MCold - EMC.W) * 
                  exp(-1.0 * log(10.0) * K.GRASS * t0), MC0)

    #Eq. 12 - Calculate GFMC
    GFMC0 <- 59.5 * ((250 - MC0) / (147.2772 + MC0))
    #Keep current and old GFMC
    GFMC <- c(GFMC, GFMC0)
    #Same for moisture content
    MC <- c(MC, MC0)
    #Reset vars
    GFMCold <- GFMC0
    MCold <- MC0
  }
  #Return requested 'out' type
  if (out=="ALL"){
    return(as.data.frame(cbind(input, GFMC, MC)))
  } else if(out == "GFMC"){
    return(GFMC)
  } else if (out == "MC"){
    return(MC)
  } else { #GFMCandMC
    return(data.frame(GFMC = GFMC, MC = MC))
  }
}

