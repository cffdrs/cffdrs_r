#' Fire Behavior Prediction System function
#' 
#' \code{fbp} calculates the outputs from the Canadian Forest Fire Behavior
#' Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) based on
#' given fire weather and fuel moisture conditions (from the Canadian Forest
#' Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, date, and
#' slope. Fire weather, for the purpose of FBP System calculation, comprises
#' observations of 10 m wind speed and direction at the time of the fire, and
#' two associated outputs from the Fire Weather Index System, the Fine Fuel
#' Moisture Content (FFMC) and Buildup Index (BUI). FWI System components can
#' be calculated with the sister function \code{\link{fwi}}.
#' 
#' The Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada
#' Fire Danger Group 1992) is a subsystem of the Canadian Forest Fire Danger
#' Rating System, which also includes the Canadian Forest Fire Weather Index
#' (FWI) System. The FBP System provides quantitative estimates of head fire
#' spread rate, fuel consumption, fire intensity, and a basic fire description
#' (e.g., surface, crown) for 16 different important forest and rangeland types
#' across Canada. Using a simple conceptual model of the growth of a point
#' ignition as an ellipse through uniform fuels and under uniform weather
#' conditions, the system gives, as a set of secondary outputs, estimates of
#' flank and back fire behavior and consequently fire area perimeter length and
#' growth rate.
#' 
#' The FBP System evolved since the mid-1970s from a series of regionally
#' developed burning indexes to an interim edition of the nationally develop
#' FBP system issued in 1984. Fire behavior models for spread rate and fuel
#' consumption were derived from a database of over 400 experimental, wild and
#' prescribed fire observations. The FBP System, while providing quantitative
#' predictions of expected fire behavior is intended to supplement the
#' experience and judgment of operational fire managers (Hirsch 1996).
#' 
#' The FBP System was updated with some minor corrections and revisions in 2009
#' (Wotton et al. 2009) with several additional equations that were initially
#' not included in the system. This fbp function included these updates and
#' corrections to the original equations and provides a complete suite of fire
#' behavior prediction variables. Default values of optional input variables
#' provide a reasonable mid-range setting. Latitude, longitude, elevation, and
#' the date are used to calculate foliar moisture content, using a set of
#' models defined in the FBP System; note that this latitude/longitude-based
#' function is only valid for Canada. If the Foliar Moisture Content (FMC) is
#' specified directly as an input, the fbp function will use this value
#' directly rather than calculate it. This is also true of other input
#' variables.
#' 
#' Note that Wind Direction (WD) is the compass direction from which wind is
#' coming. Wind azimuth (not an input) is the direction the wind is blowing to
#' and is 180 degrees from wind direction; in the absence of slope, the wind
#' azimuth is coincident with the direction the head fire will travel (the
#' spread direction azimuth, RAZ). Slope aspect is the main compass direction
#' the slope is facing. Slope azimuth (not an input) is the direction a head
#' fire will spread up slope (in the absence of wind effects) and is 180
#' degrees from slope aspect (Aspect).  Wind direction and slope aspect are the
#' commonly used directional identifiers when specifying wind and slope
#' orientation respectively.  The input theta specifies an angle (given as a
#' compass bearing) at which a user is interested in fire behavior predictions;
#' it is typically some angle off of the final spread rate direction since if
#' for instance theta=RAZ (the final spread azimuth of the fire) then the rate
#' of spread at angle theta (TROS) will be equivalent to ROS.
#' 
#' @param input The input data, a data.frame containing fuel types, fire
#' weather component, and slope (see below). Each vector of inputs defines a
#' single FBP System prediction for a single fuel type and set of weather
#' conditions. The data.frame can be used to evaluate the FBP System for a
#' single fuel type and instant in time, or multiple records for a single point
#' (e.g., one weather station, either hourly or daily for instance) or multiple
#' points (multiple weather stations or a gridded surface). All input variables
#' have to be named as listed below, but they are case insensitive, and do not
#' have to be in any particular order. Fuel type is of type character; other
#' arguments are numeric. Missing values in numeric variables could either be
#' assigned as NA or leave as blank.\cr\cr
#' 
#' 
#' \tabular{lll}{ 
#' \bold{Required Inputs:}\tab\tab\cr 
#' \bold{Input} \tab \bold{Description/Full name} \tab \bold{Defaults}\cr 
#' 
#' \var{FuelType} 
#' \tab FBP System Fuel Type including "C-1",\cr
#' \tab"C-2", "C-3", "C-4","C-5", "C-6", "C-7",\cr
#' \tab "D-1", "M-1", "M-2", "M-3", "M-4", "NF",\cr
#' \tab "D-1", "S-2", "S-3", "O-1a", "O-1b", and\cr
#' \tab  "WA", where "WA" and "NF" stand for \cr
#' \tab "water" and "non-fuel", respectively.\cr\cr
#'  
#' \var{LAT} \tab Latitude [decimal degrees] \tab 55\cr 
#' \var{LONG} \tab Longitude [decimal degrees] \tab -120\cr 
#' \var{FFMC} \tab Fine fuel moisture code [FWI System component] \tab 90\cr 
#' \var{BUI} \tab Buildup index [FWI System component] \tab 60\cr 
#' \var{WS} \tab Wind speed [km/h] \tab 10\cr
#' \var{GS} \tab Ground Slope [percent] \tab 0\cr 
#' \var{Dj} \tab Julian day \tab 180\cr 
#' \var{Aspect} \tab Aspect of the slope [decimal degrees] \tab 0\cr\cr 
#' 
#' \bold{Optional Inputs (1):}
#' \tab Variables associated with certain fuel \cr
#' \tab types. These could be skipped if relevant \cr
#' \tab fuel types do not appear in the input data.\cr\cr
#' 
#' \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr 
#' 
#' \var{PC} \tab Percent Conifer for M1/M2 [percent] \tab 50\cr 
#' \var{PDF} \tab Percent Dead Fir for M3/M4 [percent] \tab 35\cr
#' \var{cc} \tab Percent Cured for O1a/O1b [percent] \tab 80\cr 
#' \var{GFL} \tab Grass Fuel Load [kg/m^2] \tab 0.35\cr\cr 
#' 
#' \bold{Optional Inputs (2):} 
#' \tab Variables that could be ignored without \cr
#' \tab causing major impacts to the primary outputs\cr\cr
#' 
#' \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr 
#' \var{CBH}   \tab Crown to Base Height [m] \tab 3\cr 
#' \var{WD}    \tab Wind direction [decimal degrees] \tab 0\cr 
#' \var{Accel} \tab Acceleration: 1 = point, 0 = line \tab 0\cr 
#' \var{ELV*}  \tab Elevation [meters above sea level] \tab NA\cr 
#' \var{BUIEff}\tab Buildup Index effect: 1=yes, 0=no \tab 1\cr 
#' \var{D0}    \tab Julian day of minimum Foliar Moisture Content \tab 0\cr 
#' \var{hr}    \tab Hours since ignition \tab 1\cr 
#' \var{ISI}   \tab Initial spread index \tab 0\cr 
#' \var{CFL}   \tab Crown Fuel Load [kg/m^2]\tab 1.0\cr 
#' \var{FMC}   \tab Foliar Moisture Content if known [percent] \tab 0\cr 
#' \var{SH}    \tab C-6 Fuel Type Stand Height [m] \tab 0\cr 
#' \var{SD}    \tab C-6 Fuel Type Stand Density [stems/ha] \tab 0\cr 
#' \var{theta} \tab Elliptical direction of calculation [degrees] \tab 0\cr\cr }
#' 
#' @param output FBP output offers 3 options (see details in \bold{Values}
#' section):
#' 
#' \tabular{lc}{ \bold{Outputs} \tab \bold{Number of outputs}\cr 
#' \var{Primary(\bold{default})} \tab 8\cr 
#' \var{Secondary} \tab 34\cr 
#' \var{All} \tab 42\cr\cr}
#' 
#' @param m Optimal number of pixels at each iteration of computation when
#' \code{nrow(input) >= 1000}. Default \code{m = NULL}, where the function will
#' assign \code{m = 1000} when \code{nrow(input)} is between 1000 and 500,000,
#' and \code{m = 3000} otherwise. By including this option, the function is
#' able to process large dataset more efficiently. The optimal value may vary
#' with different computers.
#' 
#' @param cores Number of CPU cores (integer) used in the computation, default
#' is 1.  By signing \code{cores > 1}, the function will apply parallel
#' computation technique provided by the \code{foreach} package, which
#' significantly reduces the computation time for large input data (over a
#' million records). For small dataset, \code{cores=1} is actually faster.
#' 
#' @return \code{fbp} returns a dataframe with primary, secondary, or all
#' output variables, a combination of the primary and secondary outputs.
#' 
#' \bold{Primary} FBP output includes the following 8 variables: 
#' 
#' \item{CFB}{Crown Fraction Burned by the head fire} 
#' \item{CFC}{Crown Fuel Consumption [kg/m^2]} 
#' \item{FD}{Fire description (1=Surface, 2=Intermittent, 3=Crown)} 
#' \item{HFI}{Head Fire Intensity [kW/m]}
#' \item{RAZ}{Spread direction azimuth [degrees]} 
#' \item{ROS}{Equilibrium Head Fire Rate of Spread [m/min]} 
#' \item{SFC}{Surface Fuel Consumption [kg/m^2]} 
#' \item{TFC}{Total Fuel Consumption [kg/m^2]}
#' 
#' \bold{Secondary} FBP System outputs include the following 34 raster layers. In order
#' to calculate the reliable secondary outputs, depending on the outputs, 
#' optional inputs may have to be provided.  
#' 
#' \item{BE}{BUI effect on spread rate} 
#' \item{SF}{Slope Factor (multiplier for ROS increase upslope)} 
#' \item{ISI}{Initial Spread Index} 
#' \item{FFMC}{Fine fuel moisture code [FWI System component]} 
#' \item{FMC}{Foliar Moisture Content [\%]} 
#' \item{Do}{Julian Date of minimum FMC} 
#' \item{RSO}{Critical spread rate for crowning [m/min]}
#' \item{CSI}{Critical Surface Intensity for crowning [kW/m]}
#' \item{FROS}{Equilibrium Flank Fire Rate of Spread [m/min]}
#' \item{BROS}{Equilibrium Back Fire Rate of Spread [m/min]}
#' \item{HROSt}{Head Fire Rate of Spread at time hr [m/min]}
#' \item{FROSt}{Flank Fire Rate of Spread at time hr [m/min]}
#' \item{BROSt}{Back Fire Rate of Spread at time hr [m/min]}
#' \item{FCFB}{Flank Fire Crown Fraction Burned} 
#' \item{BCFB}{Back Fire Crown Fraction Burned} 
#' \item{FFI}{Equilibrium Spread Flank Fire Intensity [kW/m]} 
#' \item{BFI}{Equilibrium Spread Back Fire Intensity [kW/m]} 
#' \item{FTFC}{Flank Fire Total Fuel Consumption [kg/m^2] } 
#' \item{BTFC}{Back Fire Total Fuel Consumption [kg/m^2] } 
#' \item{DH}{Head Fire Spread Distance after time hr [m] }
#' \item{DB}{Back Fire Spread Distance after time hr [m] }
#' \item{DF}{Flank Fire Spread Distance after time hr [m] }
#' \item{TI}{Time to Crown Fire Initiation [hrs since ignition] }
#' \item{FTI}{Time to Flank Fire Crown initiation [hrs since ignition]} 
#' \item{BTI}{Time to Back Fire Crown initiation [hrs since ignition]} 
#' \item{LB}{Length to Breadth ratio} 
#' \item{LBt}{Length to Breadth ratio after elapsed time hr } 
#' \item{WSV}{Net vectored wind speed [km/hr]} 
#' \item{TROS*}{Equilibrium Rate of Spread at bearing theta [m/min] } 
#' \item{TROSt*}{Rate of Spread at bearing theta at time t [m/min] } 
#' \item{TCFB*}{Crown Fraction Burned at bearing theta } 
#' \item{TFI*}{Fire Intensity at bearing theta [kW/m] } 
#' \item{TTFC*}{Total Fuel Consumption at bearing theta [kg/m^2] } 
#' \item{TTI*}{Time to Crown Fire initiation at bearing theta [hrs since ignition] }
#' 
#' *These outputs represent fire behaviour at a point on the perimeter of an
#' elliptical fire defined by a user input angle theta. theta represents the
#' bearing of a line running between the fire ignition point and a point on the
#' perimeter of the fire. It is important to note that in this formulation the
#' theta is a bearing and does not represent the angle from the semi-major axis
#' (spread direction) of the ellipse. This formulation is similar but not
#' identical to methods presented in Wotton et al (2009) and Tymstra et al
#' (2009).
#' @author Xianli Wang, Alan Cantin, Marc-André Parisien, Mike Wotton, Kerry
#' Anderson, and Mike Flannigan
#' @seealso \code{\link{fwi}, \link{fbpRaster}}
#' @references 1.  Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction
#' (FBP) System: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest
#' Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.
#' 
#' 2.  Forestry Canada Fire Danger Group. 1992. Development and structure of
#' the Canadian Forest Fire Behavior Prediction System. Forestry Canada,
#' Ottawa, Ontario Information Report ST-X-3. 63 p.
#' \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}
#' 
#' 3.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions
#' to the 1992 Canadian forest fire behavior prediction system. Nat. Resour.
#' Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario,
#' Canada. Information Report GLC-X-10, 45p.
#' \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}
#' 
#' 4.  Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development
#' and structure of Prometheus: the Canadian wildland fire growth simulation
#' Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB.
#' Inf. Rep. NOR-X-417.\url{https://d1ied5g1xfgpx8.cloudfront.net/pdfs/31775.pdf}
#' @keywords methods
#' @examples
#' 
#' library(cffdrs)
#' # The dataset is the standard test data for FPB system
#' # provided by Wotton et al (2009)
#' data("test_fbp")
#' head(test_fbp)
#' #  id FuelType LAT LONG ELV FFMC BUI   WS WD GS  Dj  D0         hr PC PDF GFL cc theta Accel Aspect BUIEff CBH CFL ISI
#' #1  1      C-1  55  110  NA   90 130 20.0  0 15 182  NA 0.33333333 NA  NA  NA  NA     0     1    270      1  NA  NA   0
#' #2  2       C2  50   90  NA   97 119 20.4  0 75 121  NA 0.33333333 NA  NA  NA  NA     0     1    315      1  NA  NA   0
#' #3  3      C-3  55  110  NA   95  30 50.0  0  0 182  NA 0.08333333 NA  NA  NA  NA     0     1    180      1  NA  NA   0
#' #4  4      C-4  55  105 200   85  82  0.0 NA 75 182  NA 0.50000000 NA  NA  NA  NA     0     1    315      1  NA  NA   0
#' #5  5       c5  55  105  NA   88  56  3.4  0 23 152 145 0.50000000 NA  NA  NA  NA     0     1    180      1  NA  NA   0
#' 
#' #Primary output (default)
#' fbp(test_fbp)
#' #or
#' fbp(test_fbp,output="Primary") 
#' #or 
#' fbp(test_fbp,"P")
#' #Secondary output          
#' fbp(test_fbp,"Secondary")
#' #or
#' fbp(test_fbp,"S")
#' #All output          
#' fbp(test_fbp,"All")
#' #or
#' fbp(test_fbp,"A")
#' #For a single record:
#' fbp(test_fbp[7,])  	
#' #For a section of the records:
#' fbp(test_fbp[8:13,])	
#' #fbp function produces the default values if no data is fed to
#' #the function:
#' fbp()
#' 
#' @export fbp
fbp  <- function(input = NULL, output = "Primary", m = NULL, cores = 1){  

  #hack to avoid Note about no visible binding for global variable ID
  #http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  #look at the globalvariables() option or others in place of this issue
  # do not remove this comment until resolved
  ID <- NULL 
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  #If input is not provided, then calculate FBP with default values
  if (is.null(input)){
    fullList <- .FBPcalc(input)

  } else {
    #determine optimal number of pixels to process at each iteration
    if (is.null(m)){
      m <- ifelse(nrow(input) > 500000, 3000, 1000)
    }
    m <- ifelse(nrow(input) >= m, m, nrow(input))
    n0 <- round(nrow(input) / m)
    n <- ifelse(m * n0 >= nrow(input), n0, n0 + 1)
    #Set up parallel processing, if # of cores is entered
    if (cores > 1){
      #create and register a set of parallel R instances for foreach
      cl <- makeCluster(cores)
      registerDoParallel(cl)
      #process in parallel
      ca <- foreach(i=1:n, .packages='cffdrs') %dopar% {
        if (i==n){
          #Run FBP functions
          to.ls<-.FBPcalc(input[((i-1)*m+1):nrow(input),],output=output)
        }else {
          #Run FBP functions
          to.ls<-.FBPcalc(input[((i-1)*m+1):(i*m),],output=output)
        }
        to.ls
      }
      #close the processes
      stopCluster(cl)
      registerDoSEQ()
    #Run only a single process
    } else {
      ca <- vector('list',n)
      
      for(i in 1:n) {
        if(i == n) {
          foo <- input[((i - 1) * m + 1):nrow(input), ]
        } else {
          foo <- input[((i - 1) * m + 1):(i * m), ]
        }
        #Run FBP functions
        ca[[i]] <- .FBPcalc(foo, output = output)
      }    
    }
    #create a single keyed data table
    fullList <- rbindlist(ca)
    setkey(fullList, ID)
    #convert to data frame
    fullList <- as.data.frame(fullList)
  }
  
  return(fullList)
}

