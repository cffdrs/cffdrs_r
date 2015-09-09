\name{fbpRaster}
\alias{fbpRaster}
\title{
Raster-based Fire Behavior Prediction System Calculations
}
\description{
\code{fbpRaster} calculates the outputs from the Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) based on raster format fire weather and fuel moisture conditions (from the Canadian Forest Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, date, and slope. Fire weather, for the purpose of FBP System calculation, comprises observations of 10 m wind speed and direction at the time of the fire, and two associated outputs from the Fire Weather Index System, the Fine Fuel Moisture Content (FFMC) and Buildup Index (BUI). Raster-based FWI System components can be calculated with the sister function \code{\link{fwiRaster}}.
}
\usage{
fbpRaster(input,output="Primary",select=NULL,m=NULL,cores=1)
}
\arguments{
  \item{input}{The input data, a RasterStack containing fuel types, fire weather component, and slope layers (see below). Each vector of inputs defines a single FBP System prediction for a single fuel type and set of weather conditions. The RasterStack can be used to evaluate the FBP System for a single fuel type and instant in time, or multiple records for a single point (e.g., one weather station, either hourly or daily for instance) or multiple points (multiple weather stations or a gridded surface). All input variables have to be named as listed below, but they are case insensitive, and do not have to be in any particular order. Fuel type is of type character; other arguments are numeric. Missing values in numeric variables could either be assigned as NA or leave as blank.

	  \bold{Required Inputs:}\cr
    \tabular{lll}{
        \bold{Input} \tab \bold{Description/Full name} \tab \bold{Defaults}\cr
        \var{FuelType} \tab FBP System Fuel Type including "C-1", "C-2", "C-3", "C-4", "C-5",\tab"C2"\cr
                            "C-6", "C-7", "D-1", "M-1", "M-2", "M-3", "M-4", "NF", S-1", "S-2",\cr\tab
                            "S-3", "O-1a", "O-1b", and "WA", where "WA" and "NF" stand for "water"\cr\tab 
                            and"non-fuel", respectively.\cr\tab
                            Because raster format data cannot hold characters, we have to code \cr\tab
                            these fuel types in numeric codes. In sequence, the codes are c(1:19) \cr\tab
                            (see below). FuelType could also be converted as factor and assigned 
                            to the raster layer, the function will still work.\cr\cr
                       \tab \bold{Fuel Type	code}\cr
                       \tab C-1	     1\cr
                       \tab C-2	     2\cr
                       \tab C-3	     3\cr
                       \tab C-4	     4\cr
                       \tab C-5	     5\cr
                       \tab C-6	     6\cr
                       \tab C-7	     7\cr
                       \tab D-1	     8\cr
                       \tab M-1	     9\cr
                       \tab M-2	     10\cr
                       \tab M-3	     11\cr
                       \tab M-4	     12\cr
                       \tab NF	     13\cr
                       \tab O-1a	   14\cr
                       \tab O-1b	   15\cr
                       \tab S-1	     16\cr
                       \tab S-2	     17\cr
                       \tab S-3	     18\cr
                       \tab WA	     19\cr
                                            
        \var{LAT} \tab Latitude [decimal degrees] \tab 55\cr
        \var{LONG} \tab Longitude [decimal degrees] \tab -120\cr
        \var{FFMC} \tab Fine fuel moisture code [FWI System component] \tab 90\cr
        \var{BUI} \tab Buildup index [FWI System component] \tab 60\cr
        \var{WS} \tab Wind speed [km/h] \tab 10\cr
        \var{GS} \tab Ground Slope [percent] \tab 0\cr
        \var{Dj} \tab Julian day \tab 180\cr
        \var{Aspect} \tab Aspect of the slope [decimal degrees] \tab 0\cr
    }
    \bold{Optional Inputs (1):} Variables associated with certain fuel types. These could be skipped\cr if relevant fuel types do not appear in the input data.\cr
    \tabular{lll}{
        \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr       
        \var{PC} \tab Percent Conifer for M1/M2 [percent] \tab 50\cr
        \var{PDF} \tab Percent Dead Fir for M3/M4 [percent] \tab 35\cr
        \var{cc} \tab Percent Cured for O1a/O1b [percent] \tab 80\cr
        \var{GFL} \tab Grass Fuel Load [kg/m^2] \tab 0.35\cr
    }
    \bold{Optional Inputs (2):} Variables that could be ignored without causing major impacts to the\cr primary outputs\cr
    \tabular{lll}{    
        \bold{Input} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr    
        \var{CBH} \tab Crown to Base Height [m] \tab 3\cr        
        \var{WD} \tab Wind direction [decimal degrees] \tab 0\cr
        \var{Accel} \tab Acceleration: 1 = point, 0 = line \tab 0\cr        
        \var{ELV*} \tab Elevation [meters above sea level] \tab NA\cr
        \var{BUIEff} \tab Buildup Index effect: 1=yes, 0=no \tab 1\cr
        \var{D0} \tab Julian day of minimum Foliar Moisture Content \tab 0\cr
        \var{hr} \tab Hours since ignition \tab 1\cr
        \var{ISI} \tab Initial spread index \tab 0\cr
        \var{CFL} \tab Crown Fuel Load [kg/m^2] \tab 1.0\cr
        \var{FMC} \tab Foliar Moisture Content if known [percent] \tab 0\cr
        \var{SH} \tab C-6 Fuel Type Stand Height [m] \tab 0\cr
        \var{SD} \tab C-6 Fuel Type Stand Density [stems/ha] \tab 0\cr
        \var{theta} \tab Elliptical direction of calculation [degrees] \tab 0\cr
    }
  }
  \item{output}{
    FBP output offers 3 options (see details in \bold{Values} section):
    
    \tabular{ll}{
        \bold{Outputs} \tab \bold{Number of outputs}\cr
        \var{Primary (\bold{default})} \tab 8\cr
        \var{Secondary} \tab 34\cr
        \var{All} \tab 42\cr
    }
  }
  \item{select}{
    Selected outputs
  }
  \item{m}{
    m	Optimal number of pixels at each iteration of computation when \code{ncell(input) >= 1000}. Default m = NULL, where the function will assign m = 1000 when \code{ncell(input)} is between 1000 and 5,000,000, and m=3000 otherwise. By including this option, the function is able to process large dataset more efficiently. The optimal value may vary with different computers.    
  }
  \item{cores}{Number of CPU cores (integer) used in the computation, default is 1.  By signing \code{cores > 1}, the function will apply parallel computation technique provided by the \code{foreach} package, which significantly reduces the computation time for large input data (over a million grid points). For small dataset, \code{cores=1} is actually faster. 
  }
* Elevation is only used in the calculation of Foliar Moisture Content (FMC). However, FMC can also be calculated without elevation input. The default is to not use elevation in the calculation of FMC.
}
\details{
The Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) is a subsystem of the Canadian Forest Fire Danger Rating System, which also includes the Canadian Forest Fire Weather Index (FWI) System. The FBP System provides quantitative estimates of head fire spread rate, fuel consumption, fire intensity, and a basic fire description (e.g., surface, crown) for 16 different important forest and rangeland types across Canada. Using a simple conceptual model of the growth of a point ignition as an ellipse through uniform fuels and under uniform weather conditions, the system gives, as a set of secondary outputs, estimates of flank and back fire behavior and consequently fire area perimeter length and growth rate. 

The FBP System evolved since the mid-1970s from a series of regionally developed burning indexes to an interim edition of the nationally develop FBP system issued in 1984. Fire behavior models for spread rate and fuel consumption were derived from a database of over 400 experimental, wild and prescribed fire observations. The FBP System, while providing quantitative predictions of expected fire behavior is intended to supplement the experience and judgment of operational fire managers (Hirsch 1996). 

The FBP System was updated with some minor corrections and revisions in 2009 (Wotton et al. 2009) with several additional equations that were initially not included in the system. This fbp function included these updates and corrections to the original equations and provides a complete suite of fire behavior prediction variables. Default values of optional input variables provide a reasonable mid-range setting. Latitude, longitude, elevation, and the date are used to calculate foliar moisture content, using a set of models defined in the FBP System; note that this latitude/longitude-based function is only valid for Canada. If the Foliar Moisture Content (FMC) is specified directly as an input, the fbp function will use this value directly rather than calculate it. This is also true of other input variables.

Note that Wind Direction (WD) is the compass direction from which wind is coming. Wind azimuth (not an input) is the direction the wind is blowing to and is 180 degrees from wind direction; in the absence of slope, the wind azimuth is coincident with the direction the head fire will travel (the spread direction azimuth, RAZ). Slope aspect is the main compass direction the slope is facing. Slope azimuth (not an input) is the direction a head fire will spread up slope (in the absence of wind effects) and is 180 degrees from slope aspect (Aspect).  Wind direction and slope aspect are the commonly used directional identifiers when specifying wind and slope orientation respectively.  The input theta specifies an angle (given as a compass bearing) at which a user is interested in fire behavior predictions; it is typically some angle off of the final spread rate direction since if for instance theta=RAZ (the final spread azimuth of the fire) then the rate of spread at angle theta (TROS) will be equivalent to ROS.
}
\value{
  \code{fbpRaster} returns a RasterStack with primary, secondary, or all output variables, a combination of the primary and secondary outputs. 

  Primary FBP output includes the following 8 raster layers:
  \item{CFB }{Crown Fraction Burned by the head fire}
  \item{CFC }{Crown Fuel Consumption [kg/m^2]}
  \item{FD  }{Fire description (1=Surface, 2=Intermittent, 3=Crown)}
  \item{HFI }{Head Fire Intensity [kW/m]}
  \item{RAZ }{Spread direction azimuth [degrees]}
  \item{ROS }{Equilibrium Head Fire Rate of Spread [m/min]}
  \item{SFC }{Surface Fuel Consumption [kg/m^2]}
  \item{TFC }{Total Fuel Consumption [kg/m^2]}
  
  Secondary FBP System outputs include the following 34 raster layers. In order to calculate the reliable secondary outputs, depending on the outputs, optional inputs may have to be provided.  
  \item{BE }{BUI effect on spread rate}
  \item{SF }{Slope Factor (multiplier for ROS increase upslope)}
  \item{ISI }{Initial Spread Index}
  \item{FFMC }{Fine fuel moisture code [FWI System component]}
  \item{FMC }{Foliar Moisture Content [\%]}
  \item{Do }{Julian Date of minimum FMC}
  \item{RSO }{Critical spread rate for crowning [m/min]}
  \item{CSI }{Critical Surface Intensity for crowning [kW/m]}
  \item{FROS }{Equilibrium Flank Fire Rate of Spread [m/min]}
  \item{BROS }{Equilibrium Back Fire Rate of Spread [m/min]}
  \item{HROSt }{Head Fire Rate of Spread at time hr [m/min]}
  \item{FROSt }{Flank Fire Rate of Spread at time hr [m/min]}
  \item{BROSt }{Back Fire Rate of Spread at time hr [m/min]}
  \item{FCFB }{Flank Fire Crown Fraction Burned}
  \item{BCFB }{Back Fire Crown Fraction Burned}
  \item{FFI }{Equilibrium Spread Flank Fire Intensity [kW/m]}
  \item{BFI }{Equilibrium Spread Back Fire Intensity [kW/m]}
  \item{FTFC }{Flank Fire Total Fuel Consumption [kg/m^2]  }
  \item{BTFC }{Back Fire Total Fuel Consumption [kg/m^2]  }
  \item{DH }{Head Fire Spread Distance after time hr [m] }
  \item{DB }{Back Fire Spread Distance after time hr [m] }
  \item{DF }{Flank Fire Spread Distance after time hr [m] }
  \item{TI }{Time to Crown Fire Initiation [hrs since ignition] }
  \item{FTI }{Time to Flank Fire Crown initiation [hrs since ignition]}
  \item{BTI }{Time to Back Fire Crown initiation [hrs since ignition]}
  \item{LB }{Length to Breadth ratio}
  \item{LBt }{Length to Breadth ratio after elapsed time hr }
  \item{WSV }{Net vectored wind speed [km/hr]}
  \item{TROS* }{Equilibrium Rate of Spread at bearing theta [m/min] }
  \item{TROSt* }{Rate of Spread at bearing theta at time t [m/min] }
  \item{TCFB* }{Crown Fraction Burned at bearing theta }
  \item{TFI* }{Fire Intensity at bearing theta [kW/m] }
  \item{TTFC* }{Total Fuel Consumption at bearing theta [kg/m^2] }
  \item{TTI* }{Time to Crown Fire initiation at bearing theta [hrs since ignition] }

*These outputs represent fire behaviour at a point on the perimeter of an elliptical fire defined by a user input angle theta. theta represents the bearing of a line running between the fire ignition point and a point on the perimeter of the fire. It is important to note that in this formulation the theta is a bearing and does not represent the angle from the semi-major axis (spread direction) of the ellipse. This formulation is similar but not identical to methods presented in Wotton et al (2009) and Tymstra et al (2009).
}
\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan
}

\references{
1.  Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction (FBP) System: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.

2.  Forestry Canada Fire Danger Group. 1992. Development and structure of the Canadian Forest Fire Behavior Prediction System. Forestry Canada, Ottawa, Ontario Information Report ST-X-3. 63 p. \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}

3.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to the 1992 Canadian forest fire behavior prediction system. Nat. Resour. Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, Canada. Information Report GLC-X-10, 45p. \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}

4.  Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development and structure of Prometheus: the Canadian wildland fire growth simulation Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB. Inf. Rep. NOR-X-417.  
}

\seealso{
   \code{\link{fbp}}
}

\examples{
# The dataset is the standard test data for FBP system
# provided by Wotton et al (2009), and randomly assigned
# to a stack of raster layers
test_fbpRastersrc <- system.file("extdata", "test_fbpRaster.tif", package="cffdrs")
test_fbpRaster <- stack(test_fbpRastersrc)
input<-test_fbpRaster
# Stack doesn't hold the raster layer names, we have to assign
# them:
names(input)<-c("FuelType","LAT","LONG","ELV","FFMC","BUI", "WS","WD","GS","Dj","D0","hr","PC",
"PDF","GFL","cc","theta","Accel","Aspect","BUIEff","CBH","CFL","ISI")
# Primary outputs:
system.time(foo<-fbpRaster(input = input))
# Using the "select" option:
system.time(foo<-fbpRaster(input = input,select=c("HFI","TFC", "ROS")))
# Secondary outputs:
system.time(foo<-fbpRaster(input = input,output="S"))
# All outputs:
system.time(foo<-fbpRaster(input = input,output="A"))

# Keep only the required input layers, the other layers would be
# assigned with default values:
# keep only the required inputs:
dat0<-input[[c("FuelType","LAT","LONG","FFMC","BUI","WS","GS", "Dj","Aspect")]]
system.time(foo<-fbpRaster(input = dat0,output="A"))

# Using parallel computation option. This works better only when 
# the input is large. 
# First, create a large input by expanding the input stack:
s<-input
nrow(s)<-1000
ncol(s)<-1000 
s<-resample(input,s,method='ngb')
# Compare whether assign multiple cores would save running time:
system.time(foo1<-fbpRaster(s))
#system.time(foo2<-fbpRaster(s,cores=4))
}
\keyword{ methods }