## over wintering DC
# DCf: Final Fall DC Value
#  rw: winter precipitation (mm)
#   a: user-selected value accounting for carry-over fraction
#   b: user-selected value accounting for wetting efficiency fraction
wDC<-function(DCf=100,rw=200,a=0.75,b=0.75){
  Qf<-800*exp(-1*DCf/400) #EQ 3
  Qs <- a*Qf + b*(3.94*rw) #EQ 2
  DCs <- 400*log(800/Qs) #EQ 4
  DCs <- ifelse(DCs<15,15,DCs)
  DCs
}

