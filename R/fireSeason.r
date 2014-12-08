fireSeason <- function(input, fs.start=12,fs.end=5,method="WF93"){
  names(input) <- tolower(names(input))
  if ("yr" %in% names(input)) {
    yr <- input$yr
  } else {
    stop("Year was not provided, year is required for this function.")
  }
  if ("mon" %in% names(input)) {
    mon <- input$mon
    if(mon<1 || mon>12){
      stop("mon value is out of bounds (1-12), stopping execution.")
    }
  } else {
    stop("Month was not provided, month is required for this function.")
  }
  if ("day" %in% names(input)) {
    day <- input$day
    if(day < 1 || day > 31){
      stop("day value is out of bounds (1-31), stopping execution.")      
    }
  } else {
    stop("day was not provided, day is required for this function, stopping execution..")
  }
  
  if ("tmax" %in% names(input)) {
    tmax <- input$tmax
  } else {
    stop("Maximum Daily Temperature (tmax) was not provided, daily tmax is required for this function.")
  }
  method <- tolower(method)
  if(method != "wf93"){
    stop(paste("Selected method \"", method, "\" is unavailable, read documentation for available methods.",sep=""))
  }
  
 
  n0<-length(tmax)
  seasonActive <- FALSE
  seasonStartEnd <- NULL
  
  if(method == "wf93"){
    for (k in 1:n0){
      if (k>3){
        if(!seasonActive & (all(tmax[seq(k-3,k-1,1)]>fs.start))){
          seasonActive <- TRUE #set season to active
          seasonStartEnd <- rbind(seasonStartEnd,data.frame(yr=yr[k],mon=mon[k],day=day[k],fsdatetype="start"))
        }
        if(seasonActive & all(tmax[seq(k-3,k-1,1)]<fs.end)){
          seasonActive <- FALSE
          seasonStartEnd <- rbind(seasonStartEnd,data.frame(yr=yr[k],mon=mon[k],day=day[k],fsdatetype="end"))
        }
      }
    }
  }
  seasonStartEnd
}
