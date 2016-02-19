fbpRaster<-function(input,output = "Primary",select=NULL,m=NULL,cores=1){
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  if (is.null(m)){m<-ifelse(ncell(input)>500000,3000,1000)}
  al<-c("CFB","CFC","FD","HFI","RAZ","ROS","SFC","TFC","BE","SF","ISI","FFMC","FMC","D0",
        "RSO","CSI","FROS","BROS","HROSt","FROSt","BROSt","FCFB","BCFB","FFI","BFI",
        "FTFC","BTFC","TI","FTI","BTI","LB","LBt","WSV","DH","DB","DF","TROS","TROSt",
        "TCFB","TFI","TTFC","TTI")
  prim<-al[1:8]
  scd<-al[9:length(al)]
  if (!is.null(select)){
    select<-toupper(select)
    select<-select[!duplicated(select)]
    if(output == "SECONDARY" | output == "S"){
      if (!sort(select %in% scd)[1]){stop("Selected variables are not in the outputs")}
    }
    if (output == "PRIMARY" | output == "P"){
      if (!sort(select %in% prim)[1]){stop("Selected variables are not in the outputs")} 
    }
    if (output == "ALL" | output == "A"){
      if (!sort(select %in% al)[1]){stop("Selected variables are not in the outputs")} 
    }
  }
  names(input)<-toupper(names(input))
  output<-toupper(output)
  if("LAT" %in% names(input)){
    registerDoSEQ()
    r<-getValuesBlock_stackfix(input,nrows=nrow(input))
    r<-as.data.frame(r)
    names(r)<-names(input)
  }else{
    r<-as.data.frame(rasterToPoints(input))
    names(r)[names(r)=="y"]<-"LAT"
    if (max(r$LAT)>90|min(r$LAT)< -90){
      warning("Input projection is not in lat/long, consider re-projection or include LAT as input")
    }
  }
  r$ID<-1:nrow(r)
  
  fuelCross<-data.frame(FUELTYPE0=sort(c(paste("C",1:7,sep="-"),"D-1",paste("M",1:4,sep="-"),
                                    paste("S",1:3,sep="-"),"O-1a","O-1b","WA","NF")),code=1:19)
  r<-merge(r,fuelCross,by.x="FUELTYPE",by.y="code",all.x=TRUE,all.y=FALSE)
  
  r$FUELTYPE<-NULL
  names(r)[names(r)=="FUELTYPE0"] <- "FUELTYPE"
  r<-r[with(r,order(ID)),]
  names(r)[names(r)=="x"]<-"LONG"
  FBP<-fbp(r,output=output,m=m,cores=cores)
  if (!(output == "SECONDARY" | output == "S")){
    FBP$FD<-ifelse(FBP$FD=="I",2,FBP$FD)
    FBP$FD<-ifelse(FBP$FD=="C",3,FBP$FD)
    FBP$FD<-ifelse(FBP$FD=="S",1,FBP$FD)
    FBP$FD<-as.numeric(FBP$FD)
  }
  
  if (!is.null(select)){
    out<-out0<-input[[1]]
    values(out)<-FBP[,select[1]]
    if (length(select)>1){
      for (i in 2:length(select)){
        values(out0)<-FBP[,select[i]]
        out<-stack(out,out0)
      }
    }
    names(out)<-select
    out
  }else
    if (output == "PRIMARY" | output == "P") {
      message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
      out<-out0<-input[[1]]
      values(out)<-FBP[,prim[1]]
      for (i in 2:length(prim)){
        values(out0)<-FBP[,prim[i]]
        out<-stack(out,out0)
      }
      names(out)<-prim
      out
    }else
      if(output == "SECONDARY" | output == "S") {
        out<-out0<-input[[1]]
        values(out)<-FBP[,scd[1]]
        for (i in 2:length(scd)){
          values(out0)<-FBP[,scd[i]]
          out<-stack(out,out0)
        }
        names(out)<-scd
        out
      } else
        if(output == "ALL" | output == "A") {
          message("FD = 1,2,3 representing Surface (S),Intermittent (I), and Crown (C) fire")
          out<-out0<-input[[1]]
          values(out)<-FBP[,al[1]]
          for (i in 2:length(al)){
            values(out0)<-FBP[,al[i]]
            out<-stack(out,out0)
          }
          names(out)<-al
          out
        }   
}
