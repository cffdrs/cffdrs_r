fbp  <- function(input=NULL,output="Primary",m=NULL,cores=1){        
  #hack to avoid Note about no visible binding for global variable ID
  #http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  #look at the globalvariables() option or others in place of this issue
  # do not remove this comment until resolved
  ID <- NULL 
  if (!is.na(charmatch("input", search()))) {
    detach(input)
  }
  if (is.null(input)){
    rel<-.FBPcalc(input)
    rel
  } else {
    if (is.null(m)){m<-ifelse(nrow(input)>500000,3000,1000)}
    m<-ifelse(nrow(input)>=m,m,nrow(input))
    n0<-round(nrow(input)/m)
    n<-ifelse(m*n0>=nrow(input),n0,n0+1)
    if (cores>1){
      cl<-makeCluster(cores)
      registerDoParallel(cl)
      ca<-foreach(i=1:n, .packages='cffdrs') %dopar% {
        if (i==n){
          to.ls<-.FBPcalc(input[((i-1)*m+1):nrow(input),],output=output)
        }else {
          to.ls<-.FBPcalc(input[((i-1)*m+1):(i*m),],output=output)
        }
        to.ls
      }
      stopCluster(cl)
      registerDoSEQ()
    }else {
      ca<-vector('list',n)
      
      for (i in 1:n){
        if (i==n){
          foo<-input[((i-1)*m+1):nrow(input),]
        }else {
          foo<-input[((i-1)*m+1):(i*m),]
        }
        ca[[i]]<-.FBPcalc(foo,output = output)
      }    
    }
    rel<-rbindlist(ca)
    setkey(rel,ID)
    as.data.frame(rel)
  }
}

