best <- function(state, outcome,arg_best=TRUE)
{

  
  if(outcome!="heart attack"&&outcome!="heart failure"&&outcome!="pneumonia")
  {
    stop("invalid outcome")
  }
  
  myFilename<-"outcome-of-care-measures.csv"
  
  colIdx<-switch(EXPR=outcome,
                 "heart attack"=11,
                 "heart failure"=17,
                 "pneumonia"=23)
  
  dfrm_Data<-myFun_LoadAndPrepareData(myFilename,colIdx)
  
  if(!(state %in% dfrm_Data$State))
  {
    stop("invalid state")
  }
  
  dfrm_Data<-subset(dfrm_Data,dfrm_Data$State==state)
  if(arg_best)
  {
    valMin<-min(dfrm_Data[[colIdx]],na.rm=TRUE)
    dfrm_Data<-subset(dfrm_Data,dfrm_Data[[colIdx]]==valMin)
  }
  else
  {
    valMax<-max(dfrm_Data[[colIdx]],na.rm=TRUE)
    dfrm_Data<-subset(dfrm_Data,dfrm_Data[[colIdx]]==valMax)
  }
  
  ##Sort hospitals by names (to return omly 1 result)
  dfrm_Data<-dfrm_Data[order(dfrm_Data[["Hospital.Name"]]),]
  return(dfrm_Data[1,"Hospital.Name"])
  
}

##******************
myFun_LoadAndPrepareData<-function(arg_filename,arg_NumColumnIdx)
{
  ##Load data
  outcome <- read.csv(arg_filename, colClasses = "character")
  
  ##Transform column values to numetic
  outcome[, arg_NumColumnIdx] <- as.numeric(outcome[, arg_NumColumnIdx])
  
  return(outcome)
}