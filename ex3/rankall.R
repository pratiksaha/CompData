
rankall <- function(outcome, num = "best") {

  
  source("rankhospital.R")
  
  ##Check outcome
  if(outcome!="heart attack"&&outcome!="heart failure"&&outcome!="pneumonia")
  {
    stop("invalid outcome")
  }
  
  ##Get column position
  colIdx<-switch(EXPR=outcome,
                 "heart attack"=11,
                 "heart failure"=17,
                 "pneumonia"=23)
  
  ##Load File and prepare data
  myFilename<-"outcome-of-care-measures.csv"
  dfrm_Data<-myFun_LoadAndPrepareData(myFilename,colIdx)
  
  ##Get vector of state names:
  vec_StatesNames<-myFun_GetListGroupNames(dfrm_Data,"State")
  
  vec_HospNames<-sapply(vec_StatesNames,rankhospital,outcome=outcome,num=num)
  
  dfrm_HospForState<-data.frame(hospital=vec_HospNames,state=vec_StatesNames)
  
  return(dfrm_HospForState)
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


##Function
##will return vector of group names
myFun_GetListGroupNames<-function(arg_Data,arg_GroupColumnName)
{
  ##build a contingency table with counts per group
  ##(group = "State" in this case; counts is num of hospitals per state)
  tbl_quantitiesPerGroup<-table(arg_Data[[arg_GroupColumnName]])
  
  ##make data frame from a contingency table
  dfrm_quantitiesPerGroup<-data.frame(tbl_quantitiesPerGroup)
  
  ##return 1st column with group names
  return(dfrm_quantitiesPerGroup[[1]])
  
}