
rankhospital <- function(state, outcome, num = "best") {

  source("best.R")
  
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
  
  ##Check state
  if(!(state %in% dfrm_Data$State))
  {
    stop("invalid state")
  }
  
  ##check rank vs quantity
  ##if rank>quantity return NA
  DataSubset<-myFun_GetSubsetByGroupName(dfrm_Data,"State",state)
  num_quantityOfCases<-length(DataSubset[["State"]])
  
  if(is.numeric(num)&&(num>num_quantityOfCases))
  {
    return("NA")
  }
  
  ##if num is "best" - return best
  if(!is.numeric(num)&&(num=="best"))
  {
    return(best(state,outcome,TRUE))
  }
  
  ##if num is "worst" - return worst
  if(!is.numeric(num)&&(num=="worst"))
  {
    return(best(state,outcome,FALSE))
  }
  
  ##Sort hospitals by names (to deal with ties)
  DataSubset<-DataSubset[order(DataSubset[[colIdx]],DataSubset[["Hospital.Name"]]),]
  DataSubset<-subset(DataSubset,subset=(!is.na(DataSubset[[colIdx]])))
  
  if(is.numeric(num))
  {
    return(DataSubset[num,"Hospital.Name"])
  }
  
}

##******************
##Function
##will load and prepare data.
myFun_LoadAndPrepareData<-function(arg_filename,arg_NumColumnIdx)
{
  ##Load data
  outcome <- read.csv(arg_filename, colClasses = "character")
  
  ##Transform column values to numetic
  outcome[, arg_NumColumnIdx] <- as.numeric(outcome[, arg_NumColumnIdx])
  
  return(outcome)
}

##Function
##will return subset of original data.
myFun_GetSubsetByGroupName<-function(arg_DataFrame,arg_GroupColumnName,arg_GroupName)
{
  ##build a contingency table with counts per group
  ##(group = "State" in this case; counts is num of hospitals per state)
  tbl_quantitiesPerGroup<-table(arg_DataFrame[[arg_GroupColumnName]])
  
  ##make data frame from a contingency table
  dfrm_quantitiesPerGroup<-data.frame(tbl_quantitiesPerGroup)
  
  
  ##Get subset of original data:
  ##Only data from groups which specified name
  DataSubset<-subset(arg_DataFrame,arg_DataFrame[[arg_GroupColumnName]]==arg_GroupName)
  
  return(DataSubset)
}