rankall <- function(outcome , num = "best") {
  
  ## Read outcome data
  df=read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  df=data.frame(df[,c(2,7)], sapply(df[,c(11,17,23)], as.numeric))
  
  ## Check that outcome is valid
  if (!outcome %in% c("heart attack","heart failure","pneumonia"))
  {stop('invalid outcome')}
    
  if(outcome=="heart attack"){
    df2=na.omit(df[,c(1,2,3)])
  }
  else if(outcome=="heart failure"){
    df2=na.omit(df[,c(1,2,4)])
  }
  else {df2=na.omit(df[,c(1,2,5)])}
   

  ## For each state, find the hospital of the given rank:
  df5=c()
    for (i in unique(df2$State)){
    
    #niiltä riveiltä, joilla State=i otetaan kaikki sarakkeet
    df3=df2[df2[,2]==i,]
    
    #järjestetään mortality raten ja sairaalan nimen mukaan
    df3=df3[order(df3[,3], df3[,1]),]
    
    #tietyn rankin (rivinro = num) omaavan sairaalan valinta
    #osavaltiosta i (antaa arvon "<NA>, jos ko sijoituksella
    #olevaa sairaalaa ei löydy)
    if(num=="best"){
      df4=data.frame(df3[1,1],i)
    }
    else if(num=="worst"){
      df4=data.frame(df3[nrow(df3),1],i)
    }
    else if(num > nrow(df3)){
    df4=c("NA", i)
    }
    else {
    df4=data.frame(df3[num,1],i)
    }
    df5=rbind(df5,df4)
    
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  names(df5)=c("hospital", "state")
  df5
}
  