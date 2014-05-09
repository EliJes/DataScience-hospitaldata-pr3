
rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  koko_outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  osa_outcome=data.frame(
    Hospital = koko_outcome[,c(2)],
    State = koko_outcome[,c(7)],
    Heart_attack=as.numeric(koko_outcome[,c(11)]),
    Heart_failure=as.numeric(koko_outcome[,c(17)]),
    Pneumonia=as.numeric(koko_outcome[,c(23)]))
  
  if (!state %in% osa_outcome[,2]) {stop('invalid state')}
  if (!outcome %in% c("heart attack","heart failure","pneumonia"))
  {stop('invalid outcome')}
  
  #valitaan osavaltio
  osa_outcome = osa_outcome[state==osa_outcome[,2],]
  
  if (outcome == "heart attack"){
    #järjestetään 
  osa_outcome = osa_outcome[order(osa_outcome$Heart_attack, osa_outcome$Hospital), ]
    #liitetään rank
  osa_outcome$rank = ave(osa_outcome$Heart_attack, osa_outcome$State, FUN=order)
    #poistetaan NA:t
  osa_outcome = osa_outcome[!is.na(osa_outcome[, 3]), ]
  }
  
  else if (outcome == "heart failure"){
    #järjestetään 
    osa_outcome = osa_outcome[order(osa_outcome$Heart_failure, osa_outcome$Hospital), ] 
    #liitetään rank
    osa_outcome$rank = ave(osa_outcome$Heart_failure, osa_outcome$State, FUN=order)
    #poistetaan NA:t
    osa_outcome = osa_outcome[!is.na(osa_outcome[, 4]), ]
  } 
   else {
      #järjestetään 
      osa_outcome = osa_outcome[order(osa_outcome$Pneumonia, osa_outcome$Hospital), ] 
      #liitetään rank
      osa_outcome$rank = ave(osa_outcome$Pneumonia, osa_outcome$State, FUN=order)
      #poistetaan NA:t
      osa_outcome = osa_outcome[!is.na(osa_outcome[, 5]), ]
    }
  if(num >=1 && num <=nrow(osa_outcome)){
    hospital = as.character(osa_outcome[num==osa_outcome$rank, 1] )
  }
  else if (num=="worst"){
    hospital = as.character(osa_outcome[max(osa_outcome$rank), 1] )
  }   
  else {hospital=return (NA)
  }
hospital
}