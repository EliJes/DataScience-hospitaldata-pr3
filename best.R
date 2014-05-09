
best <- function(state, outcome) {
  
  koko_outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  osa_outcome=data.frame(
    Hospital = koko_outcome[,c(2)],
    State = koko_outcome[,c(7)],
    Heart_attack=as.numeric(koko_outcome[,c(11)]),
    Heart_failure=as.numeric(koko_outcome[,c(17)]),
    Pneumonia=as.numeric(koko_outcome[,c(23)]))
  
  
  if(!outcome %in% c("heart attack","heart failure","pneumonia"))
    {stop('invalid outcome')}
    
  if(!state %in% osa_outcome$State)
    {stop('invalid state')}
  
  #valitaan osavaltion sairaalat
  osa_outcome = osa_outcome[osa_outcome$State == state, ]
  #järjestetään sairaalan nimen mukaan
  osa_outcome = osa_outcome[order(osa_outcome[, 1]), ]
  
  if (outcome == 'heart attack'){
  #valitaan pienimmän arvon omaava sairaala
  paras = osa_outcome[which.min(osa_outcome[, 3]), "Hospital"]
  
} else if (outcome == 'heart failure'){
  #valitaan pienimmän arvon omaava sairaala
  paras = osa_outcome[which.min(osa_outcome[, 4]), "Hospital"]
  
} else {
  #valitaan pienimmän arvon omaava sairaala
  paras = osa_outcome[which.min(osa_outcome[, 5]), "Hospital"]
}
 as.character(paras) 
}
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate



