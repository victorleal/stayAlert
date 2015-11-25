# ======================================================================
# Taylor.r
# CALCULA TAYLOR EXPANSIONS
# ======================================================================

if(TAYLOR){
  cat("Taylor expanding...\n")

  cols <- ncol(fordTrain)
  last <- cols
  for(i in 1:cols){
    for(j in i:cols){
      fordTrain <- cbind(fordTrain,fordTrain[,i]*fordTrain[,j])
      last <- last + 1
      colnames(fordTrain)[last] <- paste(colnames(fordTrain)[i],colnames(fordTrain)[j],sep="x")
    }
  }
}
