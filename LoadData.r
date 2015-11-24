# LENDO DO ARQUIVO
if(READ_CSV){
  fordTrain <- read.csv("fordTrain.csv",header=T,stringsAsFactors = F)
  fordTrain <- arrange(fordTrain,TrialID, ObsNum)
  save(fordTrain,file = "fordTrain.Rdata")
  READ_CSV <- FALSE
}else{
  load("fordTrain.Rdata")
}

if(REDUCE_BY > 1){
  idx <- sample(fordTrain,replace = T, as.integer(nrow(fordTrain)/REDUCE_BY))
  fordTrain <- fordTrain[idx,]
}

Y <- factor(fordTrain$IsAlert)
meta <- data.frame(IsAlert = fordTrain$IsAlert,TrialID = fordTrain$TrialID, ObsNum = fordTrain$ObsNum)
