# Descartando variaveis desnecessárias
fordTrain$X <- NULL
fordTrain$V7 <- NULL
fordTrain$V9 <- NULL
fordTrain$P8 <- NULL
fordTrain$E7 <- NULL
fordTrain$IsAlert <- NULL
fordTrain$TrialID <- NULL
fordTrain$ObsNum <- NULL

# No desafio, a orientação era para evitar o uso de variáveis Px
if(CLEANUP_P){
  cat("Removing P-* variables...\n")
  remove <- grep(pattern = "P[0-9]+",x = colnames(fordTrain),ignore.case = T)
  fordTrain <- fordTrain[,-remove]
}

if(!is.na(FEATURES)){
  cat("Selecting features...\n")
  fordTrain <- fordTrain[,intersect(FEATURES,colnames(fordTrain))]
}