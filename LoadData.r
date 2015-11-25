# ======================================================================
# LoadData.r
# LE OS ARQUIVOS DE ENTRADA
# ======================================================================

# Se READ_CSV, leio do arquivo csv, senao, leio do arquivo Rdata (mais rapido)
if(READ_CSV){
  fordTrain <- read.csv("fordTrain.csv",header=T,stringsAsFactors = F)
  fordTrain <- arrange(fordTrain,TrialID, ObsNum)
  save(fordTrain,file = "fordTrain.Rdata")
  READ_CSV <- FALSE
}else{
  load("fordTrain.Rdata")
}

# Reduz a quantidade de registros por um fator REDUCE_BY
if(REDUCE_BY > 1){
  idx <- sample(fordTrain,replace = T, as.integer(nrow(fordTrain)/REDUCE_BY))
  fordTrain <- fordTrain[idx,]
}

# Armazeno metadados/label separadamente
Y <- factor(fordTrain$IsAlert)
meta <- data.frame(IsAlert = fordTrain$IsAlert,TrialID = fordTrain$TrialID, ObsNum = fordTrain$ObsNum)