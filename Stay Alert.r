library(dplyr)
library(ROCR)

# VARIAVEIS DE SETUP ============================================
CLEANUP_P <- TRUE
BALANCE <- TRUE
BAGGING <- TRUE
NORMALIZE <- TRUE
NORM_BY <- "COL" # ROW | COL
MESSAGE <- ""
# ===============================================================

# LENDO DO ARQUIVO
fordTrain <- read.csv("fordTrain.csv",header=T,stringsAsFactors = F)
fordTrain <- arrange(fordTrain,TrialID, ObsNum)
#write.csv(fordTrain,"fordTrain.csv")

# REDIRECIONANDO A SAIDA PARA LOG AUTOMATICO
sink("log.txt", append = T)
"----------------------- START OF EXPERIMENTS -----------------------"
""

Y <- factor(fordTrain$IsAlert)

# Descartando variaveis desnecessárias
fordTrain$X <- NULL
fordTrain$V7 <- NULL
fordTrain$V9 <- NULL
fordTrain$TrialID <- NULL
fordTrain$ObsNum <- NULL
fordTrain$IsAlert <- NULL

# No desafio, a orientação era para evitar o uso de variáveis Px
if(CLEANUP_P){
  MESSAGE <- "P-VARS: No"
  fordTrain$P1 <- NULL
  fordTrain$P2 <- NULL
  fordTrain$P3 <- NULL
  fordTrain$P4 <- NULL
  fordTrain$P5 <- NULL
  fordTrain$P6 <- NULL
  fordTrain$P7 <- NULL
  fordTrain$P8 <- NULL
}else{
  MESSAGE <- "P-VARS: Yes"
}

# TREINAMENTO
SPLIT <- as.integer(nrow(fordTrain)/2)

# inverso das probabilidades originais do dataset
if(BALANCE){
  MESSAGE <- paste(MESSAGE,"BALANCED: Yes",sep=" / ")
  PROB <- ifelse(Y == 1,.31,.69)
}else{
  MESSAGE <- paste(MESSAGE,"BALANCED: No",sep=" / ")
  PROB <- ifelse(Y == 1,.5,.5) # Nao balanceado
}

# Usando a PRIMEIRA METADE pra treinamento
if(BAGGING){
  MESSAGE <- paste(MESSAGE,"BOOTSTRAP: Yes",sep=" / ")
  BOOTSTRAP <- sample(c(1:SPLIT),size = SPLIT, replace = T,prob = PROB[1:SPLIT])
}else{
  MESSAGE <- paste(MESSAGE,"BOOTSTRAP: No",sep=" / ")
  BOOTSTRAP <- c(1:SPLIT)
}
TRAIN <- fordTrain[BOOTSTRAP,]
# Garantindo que nao ha sobreposicao treino-teste
VALIDATION_SET <- setdiff(c(SPLIT:nrow(fordTrain)),BOOTSTRAP)
VALIDATION_SET <- sample(VALIDATION_SET, size = SPLIT, replace = F)
TEST <- fordTrain[VALIDATION_SET,]


# NORMALIZACAO DOS DADOS
if(NORMALIZE){
  if(NORM_BY == "COL"){
    MESSAGE <- paste(MESSAGE,"NORMALIZE: by col",sep=" / ")
    MEANS <- apply(TRAIN,2,mean)
    SD <- apply(TRAIN,2,sd)
    TRAIN <- (TRAIN - MEANS)/SD
    TEST <- (TEST - MEANS)/SD
  }else if(NORM_BY == "ROW"){
    MESSAGE <- paste(MESSAGE,"NORMALIZE: by row",sep=" / ")
    TRAIN <- as.data.frame(t(apply(TRAIN,1,function(x){(x-mean(x))/sd(x)})))
    TEST  <- as.data.frame(t(apply(TEST, 1,function(x){(x-mean(x))/sd(x)})))
  }else{
    MESSAGE <- paste(MESSAGE,"NORMALIZE: No",sep=" / ")
  }
}else{
  MESSAGE <- paste(MESSAGE,"NORMALIZE: No",sep=" / ")
}

TRAIN$IsAlert <- Y[BOOTSTRAP]
TEST$IsAlert <- Y[VALIDATION_SET]

MESSAGE

source("LDA.r")
source("RandomForests.r")
source("LogR.r")

"------------------------ END OF EXPERIMENTS ------------------------"
""

sink(NULL)
gc()