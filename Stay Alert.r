library(dplyr)
library(ROCR)

# VARIAVEIS DE SETUP ============================================
CLEANUP_P <- FALSE
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
cat("----------------------- START OF EXPERIMENTS -----------------------")
cat("\n\n")

Y <- factor(fordTrain$IsAlert)

# Descartando variaveis desnecessárias
fordTrain$X <- NULL
fordTrain$V7 <- NULL
fordTrain$V9 <- NULL
fordTrain$P8 <- NULL
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
}else{
  MESSAGE <- "P-VARS: Yes"
}

# TREINAMENTO
SPLIT <- as.integer(nrow(fordTrain)/2)

for(i in 1:2){
  if(i==1){
    cat("TRAIN A-TEST B",sep="\n")
    TRAIN_RANGE <- c(1:SPLIT)
    TEST_RANGE <- c(SPLIT:nrow(fordTrain))
  }else{
    cat("TRAIN B-TEST A",sep="\n")
    TEST_RANGE <- c(1:SPLIT)
    TRAIN_RANGE <- c(SPLIT:nrow(fordTrain))
  }
  # Inverso das probabilidades originais do dataset
  if(BALANCE){
    MESSAGE <- paste(MESSAGE,"BALANCED: Yes",sep=" / ")
    p0 <- table(Y[TRAIN_RANGE])[1]/length(TRAIN_RANGE)
    p1 <- table(Y[TRAIN_RANGE])[2]/length(TRAIN_RANGE)
    PROB <- ifelse(Y == 1,p0,p1)
  }else{
    MESSAGE <- paste(MESSAGE,"BALANCED: No",sep=" / ")
    PROB <- ifelse(Y == 1,.5,.5) # Nao balanceado
  }

  # Usando a PRIMEIRA METADE pra treinamento
  if(BAGGING){
    MESSAGE <- paste(MESSAGE,"BOOTSTRAP: Yes",sep=" / ")
    BOOTSTRAP <- sample(TRAIN_RANGE,size = SPLIT, replace = T,prob = PROB[TRAIN_RANGE])
  }else{
    MESSAGE <- paste(MESSAGE,"BOOTSTRAP: No",sep=" / ")
    BOOTSTRAP <- c(TRAIN_RANGE)
  }
  TRAIN <- fordTrain[BOOTSTRAP,]
  # Garantindo que nao ha sobreposicao treino-teste
  VALIDATION_SET <- setdiff(c(TEST_RANGE),BOOTSTRAP)
  VALIDATION_SET <- sample(VALIDATION_SET, size = as.integer(SPLIT/3*4), replace = T)
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

  if(i==1) cat(MESSAGE,sep="\n")

  source("LDA.r")
  source("RandomForests.r")
  source("LogR.r")
}
cat("------------------------ END OF EXPERIMENTS ------------------------",sep="\n")
cat("\n")
sink(NULL)
gc()
