gc(verbose = F)
rm(list=ls())

library(dplyr)
library(ROCR)

# SETUP =========================================================
READ_CSV <- FALSE
CV <- TRUE

CLEANUP_P <- FALSE
FEATURES <- NA#c("V11","P7","V1","P6","E10","V10","E6","V6","E9","TrialID","ObsNum","IsAlert")
BALANCE <- TRUE
BAGGING <- TRUE
TAYLOR <- F

LEADS <- 5
JUMP <- 5
REDUCE_BY <- 1

NORMALIZE <- TRUE
NORM_BY <- "ROW" # ROW | COL
MESSAGE <- ""
# ===============================================================

# REDIRECIONANDO A SAIDA PARA LOG AUTOMATICO
#sink("log.txt", append = T)
cat(format(Sys.time(), "%a, %d %b %Y - %X"))
cat("\n\n")
cat("----------------------- START OF EXPERIMENTS -----------------------")
cat("\n\n")


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
  MESSAGE <- "P-VARS: No"
  remove <- grep(pattern = "P[0-9]+",x = colnames(fordTrain),ignore.case = T)
  fordTrain <- fordTrain[,-remove]
}else{
  MESSAGE <- "P-VARS: Yes"
}

if(!is.na(FEATURES)){
  MESSAGE <- paste(MESSAGE,"FEATURE SELECTION: Yes",sep=" / ")
  fordTrain <- fordTrain[,intersect(FEATURES,colnames(fordTrain))]
}else{
  MESSAGE <- paste(MESSAGE,"FEATURE SELECTION: No",sep=" / ")
}

# TAYLOR EXPANSIONS ================================================
if(TAYLOR){
  MESSAGE <- c(MESSAGE,"TAYLOR: Yes",sep=" / ")

  cols <- ncol(fordTrain)
  last <- cols
  for(i in 1:cols){
    for(j in i:cols){
      fordTrain <- cbind(fordTrain,fordTrain[,i]*fordTrain[,j])
      last <- last + 1
      colnames(fordTrain)[last] <- paste(colnames(fordTrain)[i],colnames(fordTrain)[j],sep="x")
    }
  }

}else{
  MESSAGE <- c(MESSAGE,"TAYLOR: No",sep=" / ")
}

# CRIANDO VETORES DE TIME SERIES ====================================
if(LEADS > 0){
  fordTrain <- cbind(fordTrain,meta)
  tmp_names <- colnames(fordTrain)
  MESSAGE <- paste(MESSAGE, paste("LEADS:",LEADS),sep=" / ")
  MESSAGE <- paste(MESSAGE, paste("JUMP:",JUMP),sep=" / ")
  tmp_fordTrain <- fordTrain[seq(1,nrow(fordTrain),JUMP),]

  for(i in 1:LEADS){
    tmp_df <- as.data.frame(apply(fordTrain[,tmp_names],2,function(x)lead(x,JUMP*i)))
    colnames(tmp_df) <- paste(tmp_names,i,sep="_")
    tmp_df <- tmp_df[seq(1,nrow(tmp_df),JUMP),]
    tmp_fordTrain <- cbind(tmp_fordTrain,tmp_df)
    cat(paste("Lead:",i,"\n"))
  }
  fordTrain <- tmp_fordTrain
  rm(tmp_df,tmp_fordTrain)

  a <- colnames(fordTrain)
  fordTrain <- fordTrain[which(fordTrain$TrialID == fordTrain$TrialID_1),]
  remove <- grep(pattern = "(Trial.+)|(Obs.+)|(IsAlert.+)",x = colnames(fordTrain),ignore.case = T)
  fordTrain <- fordTrain[,-remove]
  # Removendo NA's
  fordTrain <- fordTrain[apply(fordTrain,1,function(x){if(is.na(sum(x))){FALSE}else{TRUE}}),]
}

save(fordTrain,file = "fordTrain_p.Rdata")

# TREINAMENTO
SPLIT <- as.integer(nrow(fordTrain)*.5)

for(i in 1:2){
  if(!CV && i==2) break

  load("fordTrain_p.Rdata")
  fordTrain$E5xE5 <- NULL
  fordTrain$E11xV8 <- NULL
  # CROSS VALIDATION
  if(i==1){
    cat("TRAIN A-TEST B",sep="\n")
    TRAIN_RANGE <- c(1:SPLIT)
    TEST_RANGE <- c(SPLIT:nrow(fordTrain))
  }else{
    cat("TRAIN B-TEST A",sep="\n")
    TEST_RANGE <- c(1:SPLIT)
    TRAIN_RANGE <- c(SPLIT:nrow(fordTrain))
  }

  # BALANCEANDO O DATASET =====================================

  if(BALANCE){
    MESSAGE <- paste(MESSAGE,"BALANCED: Yes",sep=" / ")
    p0 <- table(Y[TRAIN_RANGE])[1]/length(TRAIN_RANGE)
    p1 <- table(Y[TRAIN_RANGE])[2]/length(TRAIN_RANGE)
    PROB <- ifelse(Y == 1,p0,p1)
  }else{
    MESSAGE <- paste(MESSAGE,"BALANCED: No",sep=" / ")
    PROB <- ifelse(Y == 1,.5,.5) # Nao balanceado
  }

  # BOOTSTRAP ================================================

  if(BAGGING){
    MESSAGE <- paste(MESSAGE,"BOOTSTRAP: Yes",sep=" / ")
    BOOTSTRAP <- sample(TRAIN_RANGE,size = SPLIT, replace = T,prob = PROB[TRAIN_RANGE])
  }else{
    MESSAGE <- paste(MESSAGE,"BOOTSTRAP: No",sep=" / ")
    BOOTSTRAP <- c(TRAIN_RANGE)
  }

  TRAIN <- fordTrain[BOOTSTRAP,]
  gc()

  # EVITANDO SOBREPOSICAO TREINO-TESTE ====================================

  VALIDATION_SET <- setdiff(c(TEST_RANGE),BOOTSTRAP)
  VALIDATION_SET <- sample(VALIDATION_SET, size = as.integer(SPLIT/3*4), replace = T)
  TEST <- fordTrain[VALIDATION_SET,]


  # NORMALIZACAO ==========================================================

  if(NORMALIZE){

    # POR COLUNA ===============================================

    if(NORM_BY == "COL"){
      MESSAGE <- paste(MESSAGE,"NORMALIZE: by col",sep=" / ")
      MEANS <- apply(TRAIN,2,mean)
      SD <- apply(TRAIN,2,sd)
      TRAIN <- (TRAIN - MEANS)/SD
      TEST <- (TEST - MEANS)/SD

    # POR LINHA (IPSATIVE) =====================================

    }else if(NORM_BY == "ROW"){
      MESSAGE <- paste(MESSAGE,"NORMALIZE: by row",sep=" / ")
      TRAIN <- as.data.frame(t(apply(TRAIN,1,function(x){(x-mean(x))/sd(x)})))
      TEST  <- as.data.frame(t(apply(TEST, 1,function(x){(x-mean(x))/sd(x)})))

    }else{
      # INVALIDO
      MESSAGE <- paste(MESSAGE,"NORMALIZE: No",sep=" / ")
    }

  # SEM NORMALIZACAO =======================================

  }else{
    MESSAGE <- paste(MESSAGE,"NORMALIZE: No",sep=" / ")
  }

  TRAIN$IsAlert <- Y[BOOTSTRAP]
  TEST$IsAlert <- Y[VALIDATION_SET]

  if(i==1) cat(MESSAGE,sep="\n")

  rm(fordTrain)

  # TREINAMENTO DE CADA METODO ============================


  source("RF.r")
  source("LDA.r")
  source("LogR.r")
}
cat("------------------------ END OF EXPERIMENTS ------------------------",sep="\n")
cat("\n")
#sink(NULL)
gc()
