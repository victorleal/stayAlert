gc(verbose = F)
rm(list=ls())

library(dplyr)
library(ROCR)

# SETUP =========================================================
READ_CSV <- F
CV <- TRUE

CLEANUP_P <- T
FEATURES <-  c("E9","E10","E7","E11","E8","V1","V10","V6","TrialID","ObsNum","IsAlert")
BALANCE <- T
BAGGING <- T
TAYLOR <- F

LEADS <- 00
JUMP <- 50
REDUCE_BY <- 1

NORMALIZE <- F
NORM_BY <- "COL" # ROW | COL
MESSAGE <- ""
# ===============================================================

# REDIRECIONANDO A SAIDA PARA LOG AUTOMATICO
#sink("log.txt", append = T)
cat(format(Sys.time(), "%a, %d %b %Y - %X"))
cat("\n\n")
cat("----------------------- START OF EXPERIMENTS -----------------------")
cat("\n\n")

source("LoadData.r")
source("FeatureSelection.r")
source("Taylor.r")
source("TimeSeries.r")
save(fordTrain,file = "fordTrain_p.Rdata")

# TREINAMENTO
SPLIT <- as.integer(nrow(fordTrain)*.5)

for(i in 1:2){
#i <- 1
  if(!CV && i==2) break

  source("CrossValidation.r")
  source("Balance.r")
  source("Bootstrap.r")
  source("Normalize.r")
  TRAIN$IsAlert <- Y[BOOTSTRAP]
  TEST$IsAlert <- Y[VALIDATION_SET]

  rm(fordTrain)
  #remove <- which(is.na(TRAIN$E9))
  #TRAIN <- TRAIN[-remove,]
  TRAIN$IsAlert <- as.factor(TRAIN$IsAlert)
  #remove <- which(is.na(TEST$E9))
  #TEST <- TEST[-remove,]
  TEST$IsAlert <- factor(TEST$IsAlert)

  source("RF.r")
  source("LDA.r")
  #source("LogR.r")
}
cat("------------------------ END OF EXPERIMENTS ------------------------",sep="\n")
cat("\n")
#sink(NULL)
gc()
