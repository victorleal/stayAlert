# ======================================================================
# Main.r
# PONTO DE ENTRADA DO PROJETO - RODAR TUDO A PARTIR DAQUI
# ======================================================================
gc(verbose = F)
rm(list=ls())

library(dplyr)
library(ROCR)

# Setup das variaveis de controle ================================================

# Ler do csv ou do rdata?
READ_CSV <- F
# Cross-validation?
CV <- TRUE
# Remover variaveis P-*?
CLEANUP_P <- F
# Vetor com as features permitidas. NA para usar as features originais
FEATURES <-  c("E9","E10","E7","E11","E8","V1","V10","V6","P7","P6","P5","TrialID","ObsNum","IsAlert")
# Balancear?
BALANCE <- T
# Usar sampling com replacement?
BAGGING <- T
# Aplicar taylor expansions? (depois da selecao de atributos)
TAYLOR <- F
# Compor uma linha por varios registros? Quantos? (0 = sem composicao)
LEADS <- 0
# Tamanho dos saltos entre uma medicao e outra
JUMP <- 10
# Fazer amostragem no dataset?
REDUCE_BY <- 1
# Normalizar? Por linha ou coluna?
NORMALIZE <- F
NORM_BY <- "COL" # ROW | COL
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

# Salvando dados pre-processados
save(fordTrain,file = "fordTrain_p.Rdata")

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
  #remove <- which(is.na(TEST$E9)) # descomentar caso faca leads
  #TEST <- TEST[-remove,]
  TEST$IsAlert <- factor(TEST$IsAlert)

  source("RF.r")
  source("LDA.r")
  source("LogR.r")
}
cat("------------------------ END OF EXPERIMENTS ------------------------",sep="\n")
cat("\n")
#sink(NULL)
gc()
