# ======================================================================
# CrossValidation.r
# GERA CONJUNTOS DE TREINO-TESTE PARA CADA ITERACAO DA VALIDACAO CRUZADA
# ======================================================================

# Carregando os dados pre-processados
load("fordTrain_p.Rdata")

# Eliminando variaveis inuteis
fordTrain$E5xE5 <- NULL
fordTrain$E11xV8 <- NULL

# Gerando conjuntos para cross-validation
# i == 1: gero um conjunto de usuarios para treino e uso o complementar para testes
# i == 2: inverto os conjuntos anteriores
if(i == 1){
  users <- unique(meta$TrialID)
  cat("TRAIN A-TEST B",sep="\n")
  TRAIN_RANGE <- which(meta$TrialID %in% sample(users, size = length(users)/2, replace = F))
  TEST_RANGE <- setdiff(c(1:nrow(fordTrain)),TRAIN_RANGE)
}else{
  cat("TRAIN B-TEST A",sep="\n")
  prev <- TEST_RANGE
  TEST_RANGE <- TRAIN_RANGE
  TRAIN_RANGE <- prev
  rm(prev)
}