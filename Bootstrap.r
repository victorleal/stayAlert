# ======================================================================
# Bootstrap.r
# APLICAR BOOTSTRAP NO CONJUNTO DE TREINAMENTO
# ======================================================================

SPLIT <- as.integer(nrow(fordTrain)*.5)
if(BAGGING){
  cat("Applying bootstrap...\n")
  BOOTSTRAP <- sample(TRAIN_RANGE,size = SPLIT, replace = T,prob = PROB[TRAIN_RANGE])
}else{
  BOOTSTRAP <- TRAIN_RANGE
}

TRAIN <- fordTrain[BOOTSTRAP,]
gc()

# EVITANDO SOBREPOSICAO TREINO-TESTE ====================================

VALIDATION_SET <- setdiff(c(TEST_RANGE),BOOTSTRAP)
VALIDATION_SET <- sample(VALIDATION_SET, size = as.integer(SPLIT/3*4), replace = T)
TEST <- fordTrain[VALIDATION_SET,]