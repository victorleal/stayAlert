if(BALANCE){
  cat("Balancing dataset...\n")
  p0 <- table(Y[TRAIN_RANGE])[1]/length(TRAIN_RANGE)
  p1 <- table(Y[TRAIN_RANGE])[2]/length(TRAIN_RANGE)
  PROB <- ifelse(Y == 1,p0,p1)
}else{
  PROB <- ifelse(Y == 1,.5,.5) # Nao balanceado
}