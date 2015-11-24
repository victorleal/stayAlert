load("fordTrain_p.Rdata")
fordTrain$E5xE5 <- NULL
fordTrain$E11xV8 <- NULL
users <- unique(meta$TrialID)
# CROSS VALIDATION
if(i==1){
  cat("TRAIN A-TEST B",sep="\n")
  TRAIN_RANGE <- which(meta$TrialID %in% sample(users, size = length(users)/2, replace = F))#c(1:SPLIT)
  TEST_RANGE <- setdiff(c(1:nrow(fordTrain)),TRAIN_RANGE) #c(SPLIT:nrow(fordTrain))
}else{
  cat("TRAIN B-TEST A",sep="\n")
  prev <- TEST_RANGE
  TEST_RANGE <- TRAIN_RANGE#c(1:SPLIT)
  TRAIN_RANGE <- prev #c(SPLIT:nrow(fordTrain))
  rm(prev)
}