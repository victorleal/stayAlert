library(MASS)
library(dplyr)

fordTrain <- read.csv(file.choose(),header=T)
fordTrain <- arrange(fordTrain,TrialID, ObsNum)

Y <- factor(fordTrain$IsAlert)
fordTrain$X <- NULL
fordTrain$TrialID <- NULL
fordTrain$ObsNum <- NULL
fordTrain$P8 <- NULL
fordTrain$V7 <- NULL
fordTrain$V9 <- NULL
fordTrain$IsAlert <- NULL

# TREINAMENTO
SPLIT <- as.integer(nrow(fordTrain)/2)
# inverso das probabilidades originais do dataset
PROB <- ifelse(Y == 1,.42,.58)
BOOTSTRAP <- sample(c(1:nrow(fordTrain)),size = SPLIT, replace = T,prob = PROB)
TRAIN <- fordTrain[BOOTSTRAP,]

# NORMALIZACAO
MEANS <- apply(TRAIN,2,mean)
SD <- apply(TRAIN,2,sd)
#TRAIN <- (TRAIN - MEANS)/SD
TRAIN$IsAlert <- NULL
#TRAIN <- apply(TRAIN,1,function(x){(x-mean(x))/sd(x)})
TRAIN$IsAlert <- Y[BOOTSTRAP]

model <- lda(IsAlert~., data=TRAIN)

# TESTE
BOOTSTRAP_VAL <- which(!c(1:nrow(fordTrain)) %in% BOOTSTRAP)
BOOTSTRAP_VAL <- sample(BOOTSTRAP_VAL, size = SPLIT, replace = T)
TEST <- fordTrain[BOOTSTRAP_VAL,]
TEST$IsAlert <- Y[BOOTSTRAP_VAL]

predicted <- predict(model,newdata = TEST,type="response")$class
sum(diag(table(predicted, TEST$IsAlert[BOOTSTRAP_VAL])/length(BOOTSTRAP_VAL)))

