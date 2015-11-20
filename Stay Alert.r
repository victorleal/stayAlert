library(dplyr)
library(ROCR)

# LENDO DO ARQUIVO
fordTrain <- read.csv("fordTrain.csv",header=T)
fordTrain <- arrange(fordTrain,TrialID, ObsNum)
#write.csv(fordTrain,"fordTrain.csv")

Y <- factor(fordTrain$IsAlert)

# Descartando variaveis desnecessárias
fordTrain$X <- NULL
fordTrain$TrialID <- NULL
fordTrain$ObsNum <- NULL

# No desafio, a orientação era para evitar o uso de variáveis Px
fordTrain$P1 <- NULL
fordTrain$P2 <- NULL
fordTrain$P3 <- NULL
fordTrain$P4 <- NULL
fordTrain$P5 <- NULL
fordTrain$P6 <- NULL
fordTrain$P7 <- NULL
fordTrain$P8 <- NULL
fordTrain$V7 <- NULL
fordTrain$V9 <- NULL
fordTrain$IsAlert <- NULL

# TREINAMENTO
SPLIT <- as.integer(nrow(fordTrain)/2)
# inverso das probabilidades originais do dataset
PROB <- ifelse(Y == 1,.31,.69)

# Usando a PRIMEIRA METADE pra treinamento
BOOTSTRAP <- sample(c(1:SPLIT),size = SPLIT, replace = T,prob = PROB[1:SPLIT])
TRAIN <- fordTrain[BOOTSTRAP,]

# NORMALIZACAO DOS DADOS
MEANS <- apply(TRAIN,2,mean)
SD <- apply(TRAIN,2,sd)
TRAIN <- (TRAIN - MEANS)/SD
#TRAIN <- apply(TRAIN,1,function(x){(x-mean(x))/sd(x)})
TRAIN$IsAlert <- Y[BOOTSTRAP]

# TESTE
VALIDATION_SET <- setdiff(c(SPLIT:nrow(fordTrain)),BOOTSTRAP)
VALIDATION_SET <- sample(VALIDATION_SET, size = SPLIT, replace = F)
TEST <- fordTrain[VALIDATION_SET,]
TEST <- (TEST - MEANS)/SD
TEST$IsAlert <- Y[VALIDATION_SET]

source("LDA.r")
source("RandomForests.r")
#source("LogR.r")
