library(dplyr)
library(MASS)
library(caret)
library(randomForest)
library(ROCR)

# LENDO DO ARQUIVO
fordTrain <- read.csv("fordTrain.csv",header=T)
fordTrain <- arrange(fordTrain,TrialID, ObsNum)
#write.csv(fordTrain,"fordTrain.csv")

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


# COMECE A RODAR AQUI ================================================================
#for(i in 1:10){
BOOTSTRAP <- sample(c(1:nrow(fordTrain)),size = SPLIT, replace = T,prob = PROB)
TRAIN <- fordTrain[BOOTSTRAP,]

#MEANS <- apply(TRAIN,2,mean)
#SD <- apply(TRAIN,2,sd)
#TRAIN <- (TRAIN - MEANS)/SD
#TRAIN <- apply(TRAIN,1,function(x){(x-mean(x))/sd(x)})
TRAIN$IsAlert <- Y[BOOTSTRAP]

#model_glm <- glm(IsAlert~.,data = TRAIN, family = binomial())
#model_lda <- lda(IsAlert ~ ., data = TRAIN)
model_rf <- randomForest(IsAlert ~ ., data = TRAIN, ntree = 10)

# TESTE
VALIDATION_SET <- which(!c(1:nrow(fordTrain)) %in% BOOTSTRAP)
VALIDATION_SET <- sample(VALIDATION_SET, size = SPLIT, replace = F)
TEST <- fordTrain[VALIDATION_SET,]
TEST$IsAlert <- Y[VALIDATION_SET]

#predicted_glm <- predict(model_glm, newdata = TEST,type="response")
#predicted_lda <- predict(model_lda, newdata = TEST)
predicted_rf <- predict(model_rf, newdata = TEST)

#sum(diag(table(predicted = factor(ifelse(predicted_glm>.5,1,0)), actual = TEST$IsAlert[VALIDATION_SET])/length(VALIDATION_SET)))
#sum(diag(table(predicted=predicted_lda$class,actual=TEST$IsAlert)))/length(VALIDATION_SET)
print(paste("Random Forests accuracy:",sum(diag(table(predicted_rf,TEST$IsAlert)/length(TEST$IsAlert)))))
#}

# PLOTA ROC E CALCULA AUC
pred <- prediction(as.numeric(predicted_rf), TEST$IsAlert)
perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, col=rainbow(10))
segments(0,0,1,1)

auc <- performance(pred, 'auc')
as.numeric(auc@y.values)
