cat("--------------------------- RANDOM FORESTS ---------------------------",sep="\n\n")

library(randomForest)
model_rf <- randomForest(IsAlert ~ ., data = TRAIN, ntree = 20)
predicted_rf <- predict(model_rf, newdata = TEST)
tbl <- table(pred = predicted_rf,actual = TEST$IsAlert)
print(tbl)
cat(paste("Accuracy on Random Forests:",sum(diag(tbl/length(TEST$IsAlert)))),sep="\n")
cat("\n")
# PLOTA ROC E CALCULA AUC
pred <- prediction(as.numeric(predicted_rf), TEST$IsAlert)
perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, col=rainbow(10))
segments(0,0,1,1)

auc <- performance(pred, 'auc')
as.numeric(auc@y.values)