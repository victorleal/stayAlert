cat("--------------------------- RANDOM FORESTS ---------------------------",sep="\n\n")

library(randomForest)
model_rf <- randomForest(IsAlert ~ ., data = TRAIN, ntree = 21)
predicted_rf <- predict(model_rf, newdata = TEST)
tbl <- table(pred = predicted_rf,actual = TEST$IsAlert)
print(tbl)
cat(paste("Accuracy on Random Forests:",mean(tbl[1,1]/sum(tbl[,1]),tbl[2,2]/sum(tbl[,2])), sep="\n"))
cat("\n")

# PLOTA ROC E CALCULA AUC
pred <- prediction(as.numeric(predicted_rf), TEST$IsAlert)
perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')

jpeg(paste0("plots/random_forests/", paste0(format(Sys.time(), "%d_%m_%Y_%H_%M"), ".jpg", sep = "")))
plot(perf, col=rainbow(10))
segments(0,0,1,1)
dev.off()

auc <- performance(pred, 'auc')
as.numeric(auc@y.values)