cat("------------------------------- LOG-R --------------------------------",sep="\n\n")

model_glm <- glm(IsAlert~.,data = TRAIN, family = binomial())
predicted_glm <- predict(model_glm, newdata = TEST,type="response")
tbl <- table(pred = as.factor(ifelse(predicted_glm>.5,1,0)), actual = TEST$IsAlert)
print(tbl)
cat(paste("Accuracy on LogR:",mean(tbl[1,1]/sum(tbl[,1]),tbl[2,2]/sum(tbl[,2])), sep="\n"))
cat("\n")
