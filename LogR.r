"------------------------------- LOG-R --------------------------------"

model_glm <- glm(IsAlert~.,data = TRAIN, family = binomial())
predicted_glm <- predict(model_glm, newdata = TEST,type="response")
print(paste("Accuracy on LogR:",sum(diag(table(predicted = as.factor(ifelse(predicted_glm>.5,1,0)), actual = TEST$IsAlert)/length(VALIDATION_SET)))))
