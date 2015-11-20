model_glm <- glm(IsAlert~.,data = TRAIN, family = binomial())
predicted_glm <- predict(model_glm, newdata = TEST,type="response")
sum(diag(table(predicted = factor(ifelse(predicted_glm>.5,1,0)), actual = TEST$IsAlert[VALIDATION_SET])/length(VALIDATION_SET)))
