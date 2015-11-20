# TESTANDO COM O CONJUNTO DE TESTES ESPECIFICADO PELO KAGGLE
TEST <- read.csv("fordTest.csv")
TEST$P1 <- NULL
TEST$P2 <- NULL
TEST$P3 <- NULL
TEST$P4 <- NULL
TEST$P5 <- NULL
TEST$P6 <- NULL
TEST$P7 <- NULL
TEST$P8 <- NULL
TEST$V7 <- NULL
TEST$V9 <- NULL

TEST <- (TEST - MEANS)/SD
TEST_Y <- read.csv("Solution.csv")
a <- which(TEST_Y$Indicator=="Public")
predicted_rf <- predict(model_rf, newdata = TEST[a,])
sum(diag(table(predicted_rf,TEST_Y$Prediction[a])))/length(a)
