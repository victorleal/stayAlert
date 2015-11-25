# ======================================================================
# Normalize.r
# NORMALIZA A ENTRADA POR LINHA OU COLUNA
# ======================================================================

if(NORMALIZE){

  # Normalizacao por coluna
  if(NORM_BY == "COL"){
    cat("Applying normalization by column\n")
    MEANS <- apply(TRAIN,2,mean)
    SD <- apply(TRAIN,2,sd)
    TRAIN <- (TRAIN - MEANS)/SD
    TEST <- (TEST - MEANS)/SD

  # Normalizacao por linha
  }else if(NORM_BY == "ROW"){
    cat("Applying normalization by row\n")
    TRAIN <- as.data.frame(t(apply(TRAIN,1,function(x){(x-mean(x))/sd(x)})))
    TEST  <- as.data.frame(t(apply(TEST, 1,function(x){(x-mean(x))/sd(x)})))
  }
}