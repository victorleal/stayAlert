if(LEADS > 0){
  fordTrain <- cbind(fordTrain,meta)
  tmp_names <- colnames(fordTrain)
  cat(paste("LEADS:",LEADS,"\n"))
  cat(paste("JUMP:",JUMP,"\n"))
  tmp_fordTrain <- fordTrain[seq(1,nrow(fordTrain),JUMP),]

  for(i in 1:LEADS){
    tmp_df <- as.data.frame(apply(fordTrain[,tmp_names],2,function(x)lead(x,JUMP*i)))
    colnames(tmp_df) <- paste(tmp_names,i,sep="_")
    tmp_df <- tmp_df[seq(1,nrow(tmp_df),JUMP),]
    tmp_fordTrain <- cbind(tmp_fordTrain,tmp_df)
    #cat(paste("Lead:",i,"\n"))
  }
  fordTrain <- tmp_fordTrain
  rm(tmp_df,tmp_fordTrain)

  a <- colnames(fordTrain)
  fordTrain <- fordTrain[which(fordTrain$TrialID == fordTrain$TrialID_1 &fordTrain$IsAlert == fordTrain$IsAlert_1),]
  meta <- data.frame(IsAlert = fordTrain$IsAlert,TrialID = fordTrain$TrialID, ObsNum = fordTrain$ObsNum)

  remove <- grep(pattern = "(Trial.+)|(Obs.+)|(IsAlert.+)",x = colnames(fordTrain),ignore.case = T)
  fordTrain <- fordTrain[,-remove]
  # Removendo NA's
  remove <- apply(fordTrain,1,function(x){if(is.na(sum(x))){FALSE}else{TRUE}})
  fordTrain <- fordTrain[-remove,]
  meta <- meta[-remove,]
  Y <- fordTrain$IsAlert
}