logreg.mismatch.genes<-function(data, org1, org2, num.bootstraps=0, num.cpu=1, cut=0.5){
  
  #implement checks later
  working.table<-data[grepl(pattern = paste("\\b[",org1,",",org2,"]X",sep = "",collapse = ""),x = rownames(data),perl = TRUE),]
  working.table<-working.table[,c(-which(colnames(working.table)%in%paste("X",org1,sep="",collapse = "")),-which(colnames(working.table)%in%paste("X",org2,sep="",collapse = "")))]
  working.table$cluster<-grepl(pattern = paste("\\b[",org2,"]X",sep = "",collapse = ""),x = rownames(working.table),perl = TRUE)
  
  f<-paste("cluster ~",paste(colnames(working.table[1:ncol(working.table)-1]),collapse = "+"))
  with(data = working.table, expr = message(paste("Using formula:",f)))
  glm.fit<-glm(formula = f, data=working.table, family=binomial)
  print(summary(glm.fit))
  message("Reducing Model ...")
  glm.fit<-step(glm.fit)
  print(summary(glm.fit))
  if(num.bootstraps>0){
   require(boot)
   message("Bootstrapping model ...")
   glm.boot<-boot(working.table,logreg.model.bootstrap,R=num.bootstraps,formula=glm.fit$formula, parallel = "multicore", ncpus = num.cpu)
   print(glm.boot)
  }
  
  glm.probs<-predict(glm.fit,type = "response")
  
  glm.probs.groups<-sapply(glm.probs,function(i){if(i>=cut){return(TRUE)}else{return(FALSE)}})
  glm.table<-table(glm.probs.groups,working.table$cluster)
  message("Contigency table:")
  print(glm.table)
  message(paste("TRUE POSITIVE RATIO:",(glm.table[1,1]+glm.table[2,2])/sum(glm.table)))
  
  cross<-data.frame(cluster=working.table$cluster,probs=glm.probs)
  cross$match<-apply(cross,1,function(i){
    return(i[1]==(i[2]>cut))
  })
  cross.mismatch<-cross[cross$match==FALSE,]
  return(cross.mismatch[,1:2])
}

logreg.model.bootstrap<-function(data, indices, formula){
  working.data<-data[indices,]
  glm.fit<-glm(formula = formula, data=working.data, family=binomial)
  return(coef(glm.fit))
}