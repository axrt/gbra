#' Use to fit and minimize a logistic regeration model given the \code{data} and the \code{part} that will be used to train, \code{1-part} to predict and evaluate the model
#' @param \code{data} a "coocked" data frame where colnames are as follows: X1..Xn, where n is the ids of genomes, rownames are: 1X0..mXn, where m is the largest orf_id
#' @param \code{org1} character name of the genome 1 (eg. 1)
#' @param \code{org2} character name of the genome 2 (eg. 2)
#' @param \code{num.bootstraps} number of bootstraps to run, default is 0, which means "do not run"
#' @param \code{num.cpu} number of cores to use for bootstrap, default is 1, makes sence only if \code{num.bootstraps} >0
#' @param \code{cut} probaility cut that allows to send a point either ot cluster 1 or cluster 2, defualt is 50%/50%, 0.5
#' @param \code{part} of the data to train the model, default is 0.75 (75%), leaving 0.25 to predict and evaluate the model
#' @return a \code{list} train.data: a data.frame the was used to train the model,
#' working.data: a data.frame that was used to evaluate the model, fit: fitted and reduced minimal model itself, mismatch: a table of points that do not match clusters by the
#' perdiction, Rsq: r.squared of the model; in case \code{num.bootstraps}>0, the bootstrapped model also gets returned
#' @examples 
#' bh.data<-read.bhs(bh.folder = "/home/alext/Documents/Research/gBLASTer/bh",sep="_") %>% 
#' as.data.frame() %>%
#' restrict.minimal.hits(minhit = 3) %>%
#' normalize.scores() %>%
#' attach.genomeid.header() %>%
#' sign.bh.table()
#' 
#' logreg.mismatch.genes(data = bh.data, org1 = "3", org2 = "6",part = 0.75)
logreg.mismatch.genes<-function(data, org1, org2, num.bootstraps=0, num.cpu=1, cut=0.5, part=0.75, keep.mod=FALSE, do.step=TRUE){
  if(!require("boot")){
    install.packages("boot")
  }
  #implement checks later
  #select the part that belongs to org1 or org2
  working.table<-data[grepl(pattern = paste("\\b(",org1,"|",org2,")X",sep = "",collapse = ""),x = rownames(data),perl = TRUE),]
  #remove the colums coming from org1 or org2
  working.table<-working.table[,c(-which(colnames(working.table)%in%paste("X",org1,sep="",collapse = "")),-which(colnames(working.table)%in%paste("X",org2,sep="",collapse = "")))]
  #add a new column that indicates if a row comes from org1 (FALSE) or org2 (TRUE)
  working.table$cluster<-grepl(pattern = paste("\\b",org2,"X",sep = "",collapse = ""),x = rownames(working.table),perl = TRUE)
  
  #assemble a formula for the model
  f<-paste("cluster ~",paste(colnames(working.table[1:ncol(working.table)-1]),collapse = "+"))
  #report formula
  with(data = working.table, expr = message(paste("Using formula:",f)))
  
  #randomly select a part of the data
  working.table.sample<-sample.data(data = working.table,part = part)
  #construct model
  glm.fit<-glm(formula = f, data=working.table.sample, family="binomial")
  #report summary
  print(summary(glm.fit))
  if(do.step){
    message("Reducing Model ...")
    #step-reduce the model
    glm.fit<-step(glm.fit)
    #report the reduced model
    print(summary(glm.fit))
  }
  #creat the null fit
  null.fit<-glm(formula = "cluster ~ 1", data = working.table.sample, family="binomial")
  #calculate the R^2 of the model
  r.squared=1-logLik(glm.fit)/logLik(null.fit)
  print(paste("R squared:",r.squared))
  #get the rest of the data
  working.table.rest<-working.table[setdiff(rownames(working.table),rownames(working.table.sample)),]
  if(!keep.mod){
    glm.fit$model<-NULL
    glm.fit$data<-NULL
  }
  if(r.squared==0){
    return(list(train.data=working.table.sample, working.data=working.table.rest,fit=glm.fit, mismatch=data.frame(),Rsq=r.squared))
  }
  if(num.bootstraps>0){
   require(boot)
   message("Bootstrapping model ...")
   glm.boot<-boot(working.table,logreg.model.bootstrap,R=num.bootstraps,formula=glm.fit$formula, parallel = "multicore", ncpus = num.cpu)
   print(glm.boot)
  }
  #predict the rest of the data
  glm.probs<-predict(glm.fit,working.table.rest,type = "response")
  #assign a row to either one, or other cluster
  glm.probs.groups<-sapply(glm.probs,function(i){if(i>=cut){return(TRUE)}else{return(FALSE)}})
  
  #calculate the tp/fp/tn/fn
  glm.table<-table(glm.probs.groups,working.table.rest$cluster)
  
  #report
  message("Contigency table:")
  print(glm.table)

  message(paste("TRUE POSITIVE RATIO:",(glm.table[1,1]+glm.table[2,2])/sum(glm.table)))
  
  cross<-data.frame(cluster=working.table.rest$cluster,probs=glm.probs)
  cross$match<-apply(cross,1,function(i){
    return(i[1]==(i[2]>cut))
  })
  cross.mismatch<-cross[cross$match==FALSE,]
  #get the return list
  out.list<-list(train.data=working.table.sample, working.data=working.table.rest,fit=glm.fit, mismatch=cross.mismatch[,1:2], Rsq=r.squared)
  if(num.bootstraps>0){
    out.list<-c(out.list,boot=glm.boot)
  }
  return(out.list)
}

logreg.model.bootstrap<-function(data, indices, formula){
  working.data<-data[indices,]
  glm.fit<-glm(formula = formula, data=working.data, family=binomial)
  return(glm.fitcoef(glm.fit))
}