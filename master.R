# load required packages
library(dplyr)
library(fitdistrplus)
library(mice)
library(glmnet)
library(lubridate)
library(randomForest)
# define project directory (code) and file directory (source data)
project.dir="/Users/eric8226/Desktop/ml-exercise/"
file.dir=paste0(project.dir,"raw data/")
# list the files with and without the paths
fs=list.files(file.dir)
fs.full=list.files(file.dir,full.names=T)
# read files with txt and cup
# (probably overkill... but good to demo for cases where there are lots to load)
for (i in 1:length(fs)) {
  if (sum(sapply(c("txt","cup"),regexpr,fs[i])>0)>0) {
    assign(gsub("\\.txt","",fs[i]),read.csv(fs.full[i],skipNul=TRUE))
  }
}
# give shorter names to data sets
tdat=cup98LRN # t=training
tdat.orig=tdat # and record original
vdat=cup98VAL # v=validation
vdat.orig=vdat # and record original
# change all variable names to lowercase for easier typing
colnames(tdat)=tolower(colnames(tdat))
colnames(vdat)=tolower(colnames(vdat))

# ----------
# cleaning
# ----------
# typically I'd write functions to do all this, so I could run the cleaning
#   separately on tdat and vdat, but I saved the code in a file as a hack
source("/Users/eric8226/Desktop/ml-exercise/clean.R")
tdat.clean=tdat
# rename the validation data "tdat" so I can reuse the clean.R code (this is ugly I know)
tdat=vdat
source("/Users/eric8226/Desktop/ml-exercise/clean.R")
# give back tdat and vdat their proper names
vdat=tdat
tdat=tdat.clean

# ----------
# modeling
# ----------
# typically I'd try a few different frameworks and use the best
#   but I'm just going to use a random forest
# save versions of tdat and vdat
tdat.clean=tdat
vdat.clean=vdat
# make sure we have a complete data set
mean(complete.cases(tdat))
# randomforest has column formatting requirements
prep.for.tree=function(x) {
  # make all numeric or factor
  for (i in 1:ncol(x)) {
    # cat(i,"/",ncol(x),"\n")
    all.i=unique(as.numeric(as.character(x[,i])))
    if (sum(is.na(all.i)>0)) {
      x[,i]=as.factor(as.character(x[,i]))
    } else {
      x[,i]=as.numeric(as.character(x[,i]))
    }
  }
  return(x)
}
tdat=prep.for.tree(tdat)
tdat.prepped=tdat # took a while to do this so save
# first have to remove categorical variables with more than 53 categories
cat.vars=sapply(tdat,class)
cat.vars.nlevels=sapply(tdat,function(x) )

tr.prop=.75
train.indxs=sample(1:nrow(tdat),round(nrow(tdat)*tr.prop))
rm.vars=-match(c("controln","target_b"),colnames(tdat))
tdat.train=tdat[train.indxs,rm.vars]
tdat.test=tdat[-train.indxs,rm.vars]
# do the random forest
# learned a while back not to call the formula (see tip on the list below)
# http://stats.stackexchange.com/questions/37370/random-forest-computing-time-in-r
tdat.rf=randomForest(tdat.train[,colnames(tdat.train)!="target_d"],
                     tdat.train$target_d,importance=T,ntree=10,do.trace=T)
tdat.rf.pred=predict(tdat.rf,select(tdat.test,-churned),type="prob")
tdat.rf.pred.range=predict(tdat.rf,select(tdat.test,-churned),type="response")
tdat.rf.pred.YN=data.frame(yes=1-tdat.rf.pred[,"no_churn"],
                           no=tdat.rf.pred[,"no_churn"])


# don't forget to exclue the DVs from the IV feature set
