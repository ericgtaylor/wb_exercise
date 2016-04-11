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
cat.vars.nlevels=sapply(tdat[,cat.vars=="factor"],function(x) length(unique(x)))
sort(cat.vars.nlevels)
# rfa_x (60-122 levels): remove the first bite, recency, to reduce the number of unique codes
rfas=paste0("rfa_",2:24)
tdat[,rfas]=substr(as.matrix(tdat[,rfas]),2,10)
# tcode (55 levels): remove a random 2 tcode levels with frequency 1
tcodes=table(tdat$tcode)
# do this once and save them so you can remove from vdat as well
# tcodes.rm=sample(rownames(tcodes[tcodes==1]),3)
tcodes.rm=c("tc76","tc24002","tc27")
tdat=filter(tdat,!(tcode%in%tcodes.rm))
tdat$tcode=as.factor(as.character(tdat$tcode))
# state (57 levels): would re-code by region but to move quickly, remove 4 least frequent
states=table(tdat$state)
tdat=filter(tdat,!(state%in%names(sort(states))[1:5]))
tdat$state=as.factor(as.character(tdat$state))
# osource (895 levels): just remove it... no time to worry about this
tdat=select(tdat,-osource)
# zip (~20k levels): just remove it since you already have state and there's way too many
tdat=select(tdat,-zip)

tr.prop=.75
train.indxs=sample(1:nrow(tdat),round(nrow(tdat)*tr.prop))
rm.vars=-match(c("controln","target_b"),colnames(tdat))
tdat.train=tdat[train.indxs,rm.vars]
tdat.test=tdat[-train.indxs,rm.vars]

cat.vars=sapply(tdat.train,class)
cat.vars.nlevels=sapply(tdat.train[,cat.vars=="factor"],function(x) length(unique(x)))
sort(cat.vars.nlevels)

# do the random forest
# learned a while back not to call the formula (see tip on the list below)
# http://stats.stackexchange.com/questions/37370/random-forest-computing-time-in-r
tdat.train.IVs=tdat.train[,!(colnames(tdat.train)%in%c("target_d"))]
tdat.train.IVs=prep.for.tree(tdat.train.IVs)
tdat.rf=randomForest(tdat.train.IVs,
                     tdat.train$target_d,importance=T,ntree=10,do.trace=T)
tdat.rf.pred=predict(tdat.rf,select(tdat.test,-churned),type="prob")
tdat.rf.pred.range=predict(tdat.rf,select(tdat.test,-churned),type="response")
tdat.rf.pred.YN=data.frame(yes=1-tdat.rf.pred[,"no_churn"],
                           no=tdat.rf.pred[,"no_churn"])
