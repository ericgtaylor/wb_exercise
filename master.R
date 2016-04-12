# load required packages
library(dplyr)
library(fitdistrplus)
library(mice)
library(glmnet)
library(lubridate)
library(randomForest)
# define project directory (code) and file directory (source data)
project.dir="/Users/erictayor/wb_exercise/"
file.dir="/Users/erictayor/Desktop/Wayblazer ml-exercise/"
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
do.target_d=T
source(paste0(project.dir,"clean.R"))
tdat.clean=tdat
tdat.target_d.mean=target_d.mean
tdat.target_d.sd=target_d.sd
# rename the validation data "tdat" so I can reuse the clean.R code (this is ugly I know)
tdat=vdat
do.target_d=F
source(paste0(project.dir,"clean.R"))
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
# put into one data set for final round of prep so factor levels are identical across train and test
#   otherwise randomForest will say "New factor levels not present in the training data"
colnames(tdat)[!(colnames(tdat)%in%colnames(vdat))]
tvdat=rbind(cbind(tdat,set="training"),
            cbind(vdat,set="validation",target_b="DK",target_d="DK"))
# make sure we have a complete data set
mean(complete.cases(tvdat))
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
tvdat=prep.for.tree(tvdat)
tvdat.prepped=tvdat # took a while to do this so save
# first have to remove categorical variables with more than 53 categories
cat.vars=sapply(tvdat,class)
cat.vars.nlevels=sapply(tvdat[,cat.vars=="factor"],function(x) length(unique(x)))
sort(cat.vars.nlevels)
# rfa_x (60-122 levels): remove the first bite, recency, to reduce the number of unique codes
rfas=paste0("rfa_",2:24)
tvdat[,rfas]=substr(as.matrix(tvdat[,rfas]),2,10)
# tcode (67 levels): remove tcode levels with frequency 1
tcodes=table(tvdat$tcode)
tcodes.rm=rownames(tcodes[tcodes==1])
tvdat=filter(tvdat,!(tcode%in%tcodes.rm))
tvdat$tcode=as.factor(as.character(tvdat$tcode))
# state (59 levels): would re-code by region but to move quickly, remove 7 least frequent
states=table(tvdat$state)
tvdat=filter(tvdat,!(state%in%names(sort(states))[1:7]))
tvdat$state=as.factor(as.character(tvdat$state))
# osource (895 levels): just remove it... no time to worry about this
tvdat=select(tvdat,-osource)
# zip (~20k levels): just remove it since you already have state and there's way too many
tvdat=select(tvdat,-zip)

# remove controln variable
controln=tvdat$controln
tvdat$controln=NULL

# do PCA on numeric variables to speed up the model
vclass=sapply(tvdat,class)
tvdat.nums=tvdat[,vclass=="numeric"]
pca.out=princomp(tvdat.nums)
plot(pca.out$sdev)
plot(pca.out$sdev[1:50]) # keep 1-7 (would typically do a more thoughful selection)
tvdat.nums.pca=pca.out$scores[,1:7]
tvdat=cbind(tvdat[,vclass!="numeric"],tvdat.nums.pca)

# split training and testing data
tvdat=prep.for.tree(tvdat)
tvdat.prepped=tvdat
# save(tvdat,file="/Users/erictayor/wb_exercise/tvdat.R")
tdat=filter(tvdat,set=="training")
vdat=filter(tvdat,set=="validation")
tr.prop=.5
train.indxs=sample(1:nrow(tdat),round(nrow(tdat)*tr.prop))
rm.vars=-match(c("target_b"),colnames(tdat))
# reformat as numeric now that tdat is separated from vdat
tdat$target_d=as.numeric(as.character(tdat$target_d))
tdat.train=tdat[train.indxs,rm.vars]
tdat.train=sample_n(tdat.train,10000)
tdat.test=tdat[-train.indxs,rm.vars]
# do the random forest
# learned a while back not to call the formula (see tip on the list below)
# http://stats.stackexchange.com/questions/37370/random-forest-computing-time-in-r
tdat.train.IVs=tdat.train[,!(colnames(tdat.train)%in%c("target_d"))]
tdat.rf=randomForest(tdat.train.IVs,
                     tdat.train$target_d,importance=T,ntree=500,do.trace=T)
tdat.rf.pred=predict(tdat.rf,select(tdat.test,-target_d))
# look at the predictions against actuals
#   the raw data scatterplot looks pretty awful
plot(tdat.rf.pred,select(tdat.test,target_d)[,1],xlim=c(0,10),ylim=c(0,10))
#   but if you aggregate the predicted values into bins there is clearly signal
plot.df=data.frame(pred=tdat.rf.pred,act=select(tdat.test,target_d)[,1])
plot.df$pred=cut(plot.df$pred,
                 quantile(plot.df$pred,probs=seq(0,1,length.out=11)),1:10)
with(plot.df,bargraph.CI(pred,act,
                         xlab="predicted values (scaled) quantiles",ylab="actual values (scaled)"))

tdat.rf.val=predict(tdat.rf,select(vdat,-target_d))
tdat.rf.val=10^(tdat.rf.val*tdat.target_d.sd+tdat.target_d.mean)
hist(tdat.rf.val)

tdat.rf.val=data.frame(pred=tdat.rf.val,
                       controln=controln[tvdat$set=="validation"])



