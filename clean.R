# ------------------------
# explore data structure
# - classes and formatting
# - missingness
# ------------------------
# identify variable classes
vclass=sapply(tdat,class)
table(vclass)
# make sure TCODE and ZIP are qualitative by appending a characters
tdat$tcode=paste0("tc",tdat$tcode)
tdat$zip=paste0("z",tdat$zip)
# make sure blank character cells are recoded as NAs
tdat.fctrs=as.matrix(tdat[,vclass=="factor"]) # as.matrix to conver to character so I can examine blanks
tdat.fctrs=gsub(" ","",tdat.fctrs) # homogenize missing values by removing spaces
tdat.fctrs[tdat.fctrs==""]=NA # replace blanks with proper NAs
tdat[,vclass=="factor"]=tdat.fctrs
# format dates
date.vars=c("dob",colnames(tdat)[grepl("date",colnames(tdat))])
for (dvar in date.vars) {
  ds=tdat[,dvar]
  ds=paste0("19",substr(ds,1,2),"-",substr(ds,3,4),"-01")
  ds[grepl("NA",ds)]=NA
  ds=ymd(ds)
  # code as days since Jan 1 1900, since presumably time is the relevant value
  ds=as.numeric(difftime(ds,ymd("1900-01-01"),units="days"))
  tdat[,dvar]=ds
}
# distribution of variables with missing data
# good resource by Gelman on missingness: http://www.stat.columbia.edu/~gelman/arm/missing.pdf
cc.rate=sapply(tdat,function(x) mean(complete.cases(x)))
hist(cc.rate,main="distribution of percent missing cells per variable")
# proportion of variables with missing data
mean(cc.rate!=1)
# which variables have misssing data?
vars.miss=names(cc.rate)[cc.rate!=1]
# create training data set excluding variables with missing data in case imputation becomes a burden
tdat.cc=tdat[,names(cc.rate)[cc.rate==1]]
mean(complete.cases(tdat.cc)) # verify this is a complete data set
# identify different patterns of missingness
tdat.mdp=md.pattern(tdat)
nrow(tdat.mdp) # number of different patterns of missingness
# examine missigness pattern frequencies (fs)
mdp.fs=as.numeric(rownames(tdat.mdp))
mdp.fs=mdp.fs[!is.na(mdp.fs)]
mdp.fs.reps=mdp.fs[mdp.fs!=1] # patterns with frequency 1 are most likely random
quantile(mdp.fs.reps,seq(0,1,by=.01)) # how many patterns are repeated frequenly?
# seem to be a few recurrent patterns but are these statistically anomalous?
# i.e., are the data missing completely at random?
nvars.miss=sum(cc.rate!=1) # how many variables have missing data?
nmiss.dist=choose(nvars.miss,0:nvars.miss) # number of possible missingness patterns per number of variables missing
nmiss.dist=nmiss.dist/sum(nmiss.dist) # convert to probability distribution
plot(nmiss.dist,type="l") # plot it
row.nmiss=apply(tdat,1,function(x) sum(is.na(x))) # number of rows in tdat with N missing cells
row.nmiss.sum=rep(0,nvars.miss+1)
row.nmiss.tab=table(row.nmiss) # number of actual missingness patterns per number of variables missing
row.nmiss.sum[as.numeric(names(row.nmiss.tab))]=row.nmiss.tab # use above to populate the distribution
row.nmiss.sum=row.nmiss.sum/sum(row.nmiss.sum) # convert to probability distribution
# definitely not missing completely at random
lines(row.nmiss.sum,col='red') # plot this on top of 'random' missingness distribution

# --------------------------------------------
# imputation
# - start with 
# - examine variable distributions
# - transformations (log-scaling, normalize)
# --------------------------------------------
# tried to do MICE (multiple imputation by chained equations) but way too slow for high-dimensional data
# tdat.mice=mice(tdat[1:10,])
# DIY imputation with iterative lasso regression (didn't take this to completion)
# 1st do random imputation
# the code from Gelman's article will do
random.imp=function(a) {
  missing=is.na(a)
  n.missing=sum(missing)
  a.obs=a[!missing]
  imputed=a
  imputed[missing]=sample(a.obs,n.missing,replace=TRUE)
  return(imputed)
}
# start with random imputation (bootstrap, essentially)
tdat.noimp=tdat
for (vm in vars.miss) {
  tdat[,vm]=random.imp(tdat[,vm])
}
mean(complete.cases(tdat)) # verify that it worked
# scale and/or normalize the numeric variables prior to imputation regressions
# extract numeric variables as a separate data frame for easier operation
vclass=sapply(tdat,class)
num.vars=grepl("numeric|integer",vclass)
tdat.num=tdat[,num.vars]
tdat=tdat[,!num.vars]
# scale and/or normalize variables when appropriate
# to identify the right transformation I wanted to do some distribution fitting 
#   but fitdist was acting up for some variables so I skipped this
# distrs=rep(NA,ncol(tdat.num))
# for (i in 1:ncol(tdat.num)) {
#   x=tdat.num[,i]
#   fit.unif=fitdist(x,"unif")
#   fit.exp=fitdist(x,"exp")
#   fit.norm=fitdist(x,"norm")
#   d=c("exp","norm")[which.min(c(fit.exp$aic,fit.norm$aic))]
#   distrs[i]=d
# }
# a simpler heuristic was to log-scale variables that span 3+ orders of magnitude
# check if there are any negatives (there are none, thankfully)
sum(apply(tdat.num,2,function(x) sum(x<0)))
# loop through variables, convert 0s to .1s and then log transform
#   ideally I'd check whether there is further log scaling among decimals 
#   and choose the 0 replacement more intelligently but not going to mess with this now
for (i in 1:ncol(tdat.num)) {
  x=tdat.num[,i]
  x[x==0]=0.1
  x.log=log10(x)
  if (length(table(floor(x.log)))>2) {
    if (sum(is.infinite(x.log))>0) {
      cat(colnames(tdat.num)[i]," still has inf\n")
    } else {
      tdat.num[,i]=x.log
    }
  }
}
# normalize everything to ensure similar scales
# remove any columns with zero variance before scaling
vars.0sd=colnames(tdat.num)[apply(tdat.num,2,sd)==0]
tdat.num=tdat.num[,!(colnames(tdat.num)%in%vars.0sd)]
tdat.num.scaled=scale(tdat.num)
sum(is.na(tdat.num.scaled)) # make sure there are no NAs
# how many outliers are there?
mean(abs(tdat.num.scaled)>4) # very few outliers
# add back in the numeric variables to tdat
tdat=cbind(tdat,tdat.num.scaled)
# 2nd use lasso regression to iteratively impute missing values for all variables
#   started (see below) but no time for this, must push forward, must get predictions!
# vars.miss=vars.miss[order(runif(length(vars.miss)))]
# for (vm in vars.miss) {
#   #   take the randomly imputed data set excluding rows with value of NA for var.miss.i in the origainl data
#   ldat=filter(tdat,!is.na(tdat.orig[,vm]))
#   vm.cv=cv.glmnet(ldat[,colnames(ldat)!=vm],ldat[,vm],alpha=1)
#   vm.fit=with(ldat,glmnet())
# }
