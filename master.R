# load required packages
library(readr)
library(fitdistrplus)
library(mice)
# define project directory (code) and file directory (source data)
project.dir="/Users/erictayor/wb_exercise/"
file.dir="/Users/erictayor/Desktop/Wayblazer ml-exercise/"
# list the files with and without the paths
fs=list.files(file.dir)
fs.full=list.files(file.dir,full.names=T)
# read files with txt and cup
for (i in 1:length(fs)) {
  if (sum(sapply(c("txt","cup"),regexpr,fs[i])>0)>0) {
    assign(gsub("\\.txt","",fs[i]),read.csv(fs.full[i],skipNul=TRUE))
  }
}
# give shorter names to data sets
tdat=cup98LRN # t=training
vdat=cup98VAL # v=validation

# ------------------------
# explore data structure
# - classes
# - missingness
# - scales
# - distributions
# ------------------------
# identify variable classes
vclass=sapply(tdat,class)
table(vclass)

# distribution of variables with missing data
# good resource by Gelman on missingness: http://www.stat.columbia.edu/~gelman/arm/missing.pdf
cc.rate=sapply(tdat,function(x) mean(complete.cases(x)))
hist(cc.rate)
# proportion of variables with missing data
mean(cc.rate!=1)
# which variables have misssing data?
names(cc.rate)[cc.rate!=1]
# for factors with missing data assign the level "_NA_" to missing cells
# (never mind ... none of the factors have missing data)
intersect(names(cc.rate)[cc.rate!=1],colnames(tdat)[vclass=="factor"])
table(sapply(tdat[,names(cc.rate)[cc.rate!=1]],class))
# create training data set excluding variables with no missing data
# (may not end up using this if I feel good about imputation rules)
tdat.cc=tdat[,names(cc.rate)[cc.rate==1]]
mean(complete.cases(tdat.cc)) # verify this is a complete data set
# examine distributions
# good forum on R packages for identifying distributions: http://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
descdist(x,discrete=FALSE)

