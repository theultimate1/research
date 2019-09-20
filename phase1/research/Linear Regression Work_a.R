#library("randomForest", lib.loc="~/R/win-library/3.4") # Not sure what this was supposed to do. -H.P.

# Install pacman if it isn't installed.
# Note that R version must be ≥ 3.5.0.
#
# You can just download R 3.5.0 from their site if you're using Windows/OSX,
# but must either build from source or download a .DEB file if using Linux.
#
# See https://stackoverflow.com/questions/44567499/install-r-latest-verison-on-ubuntu-16-04
#   and https://cran.r-project.org/bin/linux/ubuntu/
#   and https://askubuntu.com/questions/1031597/r-3-5-0-for-ubuntu
if (!require("pacman")) {
  install.packages("pacman")
  require("pacman")
}

# Our dependencies.
#
# If you can't build or get errors, try installing the R Build Tools.
#
# If it asks if you want to build from source due to no appropriate
# version being available, click `Yes` if you have the build tools.
# 
# If this fails to automatically install and/or compile packages (like on Windows),
# go to  `Tools > Install Packages` and install that way.
pacman::p_load(
  colorspace,
  scales,
  lazyeval,
  plyr,
  rlang,
  tibble,
  stringi,
  survival,
  ggplot2,
  
  reshape2,
  data.table,
  ModelMetrics,
  bindrcpp,
  glue,
  purrr,
  tidyselect,
  dplyr,
  gower,
  prodlim,
  ipred,
  lubridate,
  caret,
  
  zoo,
  multcomp,
  
  pracma,
  stats,
  MASS,
  lars,
  rpart,
  e1071,
  rpart.plot,
  randomForest
)

# Configuring for our directory structure.
data_dir <- normalizePath(file.path("./Data/Data"))

Income_Data_csv <- file.path(data_dir, 'Income_Data_2014.csv')
Census_Data_csv <- file.path(data_dir, 'Census_2010.csv')
Fire_Data_csv <- file.path(data_dir, 'hereyagoben.csv')

income <- na.omit(read.csv(Income_Data_csv)) #Income data from data portal 


census_2010 <- na.omit(read.csv(Census_Data_csv)) #Census data
fire_inc <-na.omit(read.csv(Fire_Data_csv))

#Merge fire data with census data
data<- merge(census_2010,fire_inc, by.x = "GeogKey", by.y = "area_numbe") 
data2 <- merge(data,income,by.x = "GeogKey",by.y = "Community.Area.Number")
data3 <- data2[c(-1,-2,-68,-69,-70,-71,-72,-73,-74,-75,-77)]

#Group ages together
male_under20 <- c(data3$Male..10.to.14.years+data$Male..15.to.17.years+data3$Male..Under.5.years.old+data3$Male..5.to.9.years+data3$Male..18.and.19.years)
male_20_to_55 <-data3$Male..20.years+data3$Male..21.years+data3$Male..22.to.24.years+data3$Male..25.to.29.years+data3$Male..30.to.34.years+data3$Male..35.to.39.years+data3$Male..40.to.44.years+data3$Male..45.to.49.years+data3$Male..50.to.54.years
male_over_55 <-data3$Male..55.to.59.years+data3$Male..60.and.61.years+data3$Male..62.to.64.years+data3$Male..65.and.66.years+data3$Male..67.to.69.years+data3$Male..70.to.74.years+data3$Male..75.to.79.years+data3$Male..80.to.84.years+data3$Male..85.years.and.over

female_under20 <-data3$Female..Under.5.years.old+data3$Female..5.to.9.years+data3$Female..10.to.14.years+data3$Female..15.to.17.years+data3$Female..18.and.19.years
female_20_to_55 <-data3$Female..20.years+data3$Female..21.years+data3$Female..22.to.24.years+data3$Female..25.to.29.years+data3$Female..30.to.34.years+data3$Female..35.to.39.years+data3$Female..40.to.44.years+data3$Female..45.to.49.years+data3$Female..50.to.54.years
female_over_55 <-data3$Female..55.to.59.years+data3$Female..60.and.61.years+data3$Female..62.to.64.years+data3$Female..65.and.66.years+data3$Female..67.to.69.years+data3$Female..70.to.74.years+data3$Female..75.to.79.years+data3$Female..80.to.84.years+data3$Female..85.years.and.over

data3 <- data3[c(-10,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-43,-44,-45,-46,-47,-48,-49,-50,-51,-52,-53,-54,-55)]
data3 <- data.frame(data3,male_under20,male_20_to_55,male_over_55,female_under20,female_20_to_55,female_over_55)

#This separate the training and the test data
trainPct <- 0.8
testPct <- 0.2
set.seed(300)
inTrain <- createDataPartition(y = data3$NUMPOINTS,p = trainPct, list = FALSE)
Train <-data3[inTrain,]
Test <- data3[-inTrain,]

#Training 
emptyTrain <- lm(NUMPOINTS~1,data = Train)
fullTrain <- lm(NUMPOINTS~.,data = Train)
TrainModel <- step(emptyTrain, scope = list(lower = formula(emptyTrain), upper = formula(fullTrain)), direction = "both")

#Testing
#xVars <- colnames(Test)[c(2,3,7,12,15,17,28)]
xVars <- colnames(Test)[c(-20)]
targetVar <- "NUMPOINTS"
Test$PRED <- predict(TrainModel, newdata = Test[,xVars])

plot(TrainModel)

bc<-boxcox(TrainModel,lambda = seq(-3,3,1/10))
lambda <-bc$x[which.max(bc$y)]

#Training 
boxempty <- lm(((NUMPOINTS^lambda-1)/lambda)~1,data = Train)
boxfull <- lm(((NUMPOINTS^lambda-1)/lambda)~.,data = Train)
boxModel <- step(boxempty, scope = list(lower = formula(boxempty), upper = formula(boxfull)), direction = "both")

#Testing
#xVars <- colnames(Test)[c(2,3,7,12,15,17,28)]
xVarsb <- colnames(Test)[c(-20)]
targetVarb <- "NUMPOINTS"
Test$PREDb <- predict(boxModel, newdata = Test[,xVars])^(1/lambda)

#Lasso Regression
Cols <-scale(Train[,-20])
las <-lars(Cols, Train[,20], type = "lasso")
plot(las, plottype = "coefficients")
plot(las,plottype = "Cp")
cvlar <- cv.lars(Cols, Train[,20], type = "lasso")
frac <- cvlar$index[which.min(cvlar$cv)]
lascof <- predict.lars(las, type = "coefficients", mode = "fraction", s = frac)

#Ridge Regression
ridge <- lm.ridge(formula = NUMPOINTS~.,data = Train,lambda = seq(0,100,50))


# #random forest
# df <- data.frame(data3)
# write.csv(df,'data.out.csv')
# data3.rf = randomForest(NUMPOINTS~.,data = df,importance = T, ntree = 6, maxnodes = 5, keep.forest = TRUE)
# 
# importance(data3.rf,type = 1)
# plot(data3.rf)
# varImpPlot(data3.rf)
# 
# #Create a model (Chicago Fire Index)
# rev_Dat <- data3[c(3,19,29,31,8,15,25,28,30,22,23,26,18,21,20)]
# myModel <- lm(NUMPOINTS~.,data = rev_Dat)
# summary(myModel)
# myAOV <- aov(myModel)
# summary(myAOV)
# plot(myAOV)
# emptyModel <- lm(NUMPOINTS~1,data = data3)
# fullModel <-lm(NUMPOINTS~.,data = data3)
# ModelA <- step(emptyModel, scope = list(lower = formula(emptyModel), upper = formula(fullModel)), direction = "both")
# 
# #Test model
# data3a <- data3[1:40]
