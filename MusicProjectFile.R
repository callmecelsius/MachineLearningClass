library(keras)
library(e1071)
library(LiblineaR)
library(class)

library(readr)
Music_Dataset <- na.omit(read_csv("INSERT FILE PATH HERE", 
                                  col_types = cols(date = col_date(format = "%m/%d/%Y"))))
View(Music_Dataset)

attach(Music_Dataset)

set.seed(1)


#--------------------------------------------------------------------------------------
#             Analyzing 2000-2018              @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#--------------------------------------------------------------------------------------
mean.weeks.total <- mean(Music_Dataset$weeks,na.rm = TRUE)
median.weeks.total <- median(Music_Dataset$weeks,na.rm = TRUE)
sd.weeks.total<- sd(Music_Dataset$weeks)
Music_Dataset$weeksAbove <- ifelse(Music_Dataset$weeks > mean.weeks.total, 1, 0) #converting to Classifiers
Music_Dataset$weeksYN <- ifelse(Music_Dataset$weeks > mean.weeks.total,"yes","no") #converting to Classifiers


# pairs(Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness","instrumentalness"
#                        , "danceability","duration_ms","loudness","valence")], pch=19,
#       col = (Music_Dataset$weeksAbove+2), upper.panel = NULL, cex = .5)
# 
# plot(Music_Dataset$energy, Music_Dataset$loudness, col = (Music_Dataset$weeksAbove+2), pch = 20, cex = 1)

summary(Music_Dataset$weeksYN)
table(Music_Dataset$weeksYN)


#----------------------------------------------------------------------------------------
# KNN
#----------------------------------------------------------------------------------------
attach(Music_Dataset)

#This part I am not sure why in the lab we used -86

train.X <- Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                            "instrumentalness","danceability","duration_ms","loudness","valence")]

train.Y <- Music_Dataset$weeksAbove

test.X <- Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                           "instrumentalness","danceability","duration_ms","loudness","valence")]

test.Y <- Music_Dataset$weeksAbove


train.X.scaled <- scale(Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                                         "instrumentalness","danceability","duration_ms","loudness","valence")])

test.X.scaled <- scale(Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                                        "instrumentalness","danceability","duration_ms","loudness","valence")])


for (K in c(1,3,5,10,100))
{
  set.seed(1)
  knn.pred <- knn(train=train.X,
                  test=test.X,
                  cl=train.Y,
                  k=K)
  
  head(knn.pred)
  print(mean(knn.pred != test.Y))
}

set.seed(1)
knn.pred <- knn(train=train.X,
                test=test.X,
                cl=train.Y,
                k=1)
# Table for the best K
table(knn.pred,test.Y)



#scaled
for (K in c(1,3,5,10,100))
{
  set.seed(1)
  knn.pred <- knn(train=train.X.scaled,
                  test=test.X.scaled,
                  cl=train.Y,
                  k=K)
  
  head(knn.pred)
  print(mean(knn.pred != test.Y))
}

set.seed(1)
knn.pred <- knn(train=train.X.scaled,
                test=test.X.scaled,
                cl=train.Y,
                k=1)

# Table for the best K
table(knn.pred,test.Y)


#----------------------------------------------------------------------------------------
# Model Validation
#----------------------------------------------------------------------------------------
7365*.2
7365-1473

#Below is the 80/20 Validation set UNSCALED---------------------------------------------------------
set.seed(1)
n <- nrow(Music_Dataset)
train <- sample(1:n, 5892)

train.X <- Music_Dataset[train,c("energy","liveness","tempo","speechiness","acousticness",
                                 "instrumentalness","danceability","duration_ms","loudness","valence")]

train.Y <- Music_Dataset$weeksAbove[train]

test.X <- Music_Dataset[-train,c("energy","liveness","tempo","speechiness","acousticness",
                                 "instrumentalness","danceability","duration_ms","loudness","valence")]

test.Y <- Music_Dataset$weeksAbove[-train]

for (K in c(1,3,5,10,100))
{
  set.seed(1)
  knn.pred <- knn(train=train.X,
                  test=test.X,
                  cl=train.Y,
                  k=K)
  
  head(knn.pred)
  print(mean(knn.pred != test.Y))
}


set.seed(1)
knn.pred <- knn(train=train.X.scaled,
                test=test.X.scaled,
                cl=train.Y,
                k=100)
# Table for the best K
table(knn.pred,test.Y)


#Below is the 80/20 Validation set SCALED ---------------------------------------------------------

n <- nrow(Music_Dataset)
set.seed(1)
train <- sample(1:n, 5892)

train.X <- scale(Music_Dataset[train,c("energy","liveness","tempo","speechiness","acousticness",
                                       "instrumentalness","danceability","duration_ms","loudness","valence")])
train.X

train.Y <- Music_Dataset$weeksAbove[train]

test.X <-scale(Music_Dataset[-train,c("energy","liveness","tempo","speechiness","acousticness",
                                      "instrumentalness","danceability","duration_ms","loudness","valence")],
               center = attr(train.X, "scaled:center"),
               scale = attr(train.X,"scaled:scale"))

test.Y <- Music_Dataset$weeksAbove[-train]

#running KNN 
for (K in c(1,3,5,10,100))
{
  set.seed(1)
  knn.pred <- knn(train=train.X,
                  test=test.X,
                  cl=train.Y,
                  k=K)
  
  head(knn.pred)
  print(mean(knn.pred != test.Y))
}
table(knn.pred,test.Y)



#below is CV Kinda Confusing?---------------------------------------------------------
for (K in c(1,3,5,10,100))
{
  set.seed(1)
  knn.cv.pred <- knn.cv(train = Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                                                 "instrumentalness","danceability","duration_ms","loudness","valence")],
                        cl = Music_Dataset$weeksAbove,
                        k=K)
  
  head(knn.pred)
  print(mean(knn.pred != test.Y))
  
}

table(knn.cv.pred,Music_Dataset$weeksAbove)


#----------------------------------------------------------------------------------------
# SVM Linear
#----------------------------------------------------------------------------------------

set.seed(1)
#training
dat = data.frame(x = train.X, y = as.factor(train.Y))
out = svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)



#validation
set.seed(1)
tune.out = tune(method = svm,
                y~. , data = dat,
                kernel = "linear",
                range = list (cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

#testing
out = svm(y~., data = dat, kernel = "linear", cost = 1)
dat.te = data.frame(x = test.X, y = as.factor(test.Y))
pred.te = predict(out,newdata = dat.te)

table(pred.te, dat.te$y)


#best model
svm.pred <- predict(tune.out$best.model, newdata = dat[-train,])
table(true = dat[-train,"y"],
      pred = svm.pred)

plot(out, dat[train,],
     x.liveness~ x.danceability)

#----------------------------------------------------------------------------------------
# SVM radial
#----------------------------------------------------------------------------------------

set.seed(1)
dat = data.frame(x = train.X, y = as.factor(train.Y))
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1,cost = 1)

plot(svmfit, dat[train,],
     x.danceability~ x.valence)

svm.pred <- predict(svmfit, newdata = dat[-train,])

table(true = dat[-train,"y"],
      pred = svm.pred)

(11+18)/(11+18+54+20)

tune.out=tune(svm,
              y~., data=dat[train,],
              kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))
summary(tune.out)

#best model
svm.pred <- predict(tune.out$best.model, newdata = dat[-train,])
table(true = dat[-train,"y"],
      pred = svm.pred)

#----------------------------------------------------------------------------------------
# SVM Polynomail
#----------------------------------------------------------------------------------------



set.seed(1)
dat = data.frame(x = train.X, y = as.factor(train.Y))

svmfit <- svm(y~., data = dat[train,], kernel = "polynomial", degrees = 1,cost = 1)

plot(svmfit, dat[train,],
     x.loudness~ x.danceability)

svm.pred <- predict(svmfit, newdata = dat[-train,])

table(true = dat[-train,"y"],
      pred = svm.pred)


#This took a long ass time
tune.out=tune(svm,
              y~., data=dat[train,],
              kernel="polynomial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          degree=c(1,2,3,4)))

summary(tune.out)

svm.pred <- predict(tune.out$best.model, newdata = dat[-train,])
table(true = dat[-train,"y"],
      pred = svm.pred)



#----------------------------------------------------------------------------------------
# FINAL RESULTS
#----------------------------------------------------------------------------------------



#Here is the Best model KNN  ------------------------------------

train.X <- scale(Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                                  "instrumentalness","danceability","duration_ms","loudness","valence")])

train.Y <- Music_Dataset$weeksAbove

test.X <-scale(Music_Dataset[,c("energy","liveness","tempo","speechiness","acousticness",
                                "instrumentalness","danceability","duration_ms","loudness","valence")],
               center = attr(train.X, "scaled:center"),
               scale = attr(train.X,"scaled:scale"))

test.Y <- Music_Dataset$weeksAbove


set.seed(1)
knn.pred <- knn(train=train.X,
                test=test.X,
                cl=train.Y,
                k=10)

head(knn.pred)
print(mean(knn.pred != test.Y))

# Table for the best K
table(knn.pred,test.Y)
summary(knn.pred)


#-------------- SVM ------------------------------------


n <- nrow(Music_Dataset)
set.seed(1)
train <- sample(1:n, 5892)
#It says training but don't worry its actually the whole data set
dat = data.frame(x = train.X, y = as.factor(train.Y))
out = svm(y~., data = dat, kernel = "linear", cost = 0.1)
summary(out)
table(out$fitted, dat$y)

plot(out, dat[train,],
     x.liveness~ x.danceability)


# Scaled
n <- nrow(Music_Dataset)
set.seed(1)
train <- sample(1:n, 7365)
dat = data.frame(x = test.X, y = as.factor(test.Y))
out = svm(y~., data = dat, kernel = "linear", cost = 0.1)
summary(out)
table(out$fitted, dat$y)

plot(out, dat[train,],
     x.valence~ x.danceability)



dat = data.frame(x = test.X, y = as.factor(test.Y))
out = svm(y~., data = dat, kernel = "polynomial", degrees = 1,cost = 1)
summary(out)
table(out$fitted, dat$y)

plot(out, dat[train,],
     x.valence~ x.danceability)


dat = data.frame(x = test.X, y = as.factor(test.Y))
out = svm(y~., data = dat, kernel = "radial", gamma = 1,cost = 1)
summary(out)
table(out$fitted, dat$y)

plot(out, dat[train,],
     x.valence~ x.danceability)

  
  
  
  
  
  
  
  