#PROJECT 4

library(TH.data)
GlaucomaM_data <- GlaucomaM
head(GlaucomaM_data)

#Data Pre-Processing
#check for NA's
summary(GlaucomaM_data)
#Checking for NA's
colSums(is.na(GlaucomaM_data))
#No NA's.

lapply(GlaucomaM_data, class)
#good

set.seed(5) #random sample remains fixed in every run in R.
index_training <- sample(1:nrow(GlaucomaM_data), round(0.7*nrow(GlaucomaM_data)))
training_data <- GlaucomaM_data[index_training,]
test_data <- GlaucomaM_data[-index_training,]


#MODEL FITTING: ELASTIC NET
X <- as.matrix(training_data[,-63])
Y <- training_data[,63]
library(glmnet)

fit1 <- cv.glmnet(X,Y, family='binomial')
#The next command tells which variables are selected. In addition, it gives the estimated coefficients. In this project, we care more about whether we have a good predictor for glaucoma or not, and not so much how individual variables affect the outcome. So we do not care much about the values of the estimated coefficients.
coef(fit1, s = "lambda.min")


#Let us see the performance in the training data:

#The function "show" calculates missclassification error, i.e. how many people cases of normal or glaucoma were wrongly classified
show <- function(tt){
  print(tt)
  cat(paste("Misclassification rate =", round(1-sum(diag(tt))/sum(tt),2),"\n"))
  invisible()
  }


nx <- as.matrix(training_data[,-63])#WHAT IS NX??
nrow(training_data)
nrow(test_data)
show(with(training_data, table(actual=Y, 
                                predicted=predict(fit1, newx = nx, s="lambda.min", type="class"))))

#Now let's see the performance on the test data:

nx <- as.matrix(test_data[,-63])
show(with(test_data, table(actual=test_data[,63],
                           predicted=predict(fit1, newx = nx, s="lambda.min", type="class"))))

# EXTRA: RANDOM FOREST

library(randomForest)
rf <- randomForest(X,Y)

#Performance on training data
show(with(test_data, table(actual=Y, predicted=predict(rf))))

#Performance on test data
show(with(test_data, table(actual=test_data[,63], predicted=predict(rf, newdata = test_data[,-63]))))





