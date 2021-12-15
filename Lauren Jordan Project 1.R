#Part One
breast_cancer_data <- read.csv("/Volumes/GoogleDrive/My Drive/Maryville/DSCI 302 (Intro to R)/Week 8/breast_cancer_data.csv", stringsAsFactors=TRUE)
View(breast_cancer_data)

#Part Two
BoxplotPredictorOnTarget <- function (target, predictor){
  boxplot(target ~ predictor, data = breast_cancer_data, col=c("pink"), main = "Boxplot Predictor on Target", xlab = "Diagnosis")
}
area_meanbydiagnosis <- BoxplotPredictorOnTarget(breast_cancer_data$area_mean, breast_cancer_data$diagnosis)
area_sebydiagnosis <- BoxplotPredictorOnTarget (breast_cancer_data$area_se, breast_cancer_data$diagnosis)
texture_meanbydiagnosis <- BoxplotPredictorOnTarget (breast_cancer_data$texture_mean, breast_cancer_data$diagnosis)

#Part Three
library(class)
help(knn)
predictors <- c("area_mean","area_se")
data.predictors <- breast_cancer_data[predictors]
data.target <- breast_cancer_data$diagnosis
sample.size <- floor (0.8*nrow(breast_cancer_data))
train <- data.predictors [1:sample.size, ]
test <- data.predictors[-c(1:sample.size), ]
cl <- data.target[1:sample.size]
knn.test.predict <- knn(train, test, cl, k=1)
test.label <- data.target [-c(1:sample.size)]
table(test.label, knn.test.predict)


predictors <- c("area_mean","area_se","texture_mean")
data.predictors <- breast_cancer_data[predictors]
data.target <- breast_cancer_data$diagnosis
sample.size <- floor (0.8*nrow(breast_cancer_data))
train <- data.predictors [1:sample.size, ]
test <- data.predictors[-c(1:sample.size), ]
cl <- data.target[1:sample.size]
knn.test.predict <- knn(train, test, cl, k=1)
test.label <- data.target [-c(1:sample.size)]
table(test.label, knn.test.predict)