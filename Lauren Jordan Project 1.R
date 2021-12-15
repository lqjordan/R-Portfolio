#Part One
pmsm_temperature_data <- read.csv("/Volumes/GoogleDrive/My Drive/Maryville/DSCI 302 (Intro to R)/Final Project/pmsm_temperature_data.csv")
View(pmsm_temperature_data)

#Part Two
str(pmsm_temperature_data)
pmsm_temperature_data$stator_yoke <- as.factor(pmsm_temperature_data$stator_yoke)
print(class(pmsm_temperature_data$stator_yoke))

#Part Three
is.numeric(pmsm_temperature_data$pm)
min(pmsm_temperature_data$pm, na.rm = TRUE)
max(pmsm_temperature_data$pm, na.rm = TRUE)
mean(pmsm_temperature_data$pm, na.rm = TRUE)
median(pmsm_temperature_data$pm, na.rm = TRUE)
sd(pmsm_temperature_data$pm, na.rm = TRUE)
quantile (pmsm_temperature_data$pm, probs = c(0.25,0.5,0.75), na.rm = TRUE)

#Part Four
is.numeric(pmsm_temperature_data$motor_speed)
min(pmsm_temperature_data$motor_speed, na.rm = TRUE)
max(pmsm_temperature_data$motor_speed, na.rm = TRUE)
mean(pmsm_temperature_data$motor_speed, na.rm = TRUE)
median(pmsm_temperature_data$motor_speed, na.rm = TRUE)
sd(pmsm_temperature_data$motor_speed, na.rm = TRUE)
quantile (pmsm_temperature_data$motor_speed, probs = c(0.25,0.5,0.75), na.rm = TRUE)

#Part Five
is.numeric(pmsm_temperature_data$motor_speed)
is.numeric(pmsm_temperature_data$pm)
cor(pmsm_temperature_data$motor_speed,pmsm_temperature_data$pm, use = "complete.obs")

#Part Six
table (pmsm_temperature_data$stator_yoke)
names(sort(-prop.table(table(pmsm_temperature_data$stator_yoke))))[1]

#Part Seven
library(ggplot2)
ggplot(data= pmsm_temperature_data, aes(x=pm))+
  ggtitle("Density of Permanent Magnet")+
  geom_histogram(aes(y=..density..), colour ="white", fill="purple")+
  geom_density(alpha = .2, fill="pink") + geom_vline(aes(xintercept=mean(pm, na.rm=TRUE)),
                                                     color = "black", linetype ="dashed", size = 1)

#Part Eight
ggplot(data = pmsm_temperature_data, aes(x=motor_speed, y=pm))+ 
  ggtitle("Scatter Plot of PM Against Motor Speed") +
  geom_point()+
  geom_smooth()

#Part Nine
ggplot(data = pmsm_temperature_data, aes(x=stator_yoke, y=pm))+
  ggtitle("Box Plot of PM by Stator Yoke")+
  geom_boxplot(aes(col=stator_yoke), notch =TRUE)

ggsave("/Volumes/GoogleDrive/My Drive/Maryville/DSCI 302 (Intro to R)/Final Project/pmyoke.jpg")

#Part Ten
lm.result <- lm(pm ~ ambient + coolant + motor_speed + torque, data = pmsm_temperature_data)
summary (lm.result)

lm.result2 <- lm(pm ~ ambient + coolant + u_d + motor_speed + torque + stator_winding, data = pmsm_temperature_data)
summary (lm.result2)

lm.result3 <- lm(pm ~ ambient + coolant + u_d + u_q + motor_speed + torque + stator_yoke + stator_winding, data = pmsm_temperature_data)
summary (lm.result3)

#Part Eleven
pmsm_temperature_data <- pmsm_temperature_data[complete.cases(pmsm_temperature_data),]
NormalizedData <- function(val){
  result <- (val - min(val))/(max(val)-min(val))
  return (result)
}
str(pmsm_temperature_data)
pmsm_temperature_data$pm <- NormalizedData(pmsm_temperature_data$pm)
pmsm_temperature_data$ambient <- NormalizedData(pmsm_temperature_data$ambient)
pmsm_temperature_data$coolant <- NormalizedData(pmsm_temperature_data$coolant)
pmsm_temperature_data$motor_speed <- NormalizedData(pmsm_temperature_data$motor_speed)
pmsm_temperature_data$u_d <- NormalizedData(pmsm_temperature_data$u_d)
pmsm_temperature_data$u_q <- NormalizedData(pmsm_temperature_data$u_q)
pmsm_temperature_data$torque <- NormalizedData(pmsm_temperature_data$torque)
summary(pmsm_temperature_data)
library (class)


predictors <- c("pm","ambient","coolant","motor_speed")
data.predictors <- pmsm_temperature_data[predictors]
data.target <- pmsm_temperature_data$stator_yoke
sample.size <- floor(0.85*nrow(pmsm_temperature_data))
train <- data.predictors[1:sample.size, ]
test <- data.predictors[-c(1:sample.size), ]
cl <- data.target[1:sample.size]
knn.test.predict <- knn(train=train, test=test, cl, k = 1)
test.label <- data.target[-c(1:sample.size)]
table(test.label, knn.test.predict)


predictors <- c("pm","ambient","coolant","u_d","u_q","motor_speed","torque")
data.predictors <- pmsm_temperature_data[predictors]
data.target <- pmsm_temperature_data$stator_yoke
sample.size <- floor(0.85*nrow(pmsm_temperature_data))
train <- data.predictors[1:sample.size, ]
test <- data.predictors[-c(1:sample.size), ]
cl <- data.target[1:sample.size]
knn.test.predict <- knn(train, test, cl, k = 1)
test.label <- data.target[-c(1:sample.size)]
table(test.label, knn.test.predict)
