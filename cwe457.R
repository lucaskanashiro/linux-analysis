library(ggplot2)

setwd("/home/kanashiro/tcc/linux-analysis")
pdf("~/tcc/linux-analysis/cwe457.pdf")

# Read data and handle some usefull variables
full_data <- read.csv("cwe.csv", header=TRUE, sep=',')

# Calculation of CWE476 tax per module
full_data$taxCWE457 <- full_data$CWE457/full_data$Modules

cwe457 <- full_data$taxCWE457
modules <- full_data$Modules

# Create new data frame
data <- data.frame(modules,cwe457)

# Generate 4-plot - CWE457
library(Hmisc)
t = 1:length(cwe457)
par(mfrow = c(2, 2)) 
hist(cwe457,main="",xlab="CWE457",ylim=c(0,130),xlim=c(0.005,0.012))
qqnorm(cwe457, ylab="CWE457 Quantiles", xlab="Normal Quantiles");qqline(cwe457)
plot(t,cwe457,ylab="CWE457",xlab="Versions",type="l", xlim=c(0,395), ylim=c(0.0055,0.012))
plot(cwe457,Lag(cwe457),xlab="cwe457[i-1]",ylab="cwe457[i]",ylim=c(0.005,0.012),xlim=c(0.005,0.012))
mtext("CWE 457 Data: 4-Plot", line = 0.5, outer = TRUE)

# Generate boxplot - CWE457
par(mfrow=c(1,1))
box <- boxplot(cwe457, main="CWE 457", ylim=c(0.0055,0.012))
# Identify outliers with IQR (Tukey, 1977)

# Remove identified outliers
cwe457 <- data$cwe457[! data$cwe457 %in% box$out]

index_to_remove_modules <- which(data$cwe457 %in% box$out)
modules <- data$modules[-index_to_remove_modules]

# Levene's Test - Check variance
library(car)
groups = as.factor(rep(1:4,each=100))
groups <- groups[-c(1:7,115:121,229:235,305:311)]
leveneTest(cwe457,groups)

# Find critical value.
qf(.95,3,387)

# Run test - Check randomness 
library(lawstat)
runs.test(cwe457)
# alpha = 5%, critical value = +-1.96 (standard normal distribution)

# Linear model is not the best one
# Trying to fit some model with loess to use as parameter

# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  testindex <- sample(index, trunc(length(index)/3))
  testset <- dataframe[testindex, ]
  trainset <- dataframe[-testindex, ]
  list(trainset=trainset,testset=testset)
}

# Split data into testing and training
splits <- splitdf(data)

training <- splits$trainset
testing <- splits$testset

# Loess
loess_fit <- loess(cwe457 ~ modules, data=training)
loess_residual <- resid(loess_fit)
j <- order(training$modules)
loess_predicted <- predict(loess_fit, newdata=testing)

plot(training$modules, training$cwe457, xlab="Modules", 
     ylab="taxCWE457", main="Loess - CWE457", ylim=c(0.0055,0.012),
     xlim=c(14000,38000))
lines(training$modules[j], loess_fit$fitted[j], col="red", lwd=3)

hist(loess_residual)

hist(testing$cwe457-loess_predicted)


# Great fitted model, probably a quadratic or cubic model aproximate for this one
# Next we will try to test it

# Quadratic
quadratic_model <- lm(cwe457 ~ modules + I(modules^2), data=training)
quadratic_residual <- resid(quadratic_model)
quadratic_predicted <- predict(quadratic_model, newdata=testing)

quadratic_function <- function(x) quadratic_model$coefficient[3]*x^2 + 
  quadratic_model$coefficient[2]*x + quadratic_model$coefficient[1]

plot(training$modules, training$cwe457, xlab="Modules", 
     ylab="taxCWE457", main="Quadratic Model - CWE457", ylim=c(0.0055,0.012),
     xlim=c(14000,38000))
lines(sort(training$modules), fitted(quadratic_model)[order(training$modules)], col='red', type='b')

hist(quadratic_residual)

hist(testing$cwe457-quadratic_predicted, main="Residual of Testing")

# Cubic
cubic_model <- lm(cwe457 ~ modules + I(modules^2) + I(modules^3), data=training)
cubic_residual <- resid(cubic_model)
cubic_predicted <- predict(cubic_model, newdata=testing)

cubic_function <- function(x) cubic_model$coefficient[4]*x^3 + 
  cubic_model$coefficient[3]*x^2 + cubic_model$coefficient[2]*x + 
  cubic_model$coefficient[1]

plot(training$modules, training$cwe457, xlab="Modules", 
     ylab="taxCWE457", main="Cubic Model - CWE457", ylim=c(0.0055,0.012),
     xlim=c(14000,38000))
lines(sort(training$modules), fitted(cubic_model)[order(training$modules)], col='red', type='b')

hist(cubic_residual)

hist(testing$cwe457-cubic_predicted, main="Residual of Testing")

# Comparative of models
# Plot all fitted models
ggplot(testing, aes(modules)) + 
  geom_point(aes(y = testing$cwe457, colour = "real")) + 
  geom_line(aes(y = cubic_predicted, colour = "cubic"), linetype=2) +
  geom_line(aes(y = quadratic_predicted, colour = "quadratic"), linetype=4) +
  geom_line(aes(y = loess_predicted, colour = "loess")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue","green","black"),name="tax CWE457")  +
  scale_y_continuous(limits=c(0.001,0.012), breaks=seq(0,0.015,0.001)) +
  scale_x_continuous(limits=c(14000,38000), breaks=seq(0,40000,4000))

# ANOVA analysis
anova(quadratic_model, cubic_model)

# Comare R-squared error
summary(quadratic_model)
summary(cubic_model)

# K-fold cross validation (compare mean squared error)
library(DAAG)
cubic_cv <- CVlm(df=testing, 
                 form.lm=formula(cwe457 ~ modules + I(modules^2) + I(modules^3)),
                 m=10, main="k-fold Cross Validation - Cubic Model")
quadratic_cv <- CVlm(df=testing, 
                     form.lm=formula(cwe457 ~ modules + I(modules^2)), 
                     m=10, main="k-fold Cross Validation - Quadratic Model")


require(boot)

model2 <- glm(cwe457 ~ poly(modules, 2), data=data)
model3 <- glm(cwe457 ~ poly(modules, 3), data=data)
model4 <- glm(cwe457 ~ poly(modules, 4), data=data)
model5 <- glm(cwe457 ~ poly(modules, 5), data=data)
model6 <- glm(cwe457 ~ poly(modules, 6), data=data)
model7 <- glm(cwe457 ~ poly(modules, 7), data=data)
model8 <- glm(cwe457 ~ poly(modules, 8), data=data)
model9 <- glm(cwe457 ~ poly(modules, 9), data=data)
model10 <- glm(cwe457 ~ poly(modules, 10), data=data)
model11 <- glm(cwe457 ~ poly(modules, 11), data=data)
model12 <- glm(cwe457 ~ poly(modules, 12), data=data)
model13 <- glm(cwe457 ~ poly(modules, 13), data=data)
model14 <- glm(cwe457 ~ poly(modules, 14), data=data)
model15 <- glm(cwe457 ~ poly(modules, 15), data=data)
model16 <- glm(cwe457 ~ poly(modules, 16), data=data)
model17 <- glm(cwe457 ~ poly(modules, 17), data=data)
model18 <- glm(cwe457 ~ poly(modules, 18), data=data)
model19 <- glm(cwe457 ~ poly(modules, 19), data=data)
model20 <- glm(cwe457 ~ poly(modules, 20), data=data)

squared_err <- rep(0,19)

cv2 <- cv.glm(data=data, glmfit=model2, K=10)
squared_err[1] <- cv2$delta[2]
cv3 <- cv.glm(data=data, glmfit=model3, K=10)
squared_err[2] <- cv3$delta[2]
cv4 <- cv.glm(data=data, glmfit=model4, K=10)
squared_err[3] <- cv4$delta[2]
cv5 <- cv.glm(data=data, glmfit=model5, K=10)
squared_err[4] <- cv5$delta[2]
cv6 <- cv.glm(data=data, glmfit=model6, K=10)
squared_err[5] <- cv6$delta[2]
cv7 <- cv.glm(data=data, glmfit=model7, K=10)
squared_err[6] <- cv7$delta[2]
cv8 <- cv.glm(data=data, glmfit=model8, K=10)
squared_err[7] <- cv8$delta[2]
cv9 <- cv.glm(data=data, glmfit=model9, K=10)
squared_err[8] <- cv9$delta[2]
cv10 <- cv.glm(data=data, glmfit=model10, K=10)
squared_err[9] <- cv10$delta[2]
cv11 <- cv.glm(data=data, glmfit=model11, K=10)
squared_err[10] <- cv11$delta[2]
cv12 <- cv.glm(data=data, glmfit=model12, K=10)
squared_err[11] <- cv12$delta[2]
cv13 <- cv.glm(data=data, glmfit=model13, K=10)
squared_err[12] <- cv13$delta[2]
cv14 <- cv.glm(data=data, glmfit=model14, K=10)
squared_err[13] <- cv14$delta[2]
cv15 <- cv.glm(data=data, glmfit=model15, K=10)
squared_err[14] <- cv15$delta[2]
cv16 <- cv.glm(data=data, glmfit=model16, K=10)
squared_err[15] <- cv16$delta[2]
cv17 <- cv.glm(data=data, glmfit=model17, K=10)
squared_err[16] <- cv17$delta[2]
cv18 <- cv.glm(data=data, glmfit=model18, K=10)
squared_err[17] <- cv18$delta[2]
cv19 <- cv.glm(data=data, glmfit=model19, K=10)
squared_err[18] <- cv19$delta[2]
cv20 <- cv.glm(data=data, glmfit=model20, K=10)
squared_err[19] <- cv20$delta[2]



plot(squared_err, xlab="Grau do modelo", ylab="Erro de predição", main="Erro de predição - CWE457", xlim=c(1,19))
lines(squared_err, col='red', type='b')
axis(2, at=1:19, labels=2:20)



dev.off()
