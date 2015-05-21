library(ggplot2)

setwd("/home/kanashiro/tcc/linux-analysis")
pdf("~/tcc/linux-analysis/cwe476.pdf")

# Read data and handle some usefull variables
full_data <- read.csv("cwe.csv", header=TRUE, sep=',')

# Calculation of CWE476 tax per module
full_data$taxCWE476 <- full_data$CWE476/full_data$Modules

cwe476 <- full_data$taxCWE476
modules <- full_data$Modules

# Create new data frame
data <- data.frame(modules,cwe476)

# Generate 4-plot - CWE476
library(Hmisc)
t = 1:length(cwe476)
par(mfrow = c(2, 2)) 
hist(cwe476,main="",xlab="CWE476")
qqnorm(cwe476);qqline(cwe476)
plot(t,cwe476,ylab="CWE476",xlab="Run Sequence",type="l")
plot(cwe476,Lag(cwe476),xlab="cwe476[i-1]",ylab="cwe476[i]")
mtext("CWE 476 Data: 4-Plot", line = 0.5, outer = TRUE)

# Generate boxplot - CWE476
par(mfrow=c(1,1))
boxplot(cwe476, main="CWE 476")
# Identify outliers with IQR (Tukey, 1977)

# Levene's Test - Check variance
library(car)
groups = as.factor(rep(1:4,each=100))
groups <- groups[-c(1,2,115,116,229,230,305,306,307)]
leveneTest(cwe476,groups)

# Find critical value.
qf(.95,3,387)

# Run test - Check randomness 
library(lawstat)
runs.test(cwe476)
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
loess_fit <- loess(cwe476 ~ modules, data=training)
loess_residual <- resid(loess_fit)
j <- order(training$modules)
loess_predicted <- predict(loess_fit, newdata=testing)

plot(training$modules, training$cwe476, xlab="Modules", 
     ylab="taxCWE476", main="Loess - CWE476")
lines(training$modules[j], loess_fit$fitted[j], col="red", lwd=3)

hist(loess_residual)

hist(testing$cwe476-loess_predicted)


# Great fitted model, probably a quadratic or cubic model aproximate for this one
# Next we will try to test it

# Quadratic
quadratic_model <- lm(cwe476 ~ modules + I(modules^2), data=training)
quadratic_residual <- resid(quadratic_model)
quadratic_predicted <- predict(quadratic_model, newdata=testing)

quadratic_function <- function(x) quadratic_model$coefficient[3]*x^2 + 
  quadratic_model$coefficient[2]*x + quadratic_model$coefficient[1]

plot(training$modules, training$cwe476, xlab="Modules", 
     ylab="taxCWE476", main="Quadratic Model - CWE476")
lines(sort(training$modules), fitted(quadratic_model)[order(training$modules)], col='red', type='b')

hist(quadratic_residual)

hist(testing$cwe476-quadratic_predicted, main="Residual of Testing")

# Cubic
cubic_model <- lm(cwe476 ~ modules + I(modules^2) + I(modules^3), data=training)
cubic_residual <- resid(cubic_model)
cubic_predicted <- predict(cubic_model, newdata=testing)

cubic_function <- function(x) cubic_model$coefficient[4]*x^3 + 
  cubic_model$coefficient[3]*x^2 + cubic_model$coefficient[2]*x + 
  cubic_model$coefficient[1]

plot(training$modules, training$cwe476, xlab="Modules", 
     ylab="taxCWE476", main="Cubic Model - CWE476")
lines(sort(training$modules), fitted(cubic_model)[order(training$modules)], col='red', type='b')

hist(cubic_residual)

hist(testing$cwe476-cubic_predicted, main="Residual of Testing")

# Comparative of models
# Plot all fitted models
ggplot(testing, aes(modules)) + 
  geom_point(aes(y = testing$cwe476, colour = "real")) + 
  geom_line(aes(y = cubic_predicted, colour = "cubic"), linetype=2) +
  geom_line(aes(y = quadratic_predicted, colour = "quadratic"), linetype=4) +
  geom_line(aes(y = loess_predicted, colour = "loess")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue","green","black"),name="tax CWE476")

# ANOVA analysis
anova(quadratic_model, cubic_model)

# Comare R-squared error
quadratic_model
cubic_model

dev.off()
