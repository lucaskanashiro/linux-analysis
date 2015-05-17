library(ggplot2)
library(MASS)

setwd("/home/kanashiro/tcc/linux-analysis")

# Read data and handle some usefull variables
data <- read.csv("cwe.csv", header=TRUE, sep=',')

# Calculation of CWE tax per module
data$taxCWE476 <- data$CWE476/data$Modules
data$taxCWE457 <- data$CWE457/data$Modules
data$taxCWE401 <- data$CWE401/data$Modules

pdf("~/tcc/linux-analysis/boxplots.pdf")
# Boxplot to identify outliers
cwe <- data.frame(data$taxCWE476,data$taxCWE457,data$taxCWE401)
boxplotCWE <- boxplot(cwe, names=c("CWE476", "CWE457", "CWE401"), range=0.5)

boxplotCWE476 <- boxplot(data$taxCWE476, main="CWE476", range=0.5)
boxplotCWE457 <- boxplot(data$taxCWE457, main="CWE457", range=0.5)
boxplotCWE401 <- boxplot(data$taxCWE401, main="CWE401", range=0.5)

dev.off()
# Remove outliers
cwe476 <- data$taxCWE476[! data$taxCWE476 %in% boxplotCWE476$out]
cwe457 <- data$taxCWE457[! data$taxCWE457 %in% boxplotCWE457$out]
cwe401 <- data$taxCWE401[! data$taxCWE401 %in% boxplotCWE401$out]

# Remove modules related to removed outliers
index_to_remove_modulesCWE476 <- which(data$taxCWE476 %in% boxplotCWE476$out)
modulesCWE476 <- data$Modules[-index_to_remove_modulesCWE476]

index_to_remove_modulesCWE457 <- which(data$taxCWE457 %in% boxplotCWE457$out)
modulesCWE457 <- data$Modules[-index_to_remove_modulesCWE457]

index_to_remove_modulesCWE401 <- which(data$taxCWE401 %in% boxplotCWE401$out)
modulesCWE401 <- data$Modules[-index_to_remove_modulesCWE401]

# Calculate correlation matrix
usage_data <- data.frame(data$Modules,data$taxCWE476,data$taxCWE457,data$taxCWE401)

correlation_matrix <- symnum(cor(usage_data))
correlation_matrix


# Plot of Modules vs all Vulnerabilities in the same one
modules <- data$Modules

pdf("~/tcc/linux-analysis/scatterplot.pdf")
ggplot(data, aes(modules)) + 
  geom_point(aes(y = data$taxCWE476, colour = "CWE476")) + 
  geom_point(aes(y = data$taxCWE457, colour = "CWE457")) +
  geom_point(aes(y = data$taxCWE401, colour = "CWE401")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("blue","red","yellow"),name="Vulnerabilities") +
  scale_y_continuous(limits=c(0.001,0.012), breaks=seq(0,0.015,0.001)) +
  scale_x_continuous(limits=c(14000,38000), breaks=seq(0,40000,2000))

ggplot(data, aes(modules)) + 
  geom_line(aes(y = data$taxCWE476, colour = "CWE476")) + 
  geom_line(aes(y = data$taxCWE457, colour = "CWE457")) +
  geom_line(aes(y = data$taxCWE401, colour = "CWE401")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("blue","red","yellow"), name="Vulnerabilities") +
  scale_y_continuous(limits=c(0.001,0.012), breaks=seq(0,0.015,0.001)) +
  scale_x_continuous(limits=c(14000,38000), breaks=seq(0,40000,2000))
dev.off()


# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

# Trying to fit model CWE476
dfCWE476 <- data.frame(modulesCWE476, cwe476)
splits <- splitdf(dfCWE476)

training <- splits$trainset
testing <- splits$testset

# Loess
fit <- loess(cwe476 ~ modulesCWE476, data=training)
residual <- resid(fit)
j <- order(training$modulesCWE476)
predicted <- predict(fit, newdata=testing)

# Quadratic
quadratic_model <- lm(cwe476 ~ poly(modulesCWE476, 2), data=training)
residual_quadratic <- resid(quadratic_model)
predicted_quadratic_model <- predict(quadratic_model, newdata=testing)

# Cubic
cubic_model <- lm(cwe476 ~ poly(modulesCWE476, 3), data=training)
residual_cubic <- resid(quadratic_model)
predicted_cubic_model <- predict(cubic_model, newdata=testing)

pdf("~/tcc/linux-analysis/cwe476.pdf")
# Distribution
qplot(x=cwe476, data=dfCWE476, binwidth=.0005,
      xlab='CWE476 / Modules',
      ylab='Count',
      color = I('white'), fill = I('orange'))#+
  #scale_x_continuous(limits=c(0.0055,0.0115), breaks=seq(0,0.015,0.0005)) +
  #scale_y_continuous(limits=c(0,130), breaks=seq(0,130,10))


# loess model
plot(training$modulesCWE476, training$cwe476, xlab="Modules", 
     ylab="taxCWE476", main="Local Regression - CWE476")
lines(training$modulesCWE476[j], fit$fitted[j], col="red", lwd=3)

plot(training$cwe476, residual, xlab="taxCWE476",
     ylab="residual", main="Residual of Loess - CWE476")
abline(0,0)

ggplot(testing, aes(modulesCWE476)) + 
  geom_point(aes(y = testing$cwe476, colour = "real")) + 
  geom_point(aes(y = predicted, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE476")

plot(testing$modulesCWE476, testing$cwe476-predicted, xlab="Modules",
     ylab="residual", main="Residual of Loess Model - CWE476")
abline(0,0)

# Quadratic model
plot(training$modulesCWE476, training$cwe476, xlab="Modules", 
     ylab="taxCWE476", main="Quadratic Model - CWE476")
lines(sort(training$modulesCWE476), fitted(quadratic_model)[order(training$modulesCWE476)], col='red', type='b')

plot(training$cwe476, residual_quadratic, xlab="taxCWE476",
     ylab="residual", main="Residual of Quadratic Model - CWE476")
abline(0,0)

ggplot(testing, aes(modulesCWE476)) + 
  geom_point(aes(y = testing$cwe476, colour = "real")) + 
  geom_point(aes(y = predicted_quadratic_model, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE476")

plot(testing$modulesCWE476, testing$cwe476-predicted_quadratic_model, xlab="Modules",
     ylab="residual", main="Residual of Quadratic Model - CWE476")
abline(0,0)


# Cubic model
plot(training$modulesCWE476, training$cwe476, xlab="Modules", 
     ylab="taxCWE476", main="Cubic Model - CWE476")
lines(sort(training$modulesCWE476), fitted(cubic_model)[order(training$modulesCWE476)], col='red', type='b')

plot(training$cwe476, residual_cubic, xlab="taxCWE476",
     ylab="residual", main="Residual of Cubic Model - CWE476")
abline(0,0)

ggplot(testing, aes(modulesCWE476)) + 
  geom_point(aes(y = testing$cwe476, colour = "real")) + 
  geom_point(aes(y = predicted_cubic_model, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE476")

plot(testing$modulesCWE476, testing$cwe476-predicted_cubic_model, xlab="Modules",
     ylab="residual", main="Residual of Cubic Model - CWE476")
abline(0,0)


# Plot all fitted models
ggplot(testing, aes(modulesCWE476)) + 
  geom_point(aes(y = testing$cwe476, colour = "real")) + 
  geom_line(aes(y = predicted_cubic_model, colour = "cubic"), linetype=2) +
  geom_line(aes(y = predicted_quadratic_model, colour = "quadratic"), linetype=4) +
  geom_line(aes(y = predicted, colour = "loess")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue","green","black"),name="tax CWE476")

dev.off()




# Trying to fit model CWE457
dfCWE457 <- data.frame(modulesCWE457, cwe457)
splits <- splitdf(dfCWE457)

training <- splits$trainset
testing <- splits$testset

# Loess
fit <- loess(cwe457 ~ modulesCWE457, data=training)
residual <- resid(fit)
j <- order(training$modulesCWE457)
predicted <- predict(fit, newdata=testing)

# Quadratic
quadratic_model <- lm(cwe457 ~ poly(modulesCWE457, 2), data=training)
residual_quadratic <- resid(quadratic_model)
predicted_quadratic_model <- predict(quadratic_model, newdata=testing)

# Cubic
cubic_model <- lm(cwe457 ~ poly(modulesCWE457, 3), data=training)
residual_cubic <- resid(quadratic_model)
predicted_cubic_model <- predict(cubic_model, newdata=testing)

pdf("~/tcc/linux-analysis/cwe457.pdf")
# Distribution
qplot(x=cwe457, data=dfCWE457, binwidth=.0005,
      xlab='CWE457 / Modules',
      ylab='Count',
      color = I('white'), fill = I('orange'))#+
  #scale_x_continuous(limits=c(0.0055,0.0115), breaks=seq(0,0.015,0.0005)) +
  #scale_y_continuous(limits=c(0,130), breaks=seq(0,130,10))


# loess model
plot(training$modulesCWE457, training$cwe457, xlab="Modules", 
     ylab="taxCWE457", main="Loess - CWE457")
lines(training$modulesCWE457[j], fit$fitted[j], col="red", lwd=3)

plot(training$cwe457, residual, xlab="taxCWE457",
     ylab="residual", main="Residual of Loess - CWE457")
abline(0,0)

ggplot(testing, aes(modulesCWE457)) + 
  geom_point(aes(y = testing$cwe457, colour = "real")) + 
  geom_point(aes(y = predicted, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE457")

plot(testing$modulesCWE457, testing$cwe457-predicted, xlab="Modules",
     ylab="residual", main="Residual of Loess Model - CWE457")
abline(0,0)

# Quadratic model
plot(training$modulesCWE457, training$cwe457, xlab="Modules", 
     ylab="taxCWE457", main="Quadratic Model - CWE457")
lines(sort(training$modulesCWE457), fitted(quadratic_model)[order(training$modulesCWE457)], col='red', type='b')

plot(training$cwe457, residual_quadratic, xlab="taxCWE457",
     ylab="residual", main="Residual of Quadratic Model - CWE457")
abline(0,0)

ggplot(testing, aes(modulesCWE457)) + 
  geom_point(aes(y = testing$cwe457, colour = "real")) + 
  geom_point(aes(y = predicted_quadratic_model, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE457")

plot(testing$modulesCWE457, testing$cwe457-predicted_quadratic_model, xlab="Modules",
     ylab="residual", main="Residual of Quadratic Model - CWE457")
abline(0,0)


# Cubic model
plot(training$modulesCWE457, training$cwe457, xlab="Modules", 
     ylab="taxCWE457", main="Cubic Model - CWE457")
lines(sort(training$modulesCWE457), fitted(cubic_model)[order(training$modulesCWE457)], col='red', type='b')

plot(training$cwe457, residual_cubic, xlab="taxCWE457",
     ylab="residual", main="Residual of Cubic Model - CWE457")
abline(0,0)

ggplot(testing, aes(modulesCWE457)) + 
  geom_point(aes(y = testing$cwe457, colour = "real")) + 
  geom_point(aes(y = predicted_cubic_model, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE457")

plot(testing$modulesCWE457, testing$cwe457-predicted_cubic_model, xlab="Modules",
     ylab="residual", main="Residual of Cubic Model - CWE457")
abline(0,0)


# Plot all fitted models
ggplot(testing, aes(modulesCWE457)) + 
  geom_point(aes(y = testing$cwe457, colour = "real")) + 
  geom_line(aes(y = predicted_cubic_model, colour = "cubic"), linetype=2) +
  geom_line(aes(y = predicted_quadratic_model, colour = "quadratic"), linetype=4) +
  geom_line(aes(y = predicted, colour = "loess")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue","green","black"),name="tax CWE457")

dev.off()








# Trying to fit model CWE401
dfCWE401 <- data.frame(modulesCWE401, cwe401)
splits <- splitdf(dfCWE401)

training <- splits$trainset
testing <- splits$testset

# Loess
fit <- loess(cwe401 ~ modulesCWE401, data=training)
residual <- resid(fit)
j <- order(training$modulesCWE401)
predicted <- predict(fit, newdata=testing)

# Quadratic
quadratic_model <- lm(cwe401 ~ poly(modulesCWE401, 2), data=training)
residual_quadratic <- resid(quadratic_model)
predicted_quadratic_model <- predict(quadratic_model, newdata=testing)

# Cubic
cubic_model <- lm(cwe401 ~ poly(modulesCWE401, 3), data=training)
residual_cubic <- resid(quadratic_model)
predicted_cubic_model <- predict(cubic_model, newdata=testing)

pdf("~/tcc/linux-analysis/cwe401.pdf")
# Distribution
qplot(x=cwe401, data=dfCWE401, binwidth=.0005,
      xlab='CWE401 / Modules',
      ylab='Count',
      color = I('white'), fill = I('orange'))#+
  #scale_x_continuous(limits=c(0.0055,0.0115), breaks=seq(0,0.015,0.0005)) +
  #scale_y_continuous(limits=c(0,130), breaks=seq(0,130,10))


# loess model
plot(training$modulesCWE401, training$cwe401, xlab="Modules", 
     ylab="taxCWE401", main="Loess - CWE401")
lines(training$modulesCWE401[j], fit$fitted[j], col="red", lwd=3)

plot(training$cwe401, residual, xlab="taxCWE401",
     ylab="residual", main="Residual of Loess - CWE401")
abline(0,0)

ggplot(testing, aes(modulesCWE401)) + 
  geom_point(aes(y = testing$cwe401, colour = "real")) + 
  geom_point(aes(y = predicted, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE401")

plot(testing$modulesCWE401, testing$cwe401-predicted, xlab="Modules",
     ylab="residual", main="Residual of Loess Model - CWE401")
abline(0,0)

# Quadratic model
plot(training$modulesCWE401, training$cwe401, xlab="Modules", 
     ylab="taxCWE401", main="Quadratic Model - CWE401")
lines(sort(training$modulesCWE401), fitted(quadratic_model)[order(training$modulesCWE401)], col='red', type='b')

plot(training$cwe401, residual_quadratic, xlab="taxCWE401",
     ylab="residual", main="Residual of Quadratic Model - CWE401")
abline(0,0)

ggplot(testing, aes(modulesCWE401)) + 
  geom_point(aes(y = testing$cwe401, colour = "real")) + 
  geom_point(aes(y = predicted_quadratic_model, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE401")

plot(testing$modulesCWE401, testing$cwe401-predicted_quadratic_model, xlab="Modules",
     ylab="residual", main="Residual of Quadratic Model - CWE401")
abline(0,0)


# Cubic model
plot(training$modulesCWE401, training$cwe401, xlab="Modules", 
     ylab="taxCWE401", main="Cubic Model - CWE401")
lines(sort(training$modulesCWE401), fitted(cubic_model)[order(training$modulesCWE401)], col='red', type='b')

plot(training$cwe401, residual_cubic, xlab="taxCWE401",
     ylab="residual", main="Residual of Cubic Model - CWE401")
abline(0,0)

ggplot(testing, aes(modulesCWE401)) + 
  geom_point(aes(y = testing$cwe401, colour = "real")) + 
  geom_point(aes(y = predicted_cubic_model, colour = "predicted")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue"),name="tax CWE401")

plot(testing$modulesCWE401, testing$cwe401-predicted_cubic_model, xlab="Modules",
     ylab="residual", main="Residual of Cubic Model - CWE401")
abline(0,0)


# Plot all fitted models
ggplot(testing, aes(modulesCWE401)) + 
  geom_point(aes(y = testing$cwe401, colour = "real")) + 
  geom_line(aes(y = predicted_cubic_model, colour = "cubic"), linetype=2) +
  geom_line(aes(y = predicted_quadratic_model, colour = "quadratic"), linetype=4) +
  geom_line(aes(y = predicted, colour = "loess")) +
  theme(axis.title.y=element_blank()) +
  scale_colour_manual(values=c("red","blue","green","black"),name="tax CWE401")

dev.off()


