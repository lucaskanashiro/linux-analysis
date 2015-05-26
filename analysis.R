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
boxplot(cwe, names=c("CWE476", "CWE457", "CWE401"))

dev.off()

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

