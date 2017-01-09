library(ggplot2)
library(MASS)
library(gridExtra)

current_dir = getwd()
setwd(current_dir)

cwe476_cubic_model <- function(modules) {
	tax = (1.911224*10^(-15))*modules^3 - (1.72028*10^(-10))*modules^2 + (4.85747*10^(-6))*modules - (0.03460173)
	return(tax)
}

cwe457_cubic_model <- function(modules) {
	tax = (-6.466983*10^(-16))*modules^3 + (5.603787*10^(-11))*modules^2 - (1.639652*10^(-6))*modules + (0.02287291)
	return(tax)
}

# Read data and handle some usefull variables
data <- read.csv("cwe.csv", header=TRUE, sep=',')

# Calculation of CWE tax per module
n_cwe476 = data$CWE476
n_modules = data$Modules
n_cwe457 = data$CWE457

data$taxCWE476 <- data$CWE476/data$Modules
data$taxCWE457 <- data$CWE457/data$Modules
data$taxCWE401 <- data$CWE401/data$Modules

modules1 = n_modules[1]
modules2 = n_modules[2]

cwe_476_tax1 = (1.911224*10^(-15))*modules1^3 - (1.72028*10^(-10))*modules1^2 + (4.85747*10^(-6))*modules1 - (0.03460173)
cwe_476_tax2 = (1.911224*10^(-15))*modules2^3 - (1.72028*10^(-10))*modules2^2 + (4.85747*10^(-6))*modules2 - (0.03460173)

i=1
real_taxes <- vector(mode="numeric", length=0)
cwe476_model_taxes <- vector(mode="numeric", length=0)
cwe457_model_taxes <- vector(mode="numeric", length=0)

for (cwe in data$CWE476){
	real_taxes[i] <- c(n_cwe476[i]/n_modules[i])
	cwe476_model_taxes[i] <- cwe476_cubic_model(n_modules[i])
	cwe457_model_taxes[i] <- cwe457_cubic_model(n_modules[i])
	i=i + 1
}
kernel_version = c('4.0', '4.9')
cwe476 = c(n_cwe476[1], n_cwe476[2])

df = data.frame(kernel_version, n_cwe476, n_modules, cwe476_model_taxes, real_taxes)
pdf("cwe476.png", height=4, width=7)
grid.table(df)
dev.off()


df2 = data.frame(kernel_version, n_cwe457, n_modules, cwe457_model_taxes, real_taxes)
pdf("cwe457.png", height=4, width=7)
grid.table(df)
dev.off()
