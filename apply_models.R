library(ggplot2)
library(MASS)
library(gridExtra)

current_dir = getwd()
setwd(current_dir)

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

kernel_version = c('4.0', '4.9')
cwe476 = c(n_cwe476[1], n_cwe476[2])
modules = c(n_modules[1],n_modules[2])
module_tax = c(cwe_476_tax1, cwe_476_tax2)
real_tax = c(n_cwe476[1]/n_modules[1], n_cwe476[2]/n_modules[2])

df = data.frame(kernel_version, cwe476, modules, module_tax, real_tax)
pdf("cwe476.png", height=4, width=6)
grid.table(df)
dev.off()

