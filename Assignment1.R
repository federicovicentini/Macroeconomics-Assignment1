# Federico Vicentini
# 23/05/2022

# Macroeconomics - Assignment no.1

#################
##### POINT 1####
#################

# Clear the variables
rm(list = ls())

# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(quantmod)
library(eurostat)

# Get Data from Fred website
nipa <- c("EXPGS", "IMPGS", "PCEC", "GDP", "GPDI", "GCE")
for (i in 1:length(nipa)) {
  getSymbols(nipa[i], src = "FRED")
}

# Plot residuals from NIPA equation
((EXPGS - IMPGS + PCEC + GPDI + GCE) - GDP)^2
plot(((EXPGS - IMPGS + PCEC + GPDI + GCE) - GDP)^2)


# GET THE OTHER TWO COUNTRIES FROM EUROSTAT DATABASE

# Install packages
packages <- c("tidyverse", "rsdmx", "eurostat", "tbl2xts", 
              "tidyquant", "BCDating", "pwt10", "dplyr")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
library(knitr)
kable(tail(toc))

library(xts)
library(ecb)

# For the original data, see
# http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=tsdtr210

# GDP download
namq_10_gdp <- get_eurostat("namq_10_gdp",
  stringsAsFactors = FALSE
)

# Create vector with list of countries
nimacountries <- c("ES", "FR")

# Create a vector with codes for NIMA aggregates
nimaeu <- c("B1GQ", "P31_S14_S15", "P3_S13", "P51G", "P52", "P53", "P6", "P7")

# Create a vector with names for columns in matrix dataeu
colname <- c(
  "GDP", "C", "G", "I", "inv", "saldo", "X", "IM",
  "GDPb", "Cb", "Gb", "Ib", "invb", "saldob", "Xb", "IMb"
)

# Create matrix dataeu to fill with data from the 2 countries
dataeu <- matrix(NA, 89, length(nimacountries) * length(nimaeu))
colnames(dataeu) <- colname

# Set z to partition columns
z <- length(colname) / length(nimacountries)

# For cycle to fill dataeu with data from the countries
for (i in 1:length(nimacountries)) {
  for (s in 1:length(nimaeu)) {
    newdata <- namq_10_gdp %>%
      filter(geo == nimacountries[i]) %>%
      filter(time >= "2000-01-01") %>%
      filter(unit == "CP_MEUR") %>%
      filter(s_adj == "SCA") %>%
      filter(na_item == nimaeu[s])
    newdata$time <- convert_dates(newdata$time)
    newdataxts <- xts(newdata$values, newdata$time)
    dataeu[, (s + (z * (i - 1)))] <- newdataxts
  }
}

# Change Imports from positive to negative in both countries
dataeu[, z] <- dataeu[, z] * (-1)
dataeu[, length(colname)] <- dataeu[, length(colname)] * (-1)

# Convert dataeu to xts format
dataeu <- xts(dataeu, rev(newdata$time))

# Check NIMA identity for country 1

#Create empty vectors for sums of aggregates and for gdp
sums <- c()
gdp <- c()
#Fill them with the data using a for cycle
for (i in 1:nrow(dataeu)) {
  sums[i] <- sum(dataeu[i, -c(1, (z + 1):length(colname))])
  gdp[i] <- dataeu[i, 1]
}

#Plot the values
sum(dataeu[i, -c(1, z:length(colname))]) - dataeu[i, z]
dataeu[, -c(1, z:length(colname))]

#Plot the graphs
plot(sums, col = "green", type = "l")
lines(gdp, col = "red", type = "l")
plot(sums - gdp)


# Check NIMA identity for country 2
# Code is the same as before

sums2 <- c()
gdp2 <- c()
for (i in 1:nrow(dataeu)) {
  sums2[i] <- sum(dataeu[i, -c(1:(z + 1))])
  gdp2[i] <- dataeu[i, (z + 1)]
}

sum(dataeu[1, -c(1:(z + 1))]) - dataeu[1, length(colname)]

dataeu[i, (z + 1)]

dataeu[1, -c(1:(z + 1), length(colname))]

plot(sums2, col = "green", type = "l")
lines(gdp2, col = "red", type = "l")
plot(sums2 - gdp2)









#################
##### POINT 2####
#################

#Import the library
library(BCDating)

# Generate a matrix to fill with the log of the variables
data_bc <- matrix(NA, length(GDP), 4)

# Yaking logs
data_bc[, 1] <- log(GDP)
data_bc[, 2] <- log(IMPGS)
data_bc[, 3] <- log(GPDI)
data_bc[, 4] <- log(PCEC)

for (count in 1:4) {
  # create a time series with the variable[count]
  # setting the time index as year-quarter
  data_ts <- ts(data_bc[, count], start = c(1947, 1), frequency = 4)
  # applied the BBQ method
  bc_US <- BBQ(data_ts, name = count)
  # plot the results
  summary(bc_US)
  plot(bc_US, data_ts)
}




#################
##### POINT 3#####
#################

#Load the necessary libraries

library(pwt10)
library(dplyr)

#Download penn database from the web
data("pwt10.0")
penn <- pwt10.0
rm(pwt10.0)

#List of the years we are interested in (they are 10 years apart)
list <- c("1950", "1960", "1970", "1980", "1990", "2000", "2010")


#Filter Penn data for the USA
uspenn <- penn %>%
  filter(country == "United States of America")
#Select only labour share of total income and the year
uslabsh <- uspenn %>%
  select(c(year, labsh))

#Code is the same for the other 2 countries

espenn <- penn %>%
  filter(country == "Spain")
eslabsh <- espenn %>%
  select(c(year, labsh))

frpenn <- penn %>%
  filter(country == "France")
frlabsh <- frpenn %>%
  select(c(year, labsh))

#Filter each time series only for the years we selected
#then repeat for each country

usfilt <- uslabsh %>%
  filter(year %in% list)
plot(usfilt$year, usfilt$labsh,
  ylim = c(0, 1),
  type = "b",
  ylab = "Labour Share of Total Income",
  xlab = "Year", main = "US"
)

esfilt <- eslabsh %>%
  filter(year %in% list)
plot(usfilt$year, esfilt$labsh,
  ylim = c(0, 1),
  type = "b",
  ylab = "Labour Share of Total Income",
  xlab = "Year", main = "SPAIN"
)


frfilt <- frlabsh %>%
  filter(year %in% list)
plot(frfilt$year, frfilt$labsh,
  ylim = c(0, 1),
  type = "b",
  ylab = "Labour Share of Total Income",
  xlab = "Year", main = "FRANCE"
)

#Create the list of countries we will use

countries <- c(
  "United States of America", "France", "Germany", "Japan",
  "Canada", "China",
  "Italy", "Netherlands", "United Kingdom", "Spain"
)
char <- c("country", "rgdpna", "rtfpna", "rnna", "avh", "emp")

#Select years between 1960 and 2000

penn_years <- penn %>%
  filter((year >= 1960) & (year <= 2000) & (country %in% countries))
penn_years <- select(penn_years, char)

#Create function ration to...

ratio <- function(x) {
  x <- log(x)
  out <- c(x[1])
  for (count in 2:length(x)) {
    out <- append(out, (x[count] - x[count - 1]) / x[count - 1])
  }
  return(out)
}

cap <- function(x) {
  L <- x[, 2] * x[, 3]
  k <- x[, 1] / L
  alpha <- 0.3
  out <- ratio(k) * alpha
  return(out)
}


penn_years$gdp_ratio <- ratio(penn_years[, 2])
penn_years$tfp_ratio <- ratio(penn_years[, 3])
penn_years$cap_deepe <- cap(penn_years[, c("rnna", "avh", "emp")])
penn_years$tfp_share <- penn_years$tfp_ratio / penn_years$gdp_ratio
penn_years$cap_share <- penn_years$cap_deepe / penn_years$gdp_ratio
View(penn_years[, c("country", "gdp_ratio", "tfp_ratio", 
                    "cap_deepe", "tfp_share", "cap_share")])

#################
##### POINT 4#####
#################


#Download database from Favero's book
favero <- read.csv("MRW.csv", sep = ";", dec = ",")
#Delete the ID column (it is useless for our purposes)
favero$ID <- NULL


#Create a dataframe with only the variables we are interested in
replica <- data.frame(favero$YL85, favero$N6085, favero$IY)
names(replica) <- c("Ypc", "PopGrowth", "IonY")

#Log-transform our variable of interest and add g+delta to n
replica$Ypc <- log(replica$Ypc)
replica$PopGrowth <- log(((replica$PopGrowth) / 100) + 0.05)
replica$IonY <- log((replica$IonY) / 100)
names(replica)[2] <- "ngdelta"


#Replicate regression results
reg <- lm(Ypc ~ IonY + ngdelta, data = replica)

#Print them using stargazer function
library(stargazer)
stargazer(reg, type = "text")



#Do hypothesis testing with the two provided restrictions 
#(which were respectively b1=-b2 ; b1=0.5 & b2=-0.5)

library(car)
linearHypothesis(reg, c("IonY-ngdelta=0"), test = "F")
linearHypothesis(reg, c("IonY=0.5", "ngdelta=-0.5"), test = "F")



#Perform bootstrapping using Boot from the package car

Boot(reg)


#Load library boot

library(boot)

#Create bs function to get coefficients from the regression

bs = function(formula, data, indices){
  d = data[indices,]
  fit=lm(formula, data=d)
  return(coef(fit))
}

#Create var results with function boot on the regression

results = boot(replica, statistic=bs, R=1000, formula= Ypc ~ IonY + ngdelta)

#Print the results

results



#Add schooling the replica database

replica$school=log((favero$SCHOOL)/100)



#Replicate the augmented Solow model

reg2 = lm(Ypc ~ IonY + ngdelta + school, data = replica)

#Print results using the Stargazer function

stargazer(reg2, type = "text")













