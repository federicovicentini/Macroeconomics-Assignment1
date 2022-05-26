#Federico Vicentini
#23/05/2022

#Macroeconomics - Assignment no.1

#################
#####POINT 1#####
#################

#Clear the variables
rm(list=ls()) 

#Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load packages
library(quantmod)
library(eurostat)

#Get Data from Fred website
nipa=c("EXPGS","IMPGS","PCEC","GDP","GPDI","GCE")
for (i in 1:length(nipa)) {
  getSymbols(nipa[i],src='FRED')
}

#Plot residuals from NIPA equation
((EXPGS-IMPGS+PCEC+GPDI+GCE)-GDP)^2
plot(((EXPGS-IMPGS+PCEC+GPDI+GCE)-GDP)^2)


# GET THE OTHER TWO COUNTRIES FROM EUROSTAT DATABASE

# Install packages
packages <- c("tidyverse","rsdmx","eurostat","tbl2xts","tidyquant","BCDating","pwt10","dplyr")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
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
                            stringsAsFactors = FALSE)

# Create vector with list of countries
nimacountries=c("ES","FR")

# Create a vector with codes for NIMA aggregates
nimaeu=c("B1GQ","P31_S14_S15","P3_S13","P51G","P52","P53","P6","P7")

# Create a vector with names for columns in matrix dataeu
colname=c("GDP","C","G","I","inv","saldo","X","IM",
          "GDPb","Cb","Gb","Ib","invb","saldob","Xb","IMb")

# Create matrix dataeu to fill with data from the 2 countries
dataeu=matrix(NA,89,length(nimacountries)*length(nimaeu))
colnames(dataeu)=colname

# Set z to partition columns
z=length(colname)/length(nimacountries) 

# For cycle to fill dataeu with data from the countries
for (i in 1:length(nimacountries)) {
  for (s in 1:length(nimaeu)) {
    newdata<-namq_10_gdp %>% 
      filter(geo == nimacountries[i]) %>% 
      filter(time >= '2000-01-01') %>% 
      filter(unit == 'CP_MEUR') %>% 
      filter(s_adj == 'SCA') %>% 
      filter(na_item == nimaeu[s]) 
    newdata$time<-convert_dates(newdata$time)
    newdataxts<-xts(newdata$values,newdata$time)
    dataeu[,(s+(z*(i-1)))]=newdataxts
  }
}

#Change Imports from positive to negative in both countries
dataeu[,z]=dataeu[,z]*(-1)
dataeu[,length(colname)]=dataeu[,length(colname)]*(-1)

#Convert dataeu to xts format
dataeu<-xts(dataeu,rev(newdata$time))

#Check NIMA identity for country 1
sums=c()
gdp=c()
for (i in 1:nrow(dataeu)) {
  sums[i]=sum(dataeu[i,-c(1,(z+1):length(colname))])
  gdp[i]=dataeu[i,1]
}

sum(dataeu[i,-c(1,z:length(colname))])-dataeu[i,z]
dataeu[,-c(1,z:length(colname))]

plot(sums, col="green", type="l")
lines(gdp, col="red", type="l")
plot(sums-gdp)


#Check NIMA identity for country 2
sums2=c()
gdp2=c()
for (i in 1:nrow(dataeu)) {
  sums2[i]=sum(dataeu[i,-c(1:(z+1))])
  gdp2[i]=dataeu[i,(z+1)]
}

sum(dataeu[1,-c(1:(z+1))])-dataeu[1,length(colname)]

dataeu[i,(z+1)]

dataeu[1,-c(1:(z+1),length(colname))]

plot(sums2, col="green", type="l")
lines(gdp2, col="red", type="l")
plot(sums2-gdp2)





plot(sums-gdp) #plot the residual to check if Y=C+G+I





#################
#####POINT 2#####
#################


library(BCDating) #import

data_bc<-matrix(NA,length(GDP),4) #generate a matrix to fill with the log of the variables
#taking logs
data_bc[,1]<-log(GDP)
data_bc[,2]<-log(IMPGS)
data_bc[,3]<-log(GPDI)
data_bc[,4]<-log(PCEC)

for (count in 1:4) {
  #create a time series with the variable[count]
  #setting the time index as year-quarter
  data_ts<-ts(data_bc[,count],start = c(1947, 1), frequency = 4) 
  #applied the BBQ method 
  bc_US<-BBQ(data_ts,name= count)
  #plot the results
  summary(bc_US)
  plot(bc_US,data_ts)
}




#################
#####POINT 3#####
#################

library(pwt10)
library(dplyr)
data("pwt10.0")
penn=pwt10.0
rm(pwt10.0)



uspenn<-penn %>% 
  filter(country == "United States of America")
uslabsh=uspenn %>%
  select(c(year,labsh))

espenn<-penn %>% 
  filter(country == "Spain")
eslabsh=espenn %>%
  select(c(year,labsh))

frpenn<-penn %>% 
  filter(country == "France")
frlabsh=frpenn %>%
  select(c(year,labsh))


usfilt=uslabsh %>%
  filter(year==c("1950","1960","1970"))
plot(usfilt$year,usfilt$labsh, ylim=c(0,1),
     type="b",
     ylab="Labour Share of Total Income",
     xlab="Year", main="US")

esfilt=eslabsh %>%
  filter(year==c("1950","1960","1970"))
plot(usfilt$year,esfilt$labsh, ylim=c(0,1),
     type="b",
     ylab="Labour Share of Total Income",
     xlab="Year", main="SPAIN")


frfilt=frlabsh %>%
  filter(year==c("1950","1960","1970"))
plot(frfilt$year,frfilt$labsh, ylim=c(0,1),
     type="b",
     ylab="Labour Share of Total Income",
     xlab="Year", main="FRANCE")







#################
#####POINT 4#####
#################




























