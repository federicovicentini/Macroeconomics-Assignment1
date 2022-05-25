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

plot(GDP==EXPGS-IMPGS+PCEC+GPDI+GCE)
GDP==EXPGS-IMPGS+PCEC+GPDI+GCE

((EXPGS-IMPGS+PCEC+GPDI+GCE)-GDP)^2

plot(((EXPGS-IMPGS+PCEC+GPDI+GCE)-GDP)^2)

toc <- get_eurostat_toc()

nipaeu=c("t_nama_10_ma")
for (i in 1:length(nipa)) {
  get_eurostat(nipaeu[i])
}

#eudata=get_eurostat("namq_10_gdp"))
#eudata=
#eudataes=eudata[eudata$geo=="ES"]





options(digits=4)
#install packages
packages <- c("tidyverse","rsdmx","eurostat","tbl2xts","tidyquant")
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
id <- search_eurostat("Modal split of passenger transport",
                      type = "table")$code[1]

# download data
dat <- get_eurostat(id, time_format = "num")

kable(head(dat)) #\c

datl <- label_eurostat(dat) 
kable(head(datl))

label_eurostat_vars(names(datl))

#\c
# GDP download
namq_10_gdp <- get_eurostat("namq_10_gdp", 
                            stringsAsFactors = FALSE)

str(namq_10_gdp)


nimacountries=c("ES","FR") # list of countries 
nimaeu=c("B1GQ","P31_S14_S15","P3_S13","P51G","P52","P53","P6","P7")
colname=c("GDP","C","G","I","inv","saldo","X","IM",
          "GDPb","Cb","Gb","Ib","invb","saldob","Xb","IMb")


dataeu=matrix(NA,89,length(nimacountries)*length(nimaeu))
colnames(dataeu)=colname

z=length(colname)/length(nimacountries) 
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

dataeu[,z]=dataeu[,z]*(-1)
dataeu[,length(colname)]=dataeu[,length(colname)]*(-1)

head(newdata$time)

dataeu<-xts(dataeu,rev(newdata$time))

sums=c()
gdp=c()
for (i in 1:nrow(dataeu)) {
  sums[i]=sum(dataeu[i,-c(1,(z+1):length(colname))])
  gdp[i]=dataeu[i,1]
}


sum(dataeu[i,-c(1,z:length(colname))])-dataeu[i,z]

dataeu[,-c(1,z:length(colname))]

sums
gdp
plot(sums, col="green", type="l")
lines(gdp, col="red", type="l")


plot(sums-gdp)

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





