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


rm(list=ls())
graphics.off()

options(digits=4)

packages <- c("tidyverse","rsdmx","eurostat","tbl2xts","tidyquant")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
library(knitr)
kable(tail(toc))

# For the original data, see
# http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=tsdtr210
id <- search_eurostat("Modal split of passenger transport",
                      type = "table")$code[1]

# download data
dat <- get_eurostat(id, time_format = "num")

kable(head(dat))

label_eurostat_vars(names(datl))


# GDP download
namq_10_gdp <- get_eurostat("namq_10_gdp", 
                            stringsAsFactors = FALSE)

str(namq_10_gdp)

# extract Spain GDP data
nimacountries=c("ES","IT")
nimaeu=c("B1GQ","P3_S13","P31_S14_S15","P52","P6","P7")

dataeu=matrix()

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
    dataeu[i,s]=newdataxts
  }
  
}

GDPES<-namq_10_gdp %>% 
  filter(geo == 'ES') %>% 
  filter(time >= '2000-01-01') %>% 
  filter(unit == 'CP_MEUR') %>% 
  filter(s_adj == 'SCA') %>% 
  filter(na_item == 'B1GQ') 

str(GDPES)

GDPES$time<-convert_dates(GDPES$time)
GDPESxts<-xts(GDPES$values,GDPES$time)
str(GDPESxts)
tail(GDPESxts)























