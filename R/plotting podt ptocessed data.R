# go to the output directory of hydrolight
setwd("C:/HE60/output/HydroLight/excel")
files <- list.files(pattern = "^M.*\\.xlsx$")
library(readxl)
filelist<- lapply(files,function(x)as.data.frame(read_xlsx(x,sheet = "Rrs",skip = 3)[,c(1,2)]))
library(stringr)
names(filelist)<- gsub("^M|\\.xlsx$", "", files)
library(plyr)
Rrs_dat<- ldply(filelist)
names(Rrs_dat)<- c("station","wavelength","rrs")
Rrs_dat$rrs<- as.numeric(Rrs_dat$rrs)
Rrs_dat<- Rrs_dat[!is.nan(Rrs_dat$rrs),]
library(ggplot2)
ggplot(Rrs_dat, aes(x = wavelength, y = rrs, color = station)) +
  geom_line(size = 1)

#plot(Rrs_dat$wavelength,Rrs_dat$rrs,




