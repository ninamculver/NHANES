install.packages(c("RNetCDF", "ncdf4", "fields", "maptools"))
library(ncdf4)
library(RNetCDF)

setwd("H:/Climate Change Data to Clean/Historical Data")


#----------------------------------------------#
#-------------- TEMPERATURE DATA --------------#
#----------------------------------------------#

#-----------------------------------------------#
#---------- Importing the NetCDF Data ----------#
#-----------------------------------------------#

temporary.tavg = 'Extraction_Tavg.nc'
temporary.tavg = open.nc(temporary.tavg)
temporary.tavg = read.nc(temporary.tavg)

temporary.tas = 'Extraction_tas.nc'
temporary.tas = open.nc(temporary.tas)
temporary.tas = read.nc(temporary.tas)

#------------------------------------------------#
#------------ Creating the Variables ------------#
#------------------------------------------------#

temp.longitude = temporary.tavg$longitude
temp.latitude = temporary.tavg$latitude
temp.time = temporary.tavg$time
tavg = temporary.tavg$Tavg
tas = temporary.tas$tas

#-----------------------------------------------#
#-------------- Creating Database --------------#
#-----------------------------------------------#

data = matrix(,nrow=length(temp.longitude)*length(temp.latitude)*length(temp.time),ncol = 38)
for(x in 1:length(temp.time)){ #time
  for(y in 1:length(temp.longitude)){ #longitude
    for(z in 1:length(temp.latitude)){ #latitude
      i = ((x-1)*length(temp.longitude)*length(temp.latitude)) + ((z-1)*length(temp.longitude)) + y
      data[i,1] = tavg[y,z,x]
      data[i,2:35] = tas[y,z,x,1:34]
      data[i,36] = temp.latitude[z]
      data[i,37] = temp.longitude[y]
      data[i,38] = temp.time[x]
    }
  }
}
colnames(data) = list('orig','acc101', 'acc131', 'bcc111', 'bcc11m1'
                      , 'cane', 'ccsm41', 'cesbgc', 'cesm1', 'ccmcc',
                      'cnrm', 'csiro', 'fgoals', 'fio', 'gfcm', 'gfesm2g',
                      'gfesm2m', 'gis2h', 'gis2r1', 'gis2rcc', 'hagao', 'hagcc',
                      'hages', 'inm', 'ipsl5alr', 'isplmr', 'ipsl5blr', 'miroc1',
                      'mirochem', 'miroc5', 'mpilr', 'mpimr', 'mri',
                      'noresmm', 'noresmme', 'lat', 'long', 't')

temperature.data = data.frame(data)
temperature.data = na.omit(temperature.data)

#------------------------------------------#
#----------------- Months -----------------#
#------------------------------------------#

times = as.factor(temperature.data$t)
timelevels = levels(times)

month = rep(1,nrow(temperature.data))
temperature.data = cbind(temperature.data, month)
monthlevel = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                "October", "November", "December")

january = 1
for (i in 0:49){
  if (i==0){
    january[1] = timelevels[1]}
  else{
    january[i+1] = timelevels[12*i+1]}
}

for (i in 1:nrow(temperature.data)){
  for (j in 1:length(january)){
    if (temperature.data$t[i] == january[j]){
      temperature.data$month[i] = "January"
    }
  }
}

february = 1
for (i in 0:49){
  if (i==0){
    february[1] = timelevels[2]}
  else{
    february[i+1] = timelevels[12*i+2]}
}

for (i in 1:nrow(temperature.data)){
  for (j in 1:length(february)){
    if (temperature.data$t[i] == february[j]){
      temperature.data$month[i] = "February"
    }
  }
}

march = 1
for (i in 0:49){
  if (i==0){
    march[1] = timelevels[3]}
  else{
    march[i+1] = timelevels[12*i+3]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(march)){
    if (temperature.data$t[i] == march[j]){
      temperature.data$month[i] = "March"
    }
  }
}

april= 1
for (i in 0:49){
  if (i==0){
    april[1] = timelevels[4]}
  else{
    april[i+1] = timelevels[12*i+4]}
}

for (i in 1:nrow(temperature.data)){
  for (j in 1:length(april)){
    if (temperature.data$t[i] == april[j]){
      temperature.data$month[i] = "April"
    }
  }
}

may= 1
for (i in 0:49){
  if (i==0){
    may[1] = timelevels[5]}
  else{
    may[i+1] = timelevels[12*i+5]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(may)){
    if (temperature.data$t[i] == may[j]){
      temperature.data$month[i] = "May"
    }
  }
}

june= 1
for (i in 0:49){
  if (i==0){
    june[1] = timelevels[6]}
  else{
    june[i+1] = timelevels[12*i+6]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(june)){
    if (temperature.data$t[i] == june[j]){
      temperature.data$month[i] = "June"
    }
  }
}
july= 1
for (i in 0:49){
  if (i==0){
    july[1] = timelevels[7]}
  else{
    july[i+1] = timelevels[12*i+7]}
}

for (i in 1:nrow(temperature.data)){
  for (j in 1:length(july)){
    if (temperature.data$t[i] == july[j]){
      temperature.data$month[i] = "July"
    }
  }
}

aug= 1
for (i in 0:49){
  if (i==0){
    aug[1] = timelevels[8]}
  else{
    aug[i+1] = timelevels[12*i+8]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(aug)){
    if (temperature.data$t[i] == aug[j]){
      temperature.data$month[i] = "August"
    }
  }
}

sep= 1
for (i in 0:49){
  if (i==0){
    sep[1] = timelevels[9]}
  else{
    sep[i+1] = timelevels[12*i+9]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(sep)){
    if (temperature.data$t[i] == sep[j]){
      temperature.data$month[i] = "September"
    }
  }
}

oct= 1
for (i in 0:49){
  if (i==0){
    oct[1] = timelevels[10]}
  else{
    oct[i+1] = timelevels[12*i+10]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(oct)){
    if (temperature.data$t[i] == oct[j]){
      temperature.data$month[i] = "October"
    }
  }
}

nov= 1
for (i in 0:49){
  if (i==0){
    nov[1] = timelevels[11]}
  else{
    nov[i+1] = timelevels[12*i+11]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(nov)){
    if (temperature.data$t[i] == nov[j]){
      temperature.data$month[i] = "November"
    }
  }
}

dec= 1
for (i in 0:49){
  if (i==0){
    dec[1] = timelevels[12]}
  else{
    dec[i+1] = timelevels[12*i+12]}
}
for (i in 1:nrow(temperature.data)){
  for (j in 1:length(dec)){
    if (temperature.data$t[i] == dec[j]){
      temperature.data$month[i] = "December"
    }
  }
}

#-----------------------------------------#
#----------------- Years -----------------#
#-----------------------------------------#

years = seq(1950,1999,1)
year = rep(1,nrow(temperature.data))
temporaryseq = seq(0,49,1)

temperature.data = cbind(temperature.data, year)
for (i in 1:nrow(temperature.data)){
  for (j in 1:50){
    if (floor(as.numeric(temperature.data$t[i])/365.25) == temporaryseq[j]){
      temperature.data$year[i] = years[j]
    }
  }
}
write.csv(temperature.data, file = "Historical Temperature Data.csv", row.names = FALSE)


#------------------------------------------------#
#-------------- PRECIPITATION DATA --------------#
#------------------------------------------------#

#-----------------------------------------------#
#---------- Importing the NetCDF Data ----------#
#-----------------------------------------------#

temporary.prcp = 'Extraction_Prcp.nc'
temporary.prcp = open.nc(temporary.prcp)
temporary.prcp = read.nc(temporary.prcp)

temporary.pr = 'Extraction_pr.nc'
temporary.pr = open.nc(temporary.pr)
temporary.pr = read.nc(temporary.pr)

#------------------------------------------------#
#------------ Creating the Variables ------------#
#------------------------------------------------#

prcp.longitude = temporary.prcp$longitude
prcp.latitude = temporary.prcp$latitude
prcp.time = temporary.prcp$time
prcp = temporary.prcp$Prcp
pr = temporary.pr$pr
pr.projection = temporary.pr$Projection

#-----------------------------------------------#
#-------------- Creating Database --------------#
#-----------------------------------------------#

data = matrix(,nrow=length(prcp.longitude)*length(prcp.latitude)*length(prcp.time),ncol = 38)
for(x in 1:length(prcp.time)){ #time
  for(y in 1:length(prcp.longitude)){ #longitude
    for(z in 1:length(prcp.latitude)){ #latitude
      i = ((x-1)*length(prcp.longitude)*length(prcp.latitude)) + ((z-1)*length(prcp.longitude)) + y
      data[i,1] = prcp[y,z,x]
      data[i,2:35] = pr[y,z,x,1:34]
      data[i,36] = prcp.latitude[z]
      data[i,37] = prcp.longitude[y]
      data[i,38] = prcp.time[x]
    }
  }
}
colnames(data) = list('orig','acc101', 'acc131', 'bcc111', 'bcc11m1'
                      , 'cane', 'ccsm41', 'cesbgc', 'cesm1', 'ccmcc',
                      'cnrm', 'csiro', 'fgoals', 'fio', 'gfcm', 'gfesm2g',
                      'gfesm2m', 'gis2h', 'gis2r1', 'gis2rcc', 'hagao', 'hagcc',
                      'hages', 'inm', 'ipsl5alr', 'isplmr', 'ipsl5blr', 'miroc1',
                      'mirochem', 'miroc5', 'mpilr', 'mpimr', 'mri',
                      'noresmm', 'noresmme', 'lat', 'long', 't')

precipitation.data = data.frame(data)
precipitation.data = na.omit(precipitation.data)


#------------------------------------------#
#----------------- Months -----------------#
#------------------------------------------#


times = as.factor(precipitation.data$t)
timelevels = levels(times)

month = rep(1,nrow(precipitation.data))
precipitation.data = cbind(precipitation.data, month)
january = 1
for (i in 0:49){
  if (i==0){
    january[1] = timelevels[1]}
  else{
    january[i+1] = timelevels[12*i+1]}
}

for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(january)){
    if (precipitation.data$t[i] == january[j]){
      precipitation.data$month[i] = "January"
    }
  }
}

february = 1
for (i in 0:49){
  if (i==0){
    february[1] = timelevels[2]}
  else{
    february[i+1] = timelevels[12*i+2]}
}

for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(february)){
    if (precipitation.data$t[i] == february[j]){
      precipitation.data$month[i] = "February"
    }
  }
}

march = 1
for (i in 0:49){
  if (i==0){
    march[1] = timelevels[3]}
  else{
    march[i+1] = timelevels[12*i+3]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(march)){
    if (precipitation.data$t[i] == march[j]){
      precipitation.data$month[i] = "March"
    }
  }
}

april= 1
for (i in 0:49){
  if (i==0){
    april[1] = timelevels[4]}
  else{
    april[i+1] = timelevels[12*i+4]}
}

for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(april)){
    if (precipitation.data$t[i] == april[j]){
      precipitation.data$month[i] = "April"
    }
  }
}

may= 1
for (i in 0:49){
  if (i==0){
    may[1] = timelevels[5]}
  else{
    may[i+1] = timelevels[12*i+5]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(may)){
    if (precipitation.data$t[i] == may[j]){
      precipitation.data$month[i] = "May"
    }
  }
}

june= 1
for (i in 0:49){
  if (i==0){
    june[1] = timelevels[6]}
  else{
    june[i+1] = timelevels[12*i+6]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(june)){
    if (precipitation.data$t[i] == june[j]){
      precipitation.data$month[i] = "June"
    }
  }
}
july= 1
for (i in 0:49){
  if (i==0){
    july[1] = timelevels[7]}
  else{
    july[i+1] = timelevels[12*i+7]}
}

for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(july)){
    if (precipitation.data$t[i] == july[j]){
      precipitation.data$month[i] = "July"
    }
  }
}

aug= 1
for (i in 0:49){
  if (i==0){
    aug[1] = timelevels[8]}
  else{
    aug[i+1] = timelevels[12*i+8]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(aug)){
    if (precipitation.data$t[i] == aug[j]){
      precipitation.data$month[i] = "August"
    }
  }
}

sep= 1
for (i in 0:49){
  if (i==0){
    sep[1] = timelevels[9]}
  else{
    sep[i+1] = timelevels[12*i+9]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(sep)){
    if (precipitation.data$t[i] == sep[j]){
      precipitation.data$month[i] = "September"
    }
  }
}

oct= 1
for (i in 0:49){
  if (i==0){
    oct[1] = timelevels[10]}
  else{
    oct[i+1] = timelevels[12*i+10]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(oct)){
    if (precipitation.data$t[i] == oct[j]){
      precipitation.data$month[i] = "October"
    }
  }
}

nov= 1
for (i in 0:49){
  if (i==0){
    nov[1] = timelevels[11]}
  else{
    nov[i+1] = timelevels[12*i+11]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(nov)){
    if (precipitation.data$t[i] == nov[j]){
      precipitation.data$month[i] = "November"
    }
  }
}

dec= 1
for (i in 0:49){
  if (i==0){
    dec[1] = timelevels[12]}
  else{
    dec[i+1] = timelevels[12*i+12]}
}
for (i in 1:nrow(precipitation.data)){
  for (j in 1:length(dec)){
    if (precipitation.data$t[i] == dec[j]){
      precipitation.data$month[i] = "December"
    }
  }
}

#-----------------------------------------#
#----------------- Years -----------------#
#-----------------------------------------#

precipitation.data = cbind(precipitation.data,year)
for (i in 1:nrow(precipitation.data)){
  for (j in 1:50){
    if (floor(as.numeric(precipitation.data$t[i])/365.25) == temporaryseq[j]){
      precipitation.data$year[i] = years[j]
    }
  }
}

write.csv(precipitation.data, file = "Historical Precipitation Data.csv", row.names = FALSE)
