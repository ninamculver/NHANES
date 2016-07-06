install.packages(c("RNetCDF", "ncdf4", "fields", "maptools"))
library(ncdf4)
library(RNetCDF)

setwd("C:/Users/000680776/Documents/KV Total Research/Future Data")

#-----------------------------------------------#
#---------- Importing the NetCDF Data ----------#
#-----------------------------------------------#

temporary.prcp = "Extraction_Prcp.nc"
temporary.prcp = open.nc(temporary.prcp)
future.prcp.ncdf = read.nc(temporary.prcp)

temporary.pr = "Extraction_pr.nc"
temporary.pr = open.nc(temporary.pr)
future.pr.ncdf = read.nc(temporary.pr)

temporary.tas = "Extraction_tas.nc"
temporary.tas = open.nc(temporary.tas)
future.tas.ncdf = read.nc(temporary.tas)

temporary.tavg = "Extraction_Tavg.nc"
temporary.tavg = open.nc(temporary.tavg)
future.tavg.ncdf = read.nc(temporary.tavg)

#------------------------------------------------#
#------------ Creating the Variables ------------#
#------------------------------------------------#

future.tas = future.tas.ncdf$tas
future.pr = future.pr.ncdf$pr
future.time = future.tas.ncdf$time
future.long = future.tas.ncdf$longitude
future.lat = future.tas.ncdf$latitude

#-----------------------------------------------#
#---------- Creating Temperature Data ----------#
#-----------------------------------------------#

data = matrix(,nrow=length(future.long)*length(future.lat)*length(future.time),ncol = 37)

for(x in 1:length(future.long)){ #longitude
  for(y in 1:length(future.lat)){ #latitude
    for(z in 1:length(future.time)){ #time
      i = ((x-1)*length(future.lat)*length(future.time)) + ((z-1)*length(future.lat)) + y
      data[i,1:34] = future.tas[x,y,z,1:34]
      data[i,35] = future.lat[y]
      data[i,36] = future.long[x]
      data[i,37] = future.time[z]
    }
  }
}
colnames(data) = list('acc101', 'acc131', 'bcc111', 'bcc11m1'
                      , 'cane', 'ccsm41', 'cesbgc', 'cesm1', 'ccmcc',
                      'cnrm', 'csiro', 'fgoals', 'fio', 'gfcm', 'gfesm2g',
                      'gfesm2m', 'gis2h', 'gis2r1', 'gis2rcc', 'hagao', 'hagcc',
                      'hages', 'inm', 'ipsl5alr', 'isplmr', 'ipsl5blr', 'miroc1',
                      'mirochem', 'miroc5', 'mpilr', 'mpimr', 'mri',
                      'noresmm', 'noresmme', 'lat', 'long', 't')

temp.projections = data.frame(data)
temp.projections = na.omit(temp.projections)

#-----------------------------------------------#
#--------- Creating Precipitation Data ---------#
#-----------------------------------------------#

for(x in 1:length(future.long)){ #longitude
  for(y in 1:length(future.lat)){ #latitude
    for(z in 1:length(future.time)){ #time
      i = ((x-1)*length(future.lat)*length(future.time)) + ((z-1)*length(future.lat)) + y
      data[i,1:34] = future.pr[x,y,z,1:34]
      data[i,35] = future.lat[y]
      data[i,36] = future.long[x]
      data[i,37] = future.time[z]
    }
  }
}
colnames(data) = list('acc101', 'acc131', 'bcc111', 'bcc11m1'
                      , 'cane', 'ccsm41', 'cesbgc', 'cesm1', 'ccmcc',
                      'cnrm', 'csiro', 'fgoals', 'fio', 'gfcm', 'gfesm2g',
                      'gfesm2m', 'gis2h', 'gis2r1', 'gis2rcc', 'hagao', 'hagcc',
                      'hages', 'inm', 'ipsl5alr', 'isplmr', 'ipsl5blr', 'miroc1',
                      'mirochem', 'miroc5', 'mpilr', 'mpimr', 'mri',
                      'noresmm', 'noresmme', 'lat', 'long', 't')

prcp.projections = data.frame(data)
prcp.projections = na.omit(temp.projections)

#------------------------------------------#
#----------------- Months -----------------#
#------------------------------------------#

levels(temp.projections)
levels(temp.projections$t)
is.factor(temp.projections$t)
times <- as.factor(temp.projections$t)
is.factor(times)
timelevels <- levels(times)

future.month <- rep(1,nrow(temp.projections))
temp.projections <- cbind(temp.projections, future.month)
monthlevel <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                "October", "November", "December")

january <- 1
for (i in 0:100){
  if (i==0){
    january[1] <- timelevels[1]}
  else{
    january[i+1] <- timelevels[12*i+1]}
}

#placing January in data:
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(january)){
    if (temp.projections$t[i] == january[j]){
      temp.projections$month[i] <- "January"
    }
  }
}

february <- 1
for (i in 0:100){
  if (i==0){
    february[1] <- timelevels[2]}
  else{
    february[i+1] <- timelevels[12*i+2]}
}

#placing February in data:
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(february)){
    if (temp.projections$t[i] == february[j]){
      temp.projections$month[i] <- "February"
    }
  }
}

march <- 1
for (i in 0:49){
  if (i==0){
    march[1] <- timelevels[3]}
  else{
    march[i+1] <- timelevels[12*i+3]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(march)){
    if (temp.projections$t[i] == march[j]){
      temp.projections$month[i] <- "March"
    }
  }
}

april<- 1
for (i in 0:49){
  if (i==0){
    april[1] <- timelevels[4]}
  else{
    april[i+1] <- timelevels[12*i+4]}
}

for (i in 1:nrow(temp.projections)){
  for (j in 1:length(april)){
    if (temp.projections$t[i] == april[j]){
      temp.projections$month[i] <- "April"
    }
  }
}

may<- 1
for (i in 0:49){
  if (i==0){
    may[1] <- timelevels[5]}
  else{
    may[i+1] <- timelevels[12*i+5]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(may)){
    if (temp.projections$t[i] == may[j]){
      temp.projections$month[i] <- "May"
    }
  }
}

june<- 1
for (i in 0:49){
  if (i==0){
    june[1] <- timelevels[6]}
  else{
    june[i+1] <- timelevels[12*i+6]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(june)){
    if (temp.projections$t[i] == june[j]){
      temp.projections$month[i] <- "June"
    }
  }
}
july<- 1
for (i in 0:49){
  if (i==0){
    july[1] <- timelevels[7]}
  else{
    july[i+1] <- timelevels[12*i+7]}
}

for (i in 1:nrow(temp.projections)){
  for (j in 1:length(july)){
    if (temp.projections$t[i] == july[j]){
      temp.projections$month[i] <- "July"
    }
  }
}

aug<- 1
for (i in 0:49){
  if (i==0){
    aug[1] <- timelevels[8]}
  else{
    aug[i+1] <- timelevels[12*i+8]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(aug)){
    if (temp.projections$t[i] == aug[j]){
      temp.projections$month[i] <- "August"
    }
  }
}

sep<- 1
for (i in 0:49){
  if (i==0){
    sep[1] <- timelevels[9]}
  else{
    sep[i+1] <- timelevels[12*i+9]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(sep)){
    if (temp.projections$t[i] == sep[j]){
      temp.projections$month[i] <- "September"
    }
  }
}

oct<- 1
for (i in 0:49){
  if (i==0){
    oct[1] <- timelevels[10]}
  else{
    oct[i+1] <- timelevels[12*i+10]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(oct)){
    if (temp.projections$t[i] == oct[j]){
      temp.projections$month[i] <- "October"
    }
  }
}

nov<- 1
for (i in 0:49){
  if (i==0){
    nov[1] <- timelevels[11]}
  else{
    nov[i+1] <- timelevels[12*i+11]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(nov)){
    if (temp.projections$t[i] == nov[j]){
      temp.projections$month[i] <- "November"
    }
  }
}

dec<- 1
for (i in 0:49){
  if (i==0){
    dec[1] <- timelevels[12]}
  else{
    dec[i+1] <- timelevels[12*i+12]}
}
for (i in 1:nrow(temp.projections)){
  for (j in 1:length(dec)){
    if (temp.projections$t[i] == dec[j]){
      temp.projections$month[i] <- "December"
    }
  }
}

#-----------------------------------------#
#----------------- Years -----------------#
#-----------------------------------------#

years = seq(2000,2099,1)
year = rep(1,nrow(temp.projections))
temp.projections = cbind(temp.projections, year)

trial2 <- seq(0,99,1)
floor(as.numeric(timelevels)/365.25)

for (i in 1:nrow(temp.projections)){
  for (j in 1:100){
    if (floor(as.numeric(temp.projections$t[i])/365.25) == trial2[j]){
      temp.projections$year[i] = years[j]
    }
  }
}


write.csv(temp.projections, file = "tempProjections.csv", row.names = FALSE)


