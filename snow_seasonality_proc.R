#Snow Seasonality Descriptors
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(psych)

#open folder
setwd("~/Desktop/modis_data/MODISSnowSeasonality")
dataBaseDir <- c("~/Desktop/")
lstDir <- paste(dataBaseDir,c("modis_data/MODISSnowSeasonality/"),sep="")

#Clip raster using shape file
#source: https://stat.ethz.ch/pipermail/r-sig-geo/2013-July/018912.html
clip<-function(raster,shape)
{
  cropped<-crop(raster,shape)
  cells<-rasterize(shape,cropped)
  cropped*cells
}

#in folder, files names are only the ones that end in .tif
fileNames <- list.files(pattern = ".tif")

#create data frames
#first snow day full snow season
fsSnow = data.frame("2001"=rep(0, 5), "2002"=rep(0,5), "2003"=rep(0,5), "2004"=rep(0,5), "2005"=rep(0,5),
                    "2006"=rep(0,5), "2007"=rep(0,5), "2008"=rep(0,5), "2009"=rep(0,5), "2010"=rep(0,5),
                    "2011"=rep(0,5), "2012"=rep(0,5), "2013"=rep(0,5),"2014"=rep(0,5), "2015"=rep(0,5), 
                    "2016"=rep(0,5));
#last snow day full snow season
lsSnow = data.frame("2001"=rep(0, 5), "2002"=rep(0,5), "2003"=rep(0,5), "2004"=rep(0,5), "2005"=rep(0,5),
                    "2006"=rep(0,5), "2007"=rep(0,5), "2008"=rep(0,5), "2009"=rep(0,5), "2010"=rep(0,5),
                    "2011"=rep(0,5), "2012"=rep(0,5), "2013"=rep(0,5),"2014"=rep(0,5), "2015"=rep(0,5),
                    "2016"=rep(0,5));
#first snow day continuous snow season
fsSnowCont = data.frame("2001"=rep(0, 5), "2002"=rep(0,5), "2003"=rep(0,5), "2004"=rep(0,5), "2005"=rep(0,5),
                        "2006"=rep(0,5), "2007"=rep(0,5), "2008"=rep(0,5), "2009"=rep(0,5), "2010"=rep(0,5), 
                        "2011"=rep(0,5), "2012"=rep(0,5), "2013"=rep(0,5),"2014"=rep(0,5), "2015"=rep(0,5), 
                        "2016"=rep(0,5));
#last snow day continuous snow season
lsSnowCont = data.frame("2001"=rep(0, 5), "2002"=rep(0,5), "2003"=rep(0,5), "2004"=rep(0,5), "2005"=rep(0,5),
                        "2006"=rep(0,5), "2007"=rep(0,5), "2008"=rep(0,5), "2009"=rep(0,5), "2010"=rep(0,5), 
                        "2011"=rep(0,5), "2012"=rep(0,5), "2013"=rep(0,5), "2014"=rep(0,5), "2015"=rep(0,5),
                        "2016"=rep(0,5));

#change row names of each data frame
row.names(fsSnow) <- c("min","max","median","mean","SD")
row.names(lsSnow) <- c("min","max","median","mean","SD")
row.names(fsSnowCont) <- c("min","max","median","mean","SD")
row.names(lsSnowCont) <- c("min","max","median","mean","SD")

#change column name of each data frame
colnames(fsSnow)<-c("2001","2002","2003","2004","2005","2006","2007","2008",
                     "2009","2010","2011","2012","2013","2014","2015","2016")
colnames(lsSnow)<-c("2001","2002","2003","2004","2005","2006","2007","2008",
                    "2009","2010","2011","2012","2013","2014","2015","2016")
colnames(fsSnowCont)<-c("2001","2002","2003","2004","2005","2006","2007","2008",
                    "2009","2010","2011","2012","2013","2014","2015","2016")
colnames(lsSnowCont)<-c("2001","2002","2003","2004","2005","2006","2007","2008",
                    "2009","2010","2011","2012","2013","2014","2015","2016")


#iterate through all of the files
for (i in 1:length(fileNames))
{
  savedFile = file.path('~/Desktop/modis_data/MODISSnowSeasonality/New', fileNames[i]);
  show(savedFile)
  
  #if the file already exists, don't clip it
  if (file.exists(savedFile)){
      NS_snow_depth <- stack(paste(savedFile,sep=""))
  }
  #else, crop it and continue
  else{
    
      #read stack
      snow_depth <- stack(paste(lstDir, fileNames[i],sep=""))
      
      #read Alaskan North Slope shp file
      NS <- readOGR(dsn="./LCC_Arctic_Alaska_Yukon/LCC_Arctic_Alaska_Yukon.shp",layer ="LCC_Arctic_Alaska_Yukon")
      arctic <-NS[NS$LCC_Name == "Arctic LCC",]
  
      #clip raster using shape file
      clip_snow_depth<-clip(raster = snow_depth, shape = arctic)
      NS_snow_depth <- clip_snow_depth
    
      #Write cropped raster to new file
      writeRaster(NS_snow_depth,savedFile, options="INTERLEAVE=BAND", overwrite=TRUE)
  } 

  #label plots
  labels <- c("first_snow_day", "last_snow_day", "fss_range", "longest_css_first_day", "longest_css_last_day", "longest_css_day_range", "snow_days", "no_snow_days", "css_segment_num", "mflag", "cloud_days", "tot_css_days")
  names(NS_snow_depth) <- labels[1:12]
  plot(NS_snow_depth)
  
  #populate data frame
  #first stack layer - first snow day full snow season
  extremaData <- NS_snow_depth[[1]]
  extremaData[extremaData == 0] <- NA
  fsSnow[,i] = c("Min" = extremaData@data@min, "Max" = extremaData@data@max,  
                 median(extremaData@data@values,na.rm=TRUE), 
                 round(mean(extremaData@data@values,na.rm=TRUE)),
                 sd(extremaData@data@values,na.rm=TRUE))
  #second stack layer - last snow day full snow season
  extremaData <- NS_snow_depth[[2]]
  extremaData[extremaData == 0] <- NA
  lsSnow[,i] = c("Min" = extremaData@data@min, "Max" = extremaData@data@max,  
                 median(extremaData@data@values,na.rm=TRUE), 
                 round(mean(extremaData@data@values,na.rm=TRUE)),
                 sd(extremaData@data@values,na.rm=TRUE))
  #third stack layer - first snow day continuous snow season
  extremaData <- NS_snow_depth[[3]]
  extremaData[extremaData == 0] <- NA
  fsSnowCont[,i] = c("Min" = extremaData@data@min, "Max" = extremaData@data@max, 
                     median(extremaData@data@values,na.rm=TRUE), 
                     round(mean(extremaData@data@values,na.rm=TRUE)),
                     sd(extremaData@data@values,na.rm=TRUE))
  #fourth stack layer - last snow day continuous snow season
  extremaData <- NS_snow_depth[[4]]
  extremaData[extremaData == 0] <- NA
  lsSnowCont[,i] = c("Min" = extremaData@data@min, "Max" = extremaData@data@max, 
                     median(extremaData@data@values,na.rm=TRUE), 
                     round(mean(extremaData@data@values,na.rm=TRUE)),
                     sd(extremaData@data@values,na.rm=TRUE))
  
  #check if it did anything
  show(i)
}

#Write dataframes to csv files
setwd("~/Desktop/modis_data/MODISSnowSeasonality/New/dataFrames/")
write.csv(fsSnow, file = "fsSnow.csv")
write.csv(lsSnow, file = "lsSnow.csv")
write.csv(fsSnowCont, file = "fsSnowCont.csv")
write.csv(lsSnowCont, file = "lsSnowCont.csv")



#-------------------------------------------------------------------------------------------------------------
#Analysis of Permafrost Data
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

library(sp)
library(rgdal)
library(raster)
library(rgeos)

#open folder
setwd("~/Desktop/modis_data/calm_sites/CALM_SitesData_Cleaned")
dataBaseDir <- c("~/Desktop/")
listDir <- paste(dataBaseDir,c("modis_data/calm_sites/CALM_SitesData_Cleaned/"),sep="")
boundDir <- paste(dataBaseDir,c("modis_data/calm_sites/CALM_SitesData_Cleaned//LCC_Arctic_Alaska_Yukon/"),sep="")

#Load North Slope shape file
NS <- readOGR(dsn=path.expand(paste(boundDir, c("LCC_Arctic_Alaska_Yukon.shp"),sep="")),layer ="LCC_Arctic_Alaska_Yukon")

#Load active layer shape file
activeIn <- shapefile(paste(listDir,c("CALM_SitesData_Cleaned.shp"),sep=""),stringsAsFactors=FALSE)

ntSlopeProj <-projection(NS)
activeAK <- spTransform(activeIn,ntSlopeProj)
activeAKInd <- !is.na(over(activeAK,as(NS, "SpatialPolygons")))

#Plot the North Slope
#plot(coordinates(activeAK),type="n")
#plot(NS,border="blue",add=TRUE)

#Create dataframe that has permafrost data for North Slope
ALDataFrame = data.frame("SiteCode"=rep(0, 42),"SiteName"=rep(0, 42),"Latitude"=rep(0, 42),"Longitude"=rep(0, 42),
                         "1990"=rep(0, 42),"1991"=rep(0, 42),"1992"=rep(0, 42),"1993"=rep(0, 42),"1994"=rep(0, 42),
                         "1995"=rep(0, 42),"1996"=rep(0, 42),"1997"=rep(0, 42),"1998"=rep(0, 42),"1999"=rep(0, 42),
                         "2000"=rep(0, 42),"2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                         "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42), "2009"=rep(0,42),
                         "2010"=rep(0,42),"2011"=rep(0,42), "2012"=rep(0,42), "2013"=rep(0,42), "2014"=rep(0,42),
                         "2015"=rep(0,42), "2016"=rep(0,42));
colnames(ALDataFrame)<-c("SiteCode","SiteName","Latitude","Longitude","1990","1991","1992","1993","1994","1995",
                         "1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006",
                         "2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")

#populate dataframe

ALDataFrame[,1:4] <- activeAK@data[activeAKInd,1:4]
ALDataFrame[,5:31] <- activeAK@data[activeAKInd,7:33]


# now plot active layer sites instide and outside alaska 
points(activeAK[!activeAKInd, ], pch=1, col="gray")
points(activeAK[activeAKInd, ], pch=16, col="red")

# there are strange, hidden characters in the data. 
activeAK$F19 <- as.integer(gsub("\xa0", "", activeAK$F19))
activeAK$test <- as.integer(gsub("<a0>", "", activeAK$F19))

setwd("~/Desktop/modis_data/MODISSnowSeasonality/New/dataFrames/")
write.csv(ALDataFrame, file = "ALData.csv")


#-------------------------------------------------------------------------------------------------------------
#Finds snow extent where active layer point data is
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

#first snow day (full) median snow depth at active layer depth locations
fsSnowAtLoc = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                         "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                         "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                         "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(fsSnowAtLoc) <- c("2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008",
                           "2009", "2010", "2011", "2012",
                           "2013","2014", "2015", "2016")

row.names(fsSnowAtLoc) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])
#last snow day (full) median snow depth at active layer depth locations
lsSnowAtLoc = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                         "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                         "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                         "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(lsSnowAtLoc) <- c("2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008",
                           "2009", "2010", "2011", "2012",
                           "2013","2014", "2015", "2016")
row.names(lsSnowAtLoc) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

#first snow day (continuous) median snow depth at active layer depth locations
fsSnowContAtLoc = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                             "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                             "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                             "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(fsSnowContAtLoc) <- c("2001", "2002", "2003", "2004",
                               "2005", "2006", "2007", "2008",
                               "2009", "2010", "2011", "2012",
                               "2013","2014", "2015", "2016")
row.names(fsSnowContAtLoc) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])
#last snow day (continuous) median snow depth at active layer depth locations
lsSnowContAtLoc = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                             "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                             "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                             "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(lsSnowContAtLoc) <- c("2001", "2002", "2003", "2004",
                               "2005", "2006", "2007", "2008",
                               "2009", "2010", "2011", "2012",
                               "2013","2014", "2015", "2016")
row.names(lsSnowContAtLoc) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

#duration of snow period (continuous) median snow depth at active layer depth locations
durContSnowPer = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                            "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                            "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                            "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(durContSnowPer) <- c("2001", "2002", "2003", "2004",
                              "2005", "2006", "2007", "2008",
                              "2009", "2010", "2011", "2012",
                              "2013","2014", "2015", "2016")
row.names(durContSnowPer) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])


buf <- c(3)

for (i in 1:length(fileNames)){
  
  savedFile = file.path('~/Desktop/modis_data/MODISSnowSeasonality/New', fileNames[i]);
  NS_snow_depth <- stack(paste(savedFile,sep=""))
  
  layer1 <- NS_snow_depth[[1]]
  layer2 <- NS_snow_depth[[2]]
  layer3 <- NS_snow_depth[[4]] #3
  layer4 <- NS_snow_depth[[5]] #4
  layer6 <- NS_snow_depth[[6]] #4
  
  layer1[layer1 == 0] <- NA 
  layer2[layer2 == 0] <- NA
  layer3[layer3 == 0] <- NA
  layer4[layer4 == 0] <- NA
  layer6[layer6 == 0] <- NA
  

  snowAtLocFFS <-extract(layer1,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  snowAtLocLFS <-extract(layer2,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  snowAtLocFCS <-extract(layer3,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  snowAtLocLCS <-extract(layer4,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  contSnowPerAtLoc <-extract(layer1,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)

    fsSnowAtLoc[,i] = c(snowAtLocFFS)
    lsSnowAtLoc[,i] = c(snowAtLocLFS)
    fsSnowContAtLoc[,i] = c(snowAtLocFCS)
    lsSnowContAtLoc[,i] = c(snowAtLocLCS)
    durContSnowPer[,i] = c(contSnowPerAtLoc)
}

setwd("~/Desktop/modis_data/MODISSnowSeasonality/New/dataFrames/")
write.csv(fsSnowAtLoc, file = "fsSnowAtLoc.csv")
write.csv(lsSnowAtLoc, file = "lsSnowAtLoc.csv")
write.csv(fsSnowContAtLoc, file = "fsSnowContAtLoc.csv")
write.csv(lsSnowContAtLoc, file = "lsSnowContAtLoc.csv")


#-------------------------------------------------------------------------------------------------------------
#Find dates of snow free period 2 ways
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

#Dataframe for snow free data (full season) -> Snow Year
FULLSnowFreeSY = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                         "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                         "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                         "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(FULLSnowFreeSY) <- c("2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008",
                           "2009", "2010", "2011", "2012",
                           "2013","2014", "2015", "2016")
row.names(FULLSnowFreeSY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

#Dataframe for snow free data (continuous season) -> Snow Year
CONTSnowFreeSY = data.frame("2001"=rep(0, 42), "2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                         "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                         "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                         "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(CONTSnowFreeSY) <- c("2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008",
                           "2009", "2010", "2011", "2012",
                           "2013","2014", "2015", "2016")
row.names(CONTSnowFreeSY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

#Dataframe for snow free data (full season) -> Calendar Year
FULLSnowFreeCY = data.frame("2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                            "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                            "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                            "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(FULLSnowFreeCY) <- c("2002", "2003", "2004",
                              "2005", "2006", "2007", "2008",
                              "2009", "2010", "2011", "2012",
                              "2013","2014", "2015", "2016")
row.names(FULLSnowFreeCY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

#Dataframe for snow free data (continuous season) -> Calendar Year
CONTSnowFreeCY = data.frame("2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                            "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                            "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                            "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
colnames(CONTSnowFreeCY) <- c("2002", "2003", "2004",
                              "2005", "2006", "2007", "2008",
                              "2009", "2010", "2011", "2012",
                              "2013","2014", "2015", "2016")
row.names(CONTSnowFreeCY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

#Snow Year Analysis FULL
dSFP <- vector("numeric", 42L)
for (i in 1:16)
{
  for (j in 1:42)
  {
    k <- 365 -(lsSnowAtLoc[j,i]-fsSnowAtLoc[j,i])
    dSFP[j] <- k
  }
  FULLSnowFreeSY[,i] = c(dSFP)
}

#Snow Year Analysis CONTINUOUS
dSFP2 <- vector("numeric", 42L)
for (i in 1:16)
{
  for (j in 1:42)
  {
    k <- 365 -(lsSnowContAtLoc[j,i]-fsSnowContAtLoc[j,i])
    dSFP2[j] <- k
  }
  CONTSnowFreeSY[,i] = c(dSFP2)
}

#Calendar Year Analysis FULL
dSFP3 <- vector("numeric", 42L)
for (i in 1:15)
{
  for (j in 1:42)
  {
    if (is.na(fsSnowAtLoc[j,i+1]) || is.na(lsSnowAtLoc[j,i]))
    {
      dSFP3[j] <- NA
    }
    else if (fsSnowAtLoc[j,i+1] < 365 && lsSnowAtLoc[j,i] > 365)
    {
      k <- fsSnowAtLoc[j,i+1] - (lsSnowAtLoc[j,i] -365)
      dSFP3[j] <- k
    }
    else if (fsSnowAtLoc[j,i+1] < 365 && lsSnowAtLoc[j,i] < 365)
    {
      dSFP3[j] <- fsSnowAtLoc[j,i+1] 
    }
    else if (fsSnowAtLoc[j,i+1] > 365 && lsSnowAtLoc[j,i] > 365)
    {
      k <- 365 - (lsSnowAtLoc[j,i] -365)
      dSFP3[j] <- k
    }
    else if (fsSnowAtLoc[j,i+1] > 365 && lsSnowAtLoc[j,i] < 365)
    {
      dSFP3[j] <- 365
    }
  }
  FULLSnowFreeCY[,i] = c(dSFP3)
}

#Calendar Year Analysis CONTINUOUS
dSFP4 <- vector("numeric", 42L)
for (i in 1:15)
{
  for (j in 1:42)
  {
    if (is.na(fsSnowContAtLoc[j,i+1]) || is.na(lsSnowContAtLoc[j,i]))
    {
      dSFP4[j] <- NA
    }
    else if (fsSnowContAtLoc[j,i+1] < 365 && lsSnowContAtLoc[j,i] > 365)
    {
      k <- fsSnowContAtLoc[j,i+1] - (lsSnowContAtLoc[j,i] -365)
      dSFP4[j] <- k
    }
    else if (fsSnowContAtLoc[j,i+1] < 365 && lsSnowContAtLoc[j,i] < 365)
    {
      dSFP4[j] <- fsSnowContAtLoc[j,i+1] 
    }
    else if (fsSnowContAtLoc[j,i+1] > 365 && lsSnowContAtLoc[j,i] > 365)
    {
      k <- 365 - (lsSnowContAtLoc[j,i] -365)
      dSFP4[j] <- k
    }
    else if (fsSnowAtLoc[j,i+1] > 365 && lsSnowContAtLoc[j,i] < 365)
    {
      dSFP4[j] <- 365
    }
  }
  CONTSnowFreeCY[,i] = c(dSFP4)
}
#------------------------------------------------------------------------------------
#Merging the dataframes
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

n <- 630
totalsData <- data.frame("SiteName"=rep(0,n), "Year"=rep(0,n), "ActiveLayer"=rep(0,n),
                         "CFullSnow"=rep(0,n), "CContSnow"=rep(0,n),
                         "SFullSnow"=rep(0,n), "SContSnow"=rep(0,n));
colnames(totalsData) <- c("SiteName","Year","ActiveLayer","FULLSnowFreeCY", "CONTSnowFreeCY",
                          "FULLSnowFreeSY", "CONTSnowFreeSY")
#row.names(CONTSnowFreeCY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])

for (i in 1:15)
{
  a <- 1+(42*(i-1))
  b <- 42+(42*(i-1))
  #Site Names
  totalsData[a:b,1] <- ALDataFrame[1:42,2]
  #Years
  totalsData[a:b,2] <- i+2001
  #Active Layer Depth
  totalsData[a:b,3] <- ALDataFrame[1:42, i+16]
  #C Full Snow
  totalsData[a:b,4] <- FULLSnowFreeCY[1:42,i]
  #C Cont Snow
  totalsData[a:b,5] <- CONTSnowFreeCY[1:42,i]
  #S Full Snow
  totalsData[a:b,6] <- FULLSnowFreeSY[1:42,i+1]
  #S Cont Snow
  totalsData[a:b,7] <- CONTSnowFreeSY[1:42,i+1]
}

totalsData[,3:7][totalsData[,3:7] == 0] <- NA

#------------------------------------------------------------------------------------
#First attempt at plotting
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right
boxplot(ActiveLayer ~ Year, data=totalsData,main="Active Layer Depths by Year")
boxplot(CONTSnowFreeSY ~ Year, data=totalsData,main="Continous Snow Free Period - Snow Year")
boxplot(CONTSnowFreeCY ~ Year, data=totalsData,main="Continous Snow Free Period - Cal. Year")
boxplot(FULLSnowFreeSY ~ Year, data=totalsData,main="Full Snow Free Period - Snow Year")
boxplot(FULLSnowFreeCY ~ Year, data=totalsData,main="Full Snow Free Period - Cal. Year")

contSnowFreeSnowYearInd <- totalsData$CONTSnowFreeSY >0
plot(totalsData[contSnowFreeSnowYearInd,7],totalsData[contSnowFreeSnowYearInd,3],title="Cont. SFP - Snow Year vs ALD")
contSnowFreeSYReg <- lm(totalsData[contSnowFreeSnowYearInd,3] ~totalsData[contSnowFreeSnowYearInd,7])
abline(contSnowFreeSYReg)

#contSnowFreeSYReg <- lm(ActiveLayer ~ CONTSnowFreeSY, data=totalsData)
summary(contSnowFreeSYReg)
plot(contSnowFreeSYReg)


plot(totalsData$CONTSnowFreeSY,totalsData$ActiveLayer)
abline(contSnowFreeSYReg)


#fullSnowFreeSYReg <- lm(ActiveLayer ~ FULLSnowFreeSY, data=totalsData)
fullSnowFreeSnowYearInd <- totalsData$FULLSnowFreeSY<300 #& 100<totalsData$FULLSnowFreeSY


#checking if there is any correlation between years - FULL Snow Year
par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
plot(totalsData[fullSnowFreeSnowYearInd,6],totalsData[fullSnowFreeSnowYearInd,3],
     pch = c(0,1,2,5,6,15,16,17,18,19,20,21,22,23,24),bg = c(
                                                            "red","red","red","red"))
legend('topright', inset=c(-0.2,0),names(FULLSnowFreeSY[2:16]), 
       pch=c(0,1,2,5,6,15,16,17,18,19,20,21,22,23,24), pt.bg=c("red","red","red","red"), bty='n', cex=.75)
fullSnowFreeSYReg <- lm(totalsData[fullSnowFreeSnowYearInd,3] ~ totalsData[fullSnowFreeSnowYearInd,6])
abline(fullSnowFreeSYReg)
summary(fullSnowFreeSYReg)

#Looking at individual years -FULL Snow Year
  fullSnowFreeSnowYearInd <- totalsData$FULLSnowFreeSY<300 #& 100<totalsData$FULLSnowFreeSY[1:42,1]
  for (i in 2:16)
  {
    a <- 1+(42*(i-2))
    b <- 42+(42*(i-2))
    #checking if there is any correlation between years - FULL Snow Year
    par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(totalsData[a:b,6],totalsData[a:b,3])
    legend('topright', inset=c(-0.2,0),names(FULLSnowFreeSY[i]), bty='n', cex=.75)
    fullSnowFreeSYReg <- lm(totalsData[a:b,3] ~ totalsData[a:b,6])
    abline(fullSnowFreeSYReg)
    #summary(fullSnowFreeSYReg)
  }
 
  
  #checking if there is any correlation between locations - FULL Snow Year
  fullSnowFreeSnowYearInd <- totalsData$FULLSnowFreeSY<300 #& 67<totalsData$FULLSnowFreeSY
  par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
  myshapes <- c(21,22,23,24)#,21,22,23,24,21,22,23,24,
                #21,22,23,24,21,22,23,24,21,22,23,24,
                #21,22,23,24,21,22,23,24,21,22,23,24,
                #21,22,23,24,21,22)
  mycolors <- c("red","red","red","red",
                "green","green","green","green",
                "orange","orange","orange","orange",
                "blue","blue","blue","blue",
                "cyan","cyan","cyan","cyan",
                "black","black","black","black",
                "grey","grey","grey","grey",
                "yellow","yellow","yellow","yellow",
                "pink","pink","pink","pink",
                "darkgreen","darkgreen","darkgreen","darkgreen",
                "white","white")
  #with(FULLSnowFreeSY,plot(totalsData[fullSnowFreeSnowYearInd,6], totalsData[fullSnowFreeSnowYearInd,3],
                           #pch=myshapes[row(FULLSnowFreeSY)],bg=mycolors[row(FULLSnowFreeSY)]))
 # for (i in 1:15){
#    a <- 1+(42*(i-1))
#    b <- 42+(42*(i-1))
    # plot(totalsData[fullSnowFreeSnowYearInd,6], totalsData[fullSnowFreeSnowYearInd,3],
    #      pch=myshapes[row(totalsData)[1:42]],bg=mycolors[row(totalsData)[1:42]])
  plot(totalsData[fullSnowFreeSnowYearInd,6], totalsData[fullSnowFreeSnowYearInd,3],
       pch=myshapes,bg=mycolors)
    legend(185,75, inset=c(-0.1,0),totalsData$SiteName[1:42], 
           pch= myshapes, pt.bg = mycolors,bty='n', cex=.45, ncol=2)
    #qplot(totalsData[fullSnowFreeSnowYearInd,6], totalsData[fullSnowFreeSnowYearInd,3], colour = 'color', geom = 'bin2d')
    #k1<- corr(colbind(totalsData$CONTSnowFreeSY,totalsData$`Active Layer`))
 # }
  
#Looking at individual locations - FULL Snow Year
  fullSnowFreeSnowYearInd <- totalsData$FULLSnowFreeSY<300 #& 100<totalsData$FULLSnowFreeSY[1:42,1]
  for (i in 1:42)
  {
    locData <- c(i,i+42,i+(2*42), i+(3*42),i+(4*42),i+(5*42),i+(6*42),i+(7*42),
           i+(8*42),i+(9*42),i+(10*42),i+(11*42),i+(12*42),i+(13*42),i+(14*42))
    #a <- 1+(42*(i-1))
    #b <- 42+(42*(i-1))
    #checking if there is any correlation between years - FULL Snow Year
    par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
    if (all(is.na(totalsData[locData,3])))
    {
      next
    }
    plot(totalsData[locData,6],totalsData[locData,3])
    legend('topright', inset=c(-0.2,0),ALDataFrame$SiteName[i], bty='n', cex=.75)
    #fullSnowFreeSYReg <- lm(totalsData[c,3] ~ totalsData[c,6])
    #abline(fullSnowFreeSYReg)
    #summary(fullSnowFreeSYReg)
   #show(c)
    show(i)
  }
  
  
  
  #------------------------------------------------------------------------------------
  #Freeze and Melt periods
  #Earth Lab - Project Permafrost
  #By: Ksenia Lepikhina and Jeffery Thompson (mentor)
  #Copy Right
  
  #Freeze period - freeze period = start snow - 213
  CONTFreezeSY = data.frame("2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                              "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                              "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                              "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
  colnames(CONTFreezeSY) <- c("2002", "2003", "2004",
                                "2005", "2006", "2007", "2008",
                                "2009", "2010", "2011", "2012",
                                "2013","2014", "2015", "2016")
  row.names(CONTFreezeSY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])
  
  dSFP5 <- vector("numeric", 42L)
  for (i in 1:15)
  {
    for (j in 1:42)
    {
      if (is.na(fsSnowContAtLoc[j,i]) || is.na(lsSnowContAtLoc[j,i]))
      {
        dSFP5[j] <- NA
      }
      else
      {
        show(j)
        show(i)
        d_freeze <- fsSnowContAtLoc[j,i+1]-213
        dSFP5[j] <- d_freeze
        show(d_freeze)
      }
    }
    CONTFreezeSY[,i] = c(dSFP5) 
  }
  
  
  #Melt period - freeze period = start snow - 213
  CONTMeltSY = data.frame("2002"=rep(0,42), "2003"=rep(0,42), "2004"=rep(0,42),
                            "2005"=rep(0,42), "2006"=rep(0,42), "2007"=rep(0,42), "2008"=rep(0,42),
                            "2009"=rep(0,42), "2010"=rep(0,42), "2011"=rep(0,42), "2012"=rep(0,42),
                            "2013"=rep(0,42),"2014"=rep(0,42), "2015"=rep(0,42), "2016"=rep(0,42));
  colnames(CONTMeltSY) <- c("2002", "2003", "2004",
                              "2005", "2006", "2007", "2008",
                              "2009", "2010", "2011", "2012",
                              "2013","2014", "2015", "2016")
  row.names(CONTMeltSY) <-c(activeAK@data$Site_Name[activeAKInd == TRUE])
  
  dSFP6 <- vector("numeric", 42L)
  for (i in 1:15)
  {
    for (j in 1:42)
    {
      if (is.na(fsSnowContAtLoc[j,i]) || is.na(lsSnowContAtLoc[j,i]))
      {
        dSFP6[j] <- NA
      }
      else
      {
        d_melt <- 578 - lsSnowContAtLoc[j,i+1]
        dSFP6[j] <- d_melt
      }
    }
    CONTMeltSY[,i] = c(dSFP6) 
  }
  
  #Combine Active layer depth, freeze period, and melt period into one dataframe
  m <- 630
  meltFreezeAL <- data.frame("SiteName"=rep(0,m), "Year"=rep(0,m), "ActiveLayer"=rep(0,m),
                           "d_Melt"=rep(0,m), "d_Freeze"=rep(0,m),"durationContSnowPer"=rep(0,m));
  colnames(meltFreezeAL) <- c("SiteName","Year","ActiveLayer","Melt", "Freeze", "durationContSnowPer") 
  
  for (i in 1:15)
  {
    a <- 1+(42*(i-1))
    b <- 42+(42*(i-1))
    #Site Names
    meltFreezeAL[a:b,1] <- ALDataFrame[1:42,2]
    #Years
    meltFreezeAL[a:b,2] <- i+2001
    #Active Layer Depth
    meltFreezeAL[a:b,3] <- ALDataFrame[1:42, i+16]
    #Melt
    meltFreezeAL[a:b,4] <- CONTMeltSY[1:42,i]
    #Freeze
    meltFreezeAL[a:b,5] <- CONTFreezeSY[1:42,i]
    #durationContSnowPer
    meltFreezeAL[a:b,6] <- durContSnowPer[1:42,i+1]
  }
  meltFreezeAL[,3:6][meltFreezeAL[,3:6] == 0] <- NA
  
  
  
  #checking if there is any correlation between years - AL vs Freeze
  freeze <- meltFreezeAL$Freeze <150 #& 100<totalsData$FULLSnowFreeSY[1:42,1]
  par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(meltFreezeAL[freeze,5],meltFreezeAL[freeze,3],
       pch = c(0,1,2,5,6,15,16,17,18,19,20,21,22,23,24),bg = c(
         "red","red","red","red"))
  legend('topright', inset=c(-0.2,0),names(FULLSnowFreeSY[2:16]),
         pch=c(0,1,2,5,6,15,16,17,18,19,20,21,22,23,24), pt.bg=c("red","red","red","red"), bty='n', cex=.75)
  freezeReg <- lm(meltFreezeAL[freeze,3] ~ (meltFreezeAL[freeze,4]+meltFreezeAL[freeze,5] +meltFreezeAL[freeze,6]))
  abline(freezeReg)
  summary(freezeReg)
  
  
  #Looking at individual years -AL and melt+freeze
  #fullSnowFreeSnowYearInd <- totalsData$FULLSnowFreeSY<300 #& 100<totalsData$FULLSnowFreeSY[1:42,1]
  for (i in 2:16)
  {
    a <- 1+(42*(i-2))
    b <- 42+(42*(i-2))
    #checking if there is any correlation between years - FULL Snow Year
    par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE)
    plot(meltFreezeAL[a:b,4],meltFreezeAL[a:b,3])
    legend('topright', inset=c(-0.2,0),names(FULLSnowFreeSY[i]), bty='n', cex=.75)
    meltFreezeLineIndiv <- lm(meltFreezeAL[a:b,3] ~ meltFreezeAL[a:b,4]+ meltFreezeAL[a:b,5]+durContSnowPer)
    abline(meltFreezeLineIndiv)
    summary(meltFreezeLineIndiv)
  }
  
  
  boxplot(ActiveLayer ~ Year, data=meltFreezeAL,main="Active Layer Depths by Year")
  boxplot(Melt ~ Year, data=meltFreezeAL,main="Melt - Snow Year")
  boxplot(Freeze ~ Year, data=meltFreezeAL,main="Freeze - Cal. Year")
  boxplot(durationContSnowPer ~ Year, data=meltFreezeAL,main="Duration of Cont Snow Per - Snow Year")
  
  #Combine Active layer depth, freeze period, and melt period into one dataframe
  q <- 375
  temp <- data.frame("SiteName"=rep(0,q), "Year"=rep(0,q), "ActiveLayer"=rep(0,q),
                             "d_Melt"=rep(0,q), "d_Freeze"=rep(0,q),"durationContSnowPer"=rep(0,q));
  colnames(temp) <- c("SiteName","Year","ActiveLayer","Melt", "Freeze", "durationContSnowPer") 
  
  for (i in 1:15)
  {
    a <- 1+(25*(i-1))
    b <- 25+(25*(i-1))
    #Site Names
    temp[a:b,1] <- ALDataFrame[1:25,2]
    #Years
    temp[a:b,2] <- i+2001
    #Active Layer Depth
    temp[a:b,3] <- ALDataFrame[1:25, i+16]
    #Melt
    temp[a:b,4] <- CONTMeltSY[1:25,i]
    #Freeze
    temp[a:b,5] <- CONTFreezeSY[1:25,i]
    #durationContSnowPer
    temp[a:b,6] <- durContSnowPer[1:25,i+1]
  }
  
  plot(temp[,6], temp[,3])
  tempReg <- lm(temp[,3] ~ (temp[,6]))
  abline(tempReg, method="spearman")
  summary(tempReg)
  corr.test(temp[3],temp[6], method="spearman") 
  
#1000 method 
tempvec <- c(1,3,5,7,12,14,17,22)
#figure out loop
for (i in 2:14)
{
  plot(temp[tempvec,6], temp[tempvec,3])
  tempReg <- lm(temp[tempvec,3] ~ (temp[tempvec,6]))
  abline(tempReg, method="spearman")
  summary(tempReg)
  corr.test(temp[tempvec,3],temp[tempvec,6], method="spearman") 
}
  