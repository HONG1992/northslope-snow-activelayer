#Snow Seasonality Descriptors
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

library(sp)
library(rgdal)
library(raster)
library(rgeos)

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

# ALDataFrame[,1] = c("SiteCode" = activeAK@data$Site_Code[1:41])
# ALDataFrame[,2] = c("SiteCode" = activeAK@data$Site_Name[1:41])
# ALDataFrame[,3] = c("SiteCode" = activeAK@data$Latitude[1:41])
# ALDataFrame[,4] = c("SiteCode" = activeAK@data$Longitude[1:41])
# ALDataFrame[,5] = c("SiteCode" = activeAK@data$F7[1:41])
# ALDataFrame[,6] = c("SiteCode" = activeAK@data$F8[1:41])
# ALDataFrame[,7] = c("SiteCode" = activeAK@data$F9[1:41])
# ALDataFrame[,8] = c("SiteCode" = activeAK@data$F10[1:41])
# ALDataFrame[,9] = c("SiteCode" = activeAK@data$F11[1:41])
# ALDataFrame[,10] = c("SiteCode" = activeAK@data$F12[1:41])
# ALDataFrame[,11] = c("SiteCode" = activeAK@data$F13[1:41])
# ALDataFrame[,12] = c("SiteCode" = activeAK@data$F14[1:41])
# ALDataFrame[,13] = c("SiteCode" = activeAK@data$F15[1:41])
# ALDataFrame[,14] = c("SiteCode" = activeAK@data$F16[1:41])
# ALDataFrame[,15] = c("SiteCode" = activeAK@data$F17[1:41])
# ALDataFrame[,16] = c("SiteCode" = activeAK@data$F18[1:41])
# ALDataFrame[,17] = c("SiteCode" = activeAK@data$F19[1:41])
# ALDataFrame[,18] = c("SiteCode" = activeAK@data$F20[1:41])
# ALDataFrame[,19] = c("SiteCode" = activeAK@data$F21[1:41])
# ALDataFrame[,20] = c("SiteCode" = activeAK@data$F22[1:41])
# ALDataFrame[,21] = c("SiteCode" = activeAK@data$F23[1:41])
# ALDataFrame[,22] = c("SiteCode" = activeAK@data$F24[1:41])
# ALDataFrame[,23] = c("SiteCode" = activeAK@data$F25[1:41])
# ALDataFrame[,24] = c("SiteCode" = activeAK@data$F26[1:41])
# ALDataFrame[,25] = c("SiteCode" = activeAK@data$F27[1:41])
# ALDataFrame[,26] = c("SiteCode" = activeAK@data$F28[1:41])
# ALDataFrame[,27] = c("SiteCode" = activeAK@data$F29[1:41])
# ALDataFrame[,28] = c("SiteCode" = activeAK@data$F30[1:41])
# ALDataFrame[,29] = c("SiteCode" = activeAK@data$F31[1:41])
# ALDataFrame[,30] = c("SiteCode" = activeAK@data$F32[1:41])
# ALDataFrame[,31] = c("SiteCode" = activeAK@data$test[1:41])


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

buf <- c(3)

for (i in 1:length(fileNames)){
  
  savedFile = file.path('~/Desktop/modis_data/MODISSnowSeasonality/New', fileNames[i]);
  NS_snow_depth <- stack(paste(savedFile,sep=""))
  
  layer1 <- NS_snow_depth[[1]]
  layer2 <- NS_snow_depth[[2]]
  layer3 <- NS_snow_depth[[3]]
  layer4 <- NS_snow_depth[[4]]
  layer1[layer1 == 0] <- NA 
  layer2[layer2 == 0] <- NA
  layer3[layer3 == 0] <- NA
  layer4[layer4 == 0] <- NA

  snowAtLocFFS <-extract(layer1,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  snowAtLocLFS <-extract(layer2,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  snowAtLocFCS <-extract(layer3,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
  snowAtLocLCS <-extract(layer4,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)

    fsSnowAtLoc[,i] = c(snowAtLocFFS)
    lsSnowAtLoc[,i] = c(snowAtLocLFS)
    fsSnowContAtLoc[,i] = c(snowAtLocFCS)
    lsSnowContAtLoc[,i] = c(snowAtLocLCS)
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
fullSnowFreeSnowYearInd <-totalsData$FULLSnowFreeSY <300
plot(totalsData[fullSnowFreeSnowYearInd,6],totalsData[fullSnowFreeSnowYearInd,3],title="Full SFP Snow Year vs Active Layer")
fullSnowFreeSYReg <- lm(totalsData[fullSnowFreeSnowYearInd,3] ~ totalsData[fullSnowFreeSnowYearInd,6])
abline(fullSnowFreeSYReg)
summary(fullSnowFreeSYReg)

#k1<- corr(colbind(totalsData$CONTSnowFreeSY,totalsData$`Active Layer`))

