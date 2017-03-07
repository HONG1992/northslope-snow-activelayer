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