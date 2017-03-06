# opening the modis data files

#####Do I need this?
#library(rhdf5)
#hdf_files <- list.files('~/Downloads/', pattern = '.h5')
#setwd("~")
#file <- file.path("Downloads", hdf_files)
#####

library(sp)
library(rgdal)
library(raster)
library(rgeos)

#open folder
setwd("~/Desktop/modis_data/MODISSnowSeasonality")
dataBaseDir <- c("~/Desktop/")
lstDir <- paste(dataBaseDir,c("modis_data/MODISSnowSeasonality/"),sep="")

#Clipping function
#source: https://stat.ethz.ch/pipermail/r-sig-geo/2013-July/018912.html
clip<-function(raster,shape)
{
  cropped<-crop(raster,shape)
  cells<-rasterize(shape,cropped)
  cropped*cells
}

#in folder, files names are only the ones that end in .tif
fileNames <- list.files(pattern = ".tif")

#iterate through all of the files
for (i in 1:length(fileNames))
{
  #create data frame
  d = data.frame( "min"=rep(0, 6), "max"=rep(0,6));
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
  
  #Populate data frame of min and max of 1-6 layer of raster stack
  for (j in 1:6)
  {
    extremaData <- NS_snow_depth[[j]]
    extremaData[extremaData == 0] <- NA
    d[j, ] = c("Min" = extremaData@data@min, "Max" = extremaData@data@max)
    row.names(d) <- c("firstFull","lastFull","rangeFull","firstCont","lastCont","rangeCont")
  }
  assign(paste0("year", i), d)
  
  #check if it worked
  show(i)
}

#remove the used data frame
rm(d)