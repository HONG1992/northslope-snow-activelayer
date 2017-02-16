# describe the code - what it the aim of this specific code? is it based on a paper? if so list the paper
#
# this code is based off of code written by Amy Decastro and Jeff Thompson in the summoer of 2016 as part of
#   the Earth Lab summer internship program.

# opening the modis data files

library(rhdf5)
hdf_files <- list.files('~/Downloads/', pattern = '.h5')
setwd("~")
file <- file.path("Downloads", hdf_files)

##### with Jeff 

library(sp)
library(rgdal)
library(raster)
library(rgeos)

#open .tif
setwd("~/Desktop/modis_data")
snow_depth <- stack("./2001_snowyear_metrics_v7.tif")
#plot(BAP)

#read in Alaskan North Slope shp file
NS <- readOGR(dsn="./LCC_Arctic_Alaska_Yukon/LCC_Arctic_Alaska_Yukon.shp",layer="LCC_Arctic_Alaska_Yukon")
arctic <-NS[NS$LCC_Name == "Arctic LCC",]

#source: https://stat.ethz.ch/pipermail/r-sig-geo/2013-July/018912.html
clip<-function(raster,shape) {
     cropped<-crop(raster,shape)
     cells<-rasterize(shape,cropped)
     cropped*cells
}
#clip raster to shape file
clip_snow_depth<-clip(raster = snow_depth, shape = arctic)
NS_snow_depth <- clip_snow_depth

#write raster to new .tif file
writeRaster(NS_snow_depth, filename="./2001_snowyear_metrics_New.tif", options="INTERLEAVE=BAND", overwrite=TRUE)

#plot labels
labels <- c("first_snow_day", "last_snow_day", "fss_range", "longest_css_first_day", "longest_css_last_day", "longest_css_day_range", "snow_days", "no_snow_days", "css_segment_num", "mflag", "cloud_days", "tot_css_days")
names(NS_snow_depth) <- labels[1:12]
plot(NS_snow_depth)

snow_sr<-crop(snow_depth,NS)

#extract raster values for polygon
snow_depth_ext <- extract(snow_sr, NS, cellnumbers=TRUE, df=TRUE) 
colnames(snow_depth_ext)<-c("cells","values", "first_snow_day", "last_snow_day", "fss_range", "longest_css_first_day", "longest_css_last_day", "longest_css_day_range", "snow_days", "no_snow_days", "css_segment_num", "mflag", "cloud_days", "tot_css_days")
snow_depth_ext_i <-snow_depth_ext$cell