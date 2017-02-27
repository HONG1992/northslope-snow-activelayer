# this code imports the MODIS LST data that are part of the ESA DUE Permafrost 
#   Project. This subset are the North SLope locations. 
#
# Data are described in more detail in :
#   Hachem, Sonia; Allard, Michel; Duguay, Claude R (2009): Using the MODIS 
#       land surface temperature product for mapping permafrost: an application 
#       to northern Québec and Labrador, Canada. Permafrost and Periglacial 
#       Processes, 20(4), 407-416, doi:10.1002/ppp.672
#   Hachem, Sonia; Duguay, Claude R; Allard, Michel (2011): Comparison of 
#       MODIS-derived land surface temperatures with near-surface soil and air 
#       temperature measurements in continuous permafrost terrain. The 
#       Cryosphere Discussion, 5, 1583-1625, doi:10.5194/tcd-5-1583-2011
#
#
# ESA Data User Element (DUE) Permafrost Full Product Set 
#   (doi:10.1594/PANGAEA.780111).
# 
# CALM annual thaw dept data and locations are from:
#   http://gtnpdatabase.org/activelayers
#
# this code is written by Jeff Thompson, CU Earth Lab
#   2017-02-27
#   

# load libraries
#library(rhdf5)
library(sp)
library(rgdal)
library(raster)
library(rgeos)

# set up path/directory variables
dataInDir <- c('')



# below are lines for Ksenia's snow_seasonality_proc code

# opening the modis data files

hdf_files <- list.files('~/Downloads/', pattern = '.h5')
setwd("~")
file <- file.path("Downloads", hdf_files)
##### with Jeff 

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