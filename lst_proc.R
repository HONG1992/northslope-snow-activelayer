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
# this code is written by Jeff Thompson & Ksenia , CU Earth Lab
#   2017-02-27
#   

# load libraries
#library(rhdf5)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)

# set up path/directory variables - set for Jeff's computer
dataBaseDir <- c("~/Desktop/permafrost/Alaska/")
tifext <- c(".tif")

snowDir <- paste(dataBaseDir,c("GINA/MODISSnowSeasonality/"),sep="")
activeDir <- paste(dataBaseDir,c("ActiveLayer/CALM_Sites_wData/"),sep="")
#lstDir <- paste(dataBaseDir,c("Arctic/MODIS_Pan-ARC_Weekly_2007-2013"),sep="")
lstDir <- paste(dataBaseDir,c("Pangea/LST_weekly/"),sep="")
boundDir <- paste(dataBaseDir,c("GINA/AKLCC_boundaries/"),sep="")

# file prefixes
lstPrf = c("UW_MODAV_LST_1.1.4_001_")

#snowDirIn <- paste(dataBaseDir,snowDir,sep="")
#activeDirIn <- paste(dataBaseDir,activeDir,sep="")

# set the working directory to the project directory
setwd("~/Desktop/r_data/NorthSlopeActiveLayer/")

# open the LST files - later do this in a loop
lstIn <- stack(paste(lstDir,c("2007/"),lstPrf,c("2007.1.1_0.0.0_2007.1.7_23.59.59_001_001"),tifext,sep=""))
#plot(lstIn)

# load in north slope boundary shapefile - two ways of doing this. Second works better for overlay, for some reason
#ntSlope <- shapefile(paste(boundDir,c("LCC_Arctic_Alaska_Yukon.shp"),sep=""),stringsAsFactors=FALSE)
ntSlope<-readOGR(dsn=path.expand(paste(boundDir,c("LCC_Arctic_Alaska_Yukon.shp"),sep="")),layer="LCC_Arctic_Alaska_Yukon")

#load activelayer point data - not sure why the readOGR fails for this one :(
activeIn <- shapefile(paste(activeDir,c("CALM_SitesData.shp"),sep=""),stringsAsFactors=FALSE)
#activeIn<-readOGR(dsn=path.expand(paste(activeDir,c("CALM_SitesData.shp"),sep="")),layer="CALM_SitesData.shp")

#need to reproject the active layer data to the north slope projection
ntSlopeProj <-projection(ntSlope)
activeAK <- spTransform(activeIn,ntSlopeProj)

# reproject the lst data
lstAK <- projectRaster(lstIn,crs=ntSlopeProj)

# find indexes of points that are contained by the ntSlope boundary
activeAKInd <- !is.na(over(activeAK,as(ntSlope, "SpatialPolygons")))

# plot these as a simple map to see how things look.
#   points contained by shape file are in red, else grey cicles
#points(activeIn[activeAKInd])
plot(coordinates(activeAK),type="n")
plot(ntSlope,border="blue",add=TRUE)
# now plot active layer sites instide and outside alaska 
points(activeAK[!activeAKInd, ], pch=1, col="gray")
points(activeAK[activeAKInd, ], pch=16, col="red")

# turns out there are there are strange, hidden characters in the active layer data.
#   these data need to be cleaned
# View(activeAK) # to see whole attribute table
activeAK$F7 <- as.numeric(gsub("\xa0", "", activeAK$F7))
activeAK$F7 <- as.numeric(gsub("<a0>", "", activeAK$F7))
activeAK$F19 <- as.numeric(gsub("\xa0", "", activeAK$F19))
activeAK$F19 <- as.numeric(gsub("<a0>", "", activeAK$F19))
activeAK$F25 <- as.numeric(gsub("\xa0", "", activeAK$F25))
activeAK$F25 <- as.numeric(gsub("<a0>", "", activeAK$F25))

# substitute 0 for NAs in rows 7:32
activeAK@data[,7:32][activeAK@data[,7:32] == 0]<-NA

# now that active layer data are cleaned, better write a new shapefile
writeOGR(activeAK,
         dsn=path.expand(paste(activeDir,c("CALM_SitesData_Cleaned.shp"),sep="")),
         "CALM_SitesData_Cleaned", 
         driver="ESRI Shapefile")


# load in snow seasonality data
#snowIn <-stack(paste(snowDir,"./2001_snowyear_metrics_v7.tif",sep=""))


# testing of using gsub on a slice of attribute table 
#   appears not to work
#test1<-activeAK
#test1[,7:32]<-as.integer(gsub("\xa0", "", test1[,7:32]))
#test1[,7:32]<-as.integer(gsub("<a0>", "", test1[,7:32]))

# do some buffering of active layer points 
#   buffer is in pixels - depends on raster resolution.
#   LST resolution is 1000 m
buf <- c(3)
#nthActiveBuff <-buffer(activeAK[activeAKInd],width=rad)
#nthLstBuff <-extract(lstAK,activeAK@coords[activeAKInd,],buffer=buf,fun=median,na.rm=TRUE)
nthLstBuff <-extract(lstAK,activeAK@coords[activeAKInd,],buffer=buf,fun=mean,na.rm=TRUE)
#projection(activeIn)





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
