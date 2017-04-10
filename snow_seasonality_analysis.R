#This script contains the analysis of snow seasonality descriptors and permafrost data 
#using the dataframes created in the file snow_seasonality_proc.R. See snow_seasonality_proc.R and/or
#comments for more info.


#Snow Duration and Active Layer Depth Analysis
#Earth Lab - Project Permafrost
#By: Ksenia Lepikhina and Jeffery Thompson (mentor)
#Copy Right

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(psych)
#-------------------------------------------------------------------------------------------------------------
#Plot north slope data

#label plots and plot
labels <- c("first_snow_day", "last_snow_day", "fss_range", "longest_css_first_day", "longest_css_last_day", "longest_css_day_range", "snow_days", "no_snow_days", "css_segment_num", "mflag", "cloud_days", "tot_css_days")
names(NS_snow_depth) <- labels[1:12]
plot(NS_snow_depth)
#-------------------------------------------------------------------------------------------------------------
#Analysis of Permafrost Data

#Plot the North Slope -WARNING takes forever for some reason
plot(coordinates(activeAK),type="n")
plot(NS,border="blue",add=TRUE)

#Plot active layer sites inside and outside shape file of North Slope 
points(activeAK[!activeAKInd, ], pch=1, col="gray") #outside
points(activeAK[activeAKInd, ], pch=16, col="red") #inside
#-------------------------------------------------------------------------------------------------------------
#Find dates of snow free period (full and continuous) 2 ways

#Snow Year Analysis (FULL snow season)
dSFP <- vector("numeric", 42L) #Vector to populate
#data goes from 2001-2016
#for 16 years
for (i in 1:16)
{
  #for 42 locations
  for (j in 1:42)
  {
    #dSFP = 365-(End –Start)
    k <- 365 -(lsSnowAtLoc[j,i]-fsSnowAtLoc[j,i])
    dSFP[j] <- k
  }
  #populate data frame
  FULLSnowFreeSY[,i] = c(dSFP)
}

#Snow Year Analysis (CONTINUOUS snow season)
#same as above w/ continuous instead of full
#data goes from 2001-2016
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
#lose a year in the math so data goes from 2002-2016
for (i in 1:15)
{
  for (j in 1:42)
  {
    #check if NA
    if (is.na(fsSnowAtLoc[j,i+1]) || is.na(lsSnowAtLoc[j,i]))
    {
      dSFP3[j] <- NA
    }
    # start(y+1) < 365 & end(y) > 365
    else if (fsSnowAtLoc[j,i+1] < 365 && lsSnowAtLoc[j,i] > 365)
    {
      #dSFP = start(y+1) – (end(y) -365)
      k <- fsSnowAtLoc[j,i+1] - (lsSnowAtLoc[j,i] -365)
      dSFP3[j] <- k
    }
    #start(y+1) < 365 & end(y) < 365
    else if (fsSnowAtLoc[j,i+1] < 365 && lsSnowAtLoc[j,i] < 365)
    {
      #dSFP = start(y+1)
      dSFP3[j] <- fsSnowAtLoc[j,i+1] 
    }
    #start(y+1) > 365 & end(y) > 365
    else if (fsSnowAtLoc[j,i+1] > 365 && lsSnowAtLoc[j,i] > 365)
    {
      #dSFP = 365 – (end(y) -365)
      k <- 365 - (lsSnowAtLoc[j,i] -365)
      dSFP3[j] <- k
    }
    #start(y+1) > 365 & end(y) < 365
    else if (fsSnowAtLoc[j,i+1] > 365 && lsSnowAtLoc[j,i] < 365)
    {
      #dSFP = 365
      dSFP3[j] <- 365
    }
  }
  #populate dataframe
  FULLSnowFreeCY[,i] = c(dSFP3)
}

#Calendar Year Analysis CONTINUOUS
#same format as above except continuous instead of full
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

#------------------------------------------------------------------------------------
#First attempt at plotting

#test box plots
#AL depth vs Years
boxplot(ActiveLayer ~ Year, data=totalsData,main="Active Layer Depths by Year")
#Continuous snow free period (snow year) vs year
boxplot(CONTSnowFreeSY ~ Year, data=totalsData,main="Continous Snow Free Period - Snow Year")
#Continuous snow free period (calendar year) vs year
boxplot(CONTSnowFreeCY ~ Year, data=totalsData,main="Continous Snow Free Period - Cal. Year")
#Full snow free period (snow year) vs year
boxplot(FULLSnowFreeSY ~ Year, data=totalsData,main="Full Snow Free Period - Snow Year")
#Full snow free period (calendar year) vs year
boxplot(FULLSnowFreeCY ~ Year, data=totalsData,main="Full Snow Free Period - Cal. Year")

#Active layer vs Continuous Snow Free Period (snow year)
contSnowFreeSnowYearInd <- totalsData$CONTSnowFreeSY >0
plot(totalsData[contSnowFreeSnowYearInd,7],totalsData[contSnowFreeSnowYearInd,3],title="Cont. SFP - Snow Year vs ALD")
contSnowFreeSYReg <- lm(totalsData[contSnowFreeSnowYearInd,3] ~totalsData[contSnowFreeSnowYearInd,7])
abline(contSnowFreeSYReg)

#same as above but with more info
contSnowFreeSYReg <- lm(ActiveLayer ~ CONTSnowFreeSY, data=totalsData)
summary(contSnowFreeSYReg)
plot(contSnowFreeSYReg)

#same as above but with better labels
#plot(totalsData$CONTSnowFreeSY,totalsData$ActiveLayer)
#abline(contSnowFreeSYReg)


#checking if there is any correlation between years - FULL Snow Year
par(mar=c(4.1, 4.1, 4.1, 8.1), xpd=TRUE) #sets border size
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
  #set each row to go from a:b each iteration
  a <- 1+(42*(i-2))
  b <- 42+(42*(i-2))
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
#plot each location on a separate plot
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

dSFP5 <- vector("numeric", 42L) #new vector
#2002-2016
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
      #check
      show(j)
      show(i)
      #calculate the freeze period
      d_freeze <- fsSnowContAtLoc[j,i+1]-213
      dSFP5[j] <- d_freeze
      #check
      show(d_freeze)
    }
  }
  #place in Dataframe
  CONTFreezeSY[,i] = c(dSFP5) 
}

#almost the same as above
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
      #calculate the melt period 
      d_melt <- 578 - lsSnowContAtLoc[j,i+1]
      dSFP6[j] <- d_melt
    }
  }
  CONTMeltSY[,i] = c(dSFP6) 
}

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
summary(freezeReg) #BEST correation R^2 7.89%

#Looking at individual years -AL and melt+freeze+duration of continuous snow period <-(meltFreezeLineIndiv)
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

#AL depth vs Years
boxplot(ActiveLayer ~ Year, data=meltFreezeAL,main="Active Layer Depths by Year")
#Melt vs Years
boxplot(Melt ~ Year, data=meltFreezeAL,main="Melt - Year")
#Freeze vs Years
boxplot(Freeze ~ Year, data=meltFreezeAL,main="Freeze - Year")
#duration of continuous snow period vs Years
boxplot(durationContSnowPer ~ Year, data=meltFreezeAL,main="Duration of Cont Snow Per - Year")

#look only at RED (see original data) data (*also see temp for the dataframe); temp = red coloring
plot(temp[,6], temp[,3])
tempReg <- lm(temp[,3] ~ (temp[,6]))
abline(tempReg, method="spearman")
summary(tempReg)
corr.test(temp[3],temp[6], method="spearman")  #NOT a very good correlation

#1000 method (The red ones in the data using the 1000 method)
plot(thousandDF[,4], thousandDF[,3])
thousandReg <- lm(thousandDF[,3] ~ (thousandDF[,4]))
abline(thousandReg, method= "spearman")
summary(thousandReg)
corr.test(thousandDF[3],thousandDF[4],method= "spearman") #NOT a very good correlation AT ALL using the 1000 method
