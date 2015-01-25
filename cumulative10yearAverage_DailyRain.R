#####################################################################################################
#  Name:       10 year index rainfall averages
#  Desc:       Calculate daily average rainfall based on on 10 years of record for sites with 
#              sufficient available data
#  Created by: seanlhodges
#  Created on: 21-January-2015
#  
#####################################################################################################


# Load requried libraries

library(XML)
library(reshape2)

###########################################################
## Pulling Rainfall Record
HSERVER<-c("hilltopdev")
LastYear<-2014

## STEP 1 - Using server API to determine available rainfall sites ---------------------------------------
# Based on the collection for rainfall, specify URL for 10 years
# of daily rainfall record for every active rainfall site

dataStart <- as.POSIXct(strptime("2003-01-01T00:00:00",format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))

url <- "http://hilltopdev.horizons.govt.nz/provisional.hts?service=Hilltop&request=GetData&Collection=zVirtual%20Rainfall&interval=1%20day&method=Total&from=2003-01-01&to=2013-12-31"

getData.xml <- xmlInternalTreeParse(url)
sites<-sapply(getNodeSet(getData.xml,"//Measurement/@SiteName"),as.character)

minday <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement/Data/E[1]/T",sep="")), xmlValue)
maxday <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement/Data/E[last()]/T",sep="")), xmlValue)

minday <- as.POSIXct(strptime(minday,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))
maxday <- as.POSIXct(strptime(maxday,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))



# Setting flags for those sites that have a start date matching dataStart i.e have 30 years record
flag10 <- (minday-dataStart) == 0

# Create a dataframe to summarise what sites can be used for 10 year statistics
dfscan_all <- data.frame(sites,flag10,minday,maxday)
# A bit of a tidy up
rm(sites,flag10,minday,maxday,dataStart,url)


dfscan <- subset(dfscan,flag10==TRUE)
## STEP 2 - Using SOS to call data and create 10 year data matrix ---------------------------------------
# Using the list of sites, and constructing the SOS2.0 requests to deliver the data
# as WaterML2


## DEBUG THIS SECTION USE i=1 : Akitio at Toi Flat

## For each site 
for(siteIndex in 1:length(dfscan[,1])){
  
  
    ## Start [myCodeBlock] ----------------
    load(file=paste("10YR_qdata ",as.character(dfscan$sites[siteIndex]),".RData",sep=""))  ## this loads in the data quantile data.frame - dq
    load(file=paste("10YR_rdata ",as.character(dfscan$sites[siteIndex]),".RData",sep=""))  ## this loads in rainfall series data.frame - df
    
    
    ## PROCESSING CURRENT YEAR RAINFALL RECORD -----------------------------------------
    url2 <- paste("http://hilltopdev.horizons.govt.nz/boo.hts?service=Hilltop&request=GetData&Site=",as.character(dfscan$sites[siteIndex]),"&Measurement=Rainfall [SCADA Rainfall]&method=Total&interval=1 day&alignment=0:00:00&from=2014-07-02&to=2015-07-01",sep="")
    getThisYear.xml <- xmlInternalTreeParse(url2)
    csites<-sapply(getNodeSet(getThisYear.xml,"//Measurement/@SiteName"),as.character)
    
    
    cday <- sapply(getNodeSet(getThisYear.xml, paste("//Hilltop/Measurement[@SiteName='",csites[1],"']/Data/E/../E/T",sep="")), xmlValue)
    ctotal <- sapply(getNodeSet(getThisYear.xml, paste("//Hilltop/Measurement[@SiteName='",csites[1],"']/Data/E/../E/I1",sep="")), xmlValue)
    csite <- rep(csites[1],length(cday))
    
    ctotal<-as.numeric(ctotal)
    cday<-as.Date(cday)
    cday<-cday-1   ## Shift date back by one day. Rainfall is total to midnight and gets filed against yyyymmdd 24:00m, which is the same as yymmdd+1 00:00 
    
    cdd<-as.numeric(format(cday,"%d"))
    cmm<-as.numeric(format(cday,"%m"))
    cyy<-as.numeric(format(cday,"%Y"))
    cddmm<-as.character(format(cday,"%d-%B"))
    
    cdf <- data.frame(cday,ctotal,csite,cddmm,cyy,cmm, stringsAsFactors=FALSE)
    as.factor(cdf$cyy)
    
    ## PROCESSING LAST YEARS RAINFALL RECORD  -----------------------------------------
    url3<- paste("http://hilltopdev.horizons.govt.nz/provisional.hts?service=Hilltop&request=GetData&Site=",as.character(dfscan$sites[siteIndex]),"&Measurement=Rainfall&method=Total&interval=1 day&alignment=0:00:00&&from=2013-07-02&to=2014-07-01",sep="")
    getLastYear.xml <- xmlInternalTreeParse(url3)
    lsites<-sapply(getNodeSet(getLastYear.xml,"//Measurement/@SiteName"),as.character)
    
    
    lday <- sapply(getNodeSet(getLastYear.xml, paste("//Hilltop/Measurement[@SiteName='",lsites[1],"']/Data/E/../E/T",sep="")), xmlValue)
    ltotal <- sapply(getNodeSet(getLastYear.xml, paste("//Hilltop/Measurement[@SiteName='",lsites[1],"']/Data/E/../E/I1",sep="")), xmlValue)
    lsite <- rep(lsites[1],length(lday))
    
    ltotal<-as.numeric(ltotal)
    lday<-as.Date(lday)
    lday<-lday-1   ## Shift date back by one day. Rainfall is total to midnight and gets filed against yyyymmdd 24:00m, which is the same as yymmdd+1 00:00 
    
    ldd<-as.numeric(format(lday,"%d"))
    lmm<-as.numeric(format(lday,"%m"))
    lyy<-as.numeric(format(lday,"%Y"))
    lddmm<-as.character(format(lday,"%d-%B"))
    
    if(length(ldd)>=365){
          
        
        ldf <- data.frame(lday,ltotal,lsite,lddmm,lyy,lmm, stringsAsFactors=FALSE)
        as.factor(ldf$lyy)
        
        ## PREPARE DATA FOR PLOTTING BARPLOT --------------------------------------------------------------
        d <- read.zoo(df[ ,-3:-6]) # Removing unnecessary columns
        e <- read.zoo(cdf[ ,-3:-6]) # Removing unnecessary columns
        l <- read.zoo(ldf[ ,-3:-6]) # Removing unnecessary columns
        
        
        # CUMULATIVE VALUS SELECTED YEARS
        # Selecting last years time slice
        x <- window(l, start=as.Date(paste(as.character(LastYear-1),"-07-01",sep="")), end=as.Date(paste(as.character(LastYear),"-06-30",sep="")))
        index(x) <- 1:length(x)
        
        # Selecting 2003-04 Flood Time slice
        y <- window(d, start=as.Date("2003-07-01"), end=as.Date("2004-06-30"))
        index(y) <- 1:length(y)
        
        # Selecting 2014-15 year time slice
        z <- window(e, start=as.Date("2014-07-01"))
        index(z) <- 1:length(z)
        
        # CALCULATING STATS OVER RECORD
        # Grabbing 30 years of record
        #y30 <- window(d, start=as.Date("1981-01-01"), end=as.Date("2010-12-31")) ## 10957 days
        # Calculate monthly totals across the last thirty years
        y10sum <- aggregate(d, as.Date(cut(time(d), "month")), sum)          ## 360 (12 months x 30yrs)
        y10mean <- monthlyfunction(y10sum, FUN=mean, na.rm=TRUE)                 ## 12 rows
        coredata(y10mean)<-coredata(y10mean)[c(7:12,1:6)]
        index(y10mean)<-index(y10mean)[c(7:12,1:6)]
        
        # Applying monthlyfunction to this season and last season
        yNow  <- window(e, start=as.Date("2014-07-01"), end=as.Date("2015-06-30"))
        yLast <- window(l, start=as.Date("2013-07-01"), end=as.Date("2014-06-30"))
        
        yNowSum  <- as.data.frame(monthlyfunction(yNow, FUN=sum, na.rm=TRUE))
        yLastSum <- as.data.frame(monthlyfunction(yLast, FUN=sum, na.rm=TRUE))
        if(length(yNowSum)!=12){
          n<-as.numeric(length(yNowSum[,1]))
          for(i in (n+1):12){
            yNowSum[i, ]<-0
          }
        }
        
        ## The next statements binds all vectors together into a dataframe. Each Vector/DF needs
        ## to be the same length. Might pay to check current year, and extend if necessary
        bar<-as.data.frame(list(y10mean,yLastSum,yNowSum))
        colnames(bar) <- c("Average","LastSeason","ThisYear")
        
        # No data values for remaining months in year
        #bar$ThisYear[10:12] <- 0
        
        ## This section can be modified to deal with dates over a Jul-Jun year
        melt.bar<-melt(data.frame(month=rownames(bar), bar, stringsAsFactors=FALSE), id.vars="month")
        melt.bar$monthnum <- c(1:12)
        
        #melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun",
        #                                                                                    "Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
        
        Jul2JunLabels<-c("Jul","Aug","Sep","Oct","Nov","Dec",
                         "Jan","Feb","Mar","Apr","May","Jun")
        
        melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=Jul2JunLabels,ordered=TRUE)
        
        #casting as array
        bar <- acast(melt.bar, variable ~ monthnum, sum, margins=FALSE)
        
        ## PLOTTING ROUTINE ----------------------------------------------------------------------
        ## NOTE: Using Base Graphics Package
        
        ## PLOT SETTINGS ## ----------------------------------------------------------------------
        outputType <- "image" # c("screen","image")
        opar<-par()
        
        if(outputType=="image"){
          png(paste("//ares/Environmental Archive/Resource/Rainfall Statistics/",as.character(dfscan$sites[siteIndex]),".","PNG",sep=""),width=1024,height=680,units="px", res=96) ## ,quality=100 -- just for JPEGs
        }
        
        par(mfrow=c(3,1),mar=c(2,3,1,3)+0.1,cex=0.9, mgp=c(1.5,0.4,0.0),tck=1,tcl=-0.3)
        
        ## PLOT 1 ## --------------------------------------------------------------------------------
        ## Daily Cummulative Rainfall PLOT
        plot(cumsum(y), type="n", axes=FALSE, ylim = c(0, max(dq$p90)*1.2), xlab="", ylab="Accumulated rain (mm)") # bty='n'
        title(main = "July to June rainfall accumulation to date (mm)", font.main = 1.5)
        
        i.for <- 1:365
        i.back <- 365:1
        
        x.polygon <- c( i.for, i.back )
        y1.polygon <- c( dq$p90[i.for] , dq$p10[i.back] )
        y2.polygon <- c( dq$p95[i.for] , dq$p5[i.back] )
        
        polygon( x.polygon , y2.polygon , col=rgb(0.1,0.1,0.1,0.2, maxColorValue=1), border = NA)  ##  5-95th %-ile band
        polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)  ## 10-90th %-ile band
        
        points(dq$p50,type="l", col="black", lwd=2, lty="solid")   ## Plotting median
        points(cumsum(x),type="l", col="cornflowerblue",lwd=2)     ## Plotting last year
        points(cumsum(z),type="l", col="coral3", lwd=3)            ## Plotting this year
        
        axis(side=2, outer=FALSE)
        axis(side=1, at=seq(14,365,by=31), labels=Jul2JunLabels, tck=0)
        
        
        if(outputType=="image"){
          legend("topleft",legend= c("10-90%-ile","5-95%-ile","Median","2013-2014","2014-2015"),
                 fill = c(rgb(0.2,0.2,0.2,0.3),rgb(0.1,0.1,0.1,0.2), "black", "cornflowerblue", "coral3"),
                 cex=1,
                 bty="n",
                 ncol=5,
                 border="black")
        } else {
          legend(x=0,y=max(dq$p90)*1.4,legend= c("10-90%-ile","5-95%-ile","Median","2013-2014","2014-2015"),
                 fill = c(rgb(0.2,0.2,0.2,0.3),rgb(0.1,0.1,0.1,0.2), "black", "cornflowerblue", "coral3"),
                 cex=0.7,
                 bty="n",
                 ncol=4,
                 border="black")
          
        }
        
        ## PLOT 2 ## --------------------------------------------------------------------------------
        ## Daily Departure from median rainfall PLOT
        
        # Empty plot to add cumulative rainfall values to.
        plot(cumsum(y), type="n", axes=FALSE, xlab="", ylim=c(-700,700),ylab="Difference from median (mm)") # bty='n'
        title(main = "Accumulated difference from Daily Median Rainfall (mm)", font.main = 1.5)
        
        cx <- cumsum(x)-dq$p50[1:length(x)]   ## Timeseries data
        cxv <- as.vector(cx)                  ## Last years difference between median and the accumulating rainfall total
        
        cz <- cumsum(z)-dq$p50[1:length(z)]   ## Timeseries data
        czv <- as.vector(cz)                  ## This years difference between median and the accumulating rainfall total
        
        dczv <- data.frame(dfscan$sites[siteIndex],cday,czv)
        names(dczv) <- c("Sitename","Date","RainfallAccumulation")
        write.csv(dczv,paste("//ares/Environmental Archive/Resource/Rainfall Statistics/",as.character(dfscan$sites[siteIndex]),".csv",sep=""),row.names=FALSE)
        
        if(siteIndex==1){
          lastValues<-data.frame(dczv[length(dczv[,1]),])
        } else{
          lastValues<-rbind.data.frame(lastValues,dczv[length(dczv[,1]),])
        }
        
        points(seq(0,0,length.out=365),type="l", col="black", lwd=2, lty="solid")
        points(cxv,type="l", col="cornflowerblue", lwd=2, lty="solid")
        points(czv,type="l", col="coral3", lwd=2, lty="solid")
        
        i.for <- 1:365
        i.back <- 365:1
        
        x.polygon <- c( i.for, i.back )
        y1.polygon <- c( czv[i.for] , 0[i.back] )
        y1.polygon[(length(czv)+1):730] <- 0
        
        polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)
        
        axis(side=2, outer=FALSE)
        axis(side=1, at=seq(14,365,by=31), labels=Jul2JunLabels, tck=0)
        
        
        if(outputType=="image"){
          legend("topleft",legend= c("Median","2013-2014","2014-2015"),
                 fill = c("black", "cornflowerblue", "coral3"),
                 cex=1,
                 bty="n",
                 ncol=3,
                 border="black")
        } else {
          legend(x=0,y=700*1.2,legend= c("Median","2013-2014","2014-2015"),
                 fill = c("black", "cornflowerblue", "coral3"),
                 cex=1,
                 bty="n",
                 ncol=3,
                 border="black")
          
        }
        
        ## PLOT 3 ## --------------------------------------------------------------------------------
        ## Monthly Summary Rainfall PLOT
        
        barplot(as.matrix(bar),  beside = TRUE, col = c("cornsilk3", "cornflowerblue", "coral3"), 
                ylim = c(0, max(as.matrix(bar))*1.3), ylab="Monthly rain (mm)", border=TRUE)
        title(main = "Monthly Rainfall Summary", font.main = 1.5)
        box()
        
        if(outputType=="image"){
          legend("topleft", legend=c("Average","2013-2014","2014-2015"),
                 fill = c("cornsilk3", "cornflowerblue", "coral3"),
                 cex=1,
                 bty="n",
                 ncol=3,
                 border="black")
        } else {
          legend(x=0, y=max(as.matrix(bar))*1.45, legend=c("Average","2013-2014","2014-2015"),
                 fill = c("cornsilk3", "cornflowerblue", "coral3"),
                 cex=0.7,
                 bty="n",
                 ncol=3,
                 border="black")
          
        }
        #par(new=F)
        if(outputType=="image"){
          dev.off()
        }
        
        par(opar)
        
        ###### END #####
        
        
        ## End [myCodeBlock] ----------------
    }  
}

#output LastValues for upload by Hilltop
write.csv(lastValues,paste("//ares/Environmental Archive/Resource/Rainfall Statistics/rainfall_accumulation.csv",sep=""),row.names=FALSE)

## DEBUG SECTION FOR TESTING CODE AGAINST i=1

siteIndex <- 2
cat(as.character(dfscan$sites[siteIndex]),"\n")

## Start [myCodeBlock] ----------------
load(file=paste("10YR_qdata ",as.character(dfscan$sites[siteIndex]),".RData",sep=""))  ## this loads in the data quantile data.frame - dq
load(file=paste("10YR_rdata ",as.character(dfscan$sites[siteIndex]),".RData",sep=""))  ## this loads in rainfall series data.frame - df


## PROCESSING CURRENT YEAR RAINFALL RECORD -----------------------------------------
url2 <- paste("http://hilltopdev.horizons.govt.nz/boo.hts?service=Hilltop&request=GetData&Site=",as.character(dfscan$sites[siteIndex]),"&Measurement=Rainfall [SCADA Rainfall]&method=Total&interval=1 day&alignment=0:00:00&from=2014-07-02&to=2015-07-01",sep="")
getThisYear.xml <- xmlInternalTreeParse(url2)
csites<-sapply(getNodeSet(getThisYear.xml,"//Measurement/@SiteName"),as.character)


cday <- sapply(getNodeSet(getThisYear.xml, paste("//Hilltop/Measurement[@SiteName='",csites[1],"']/Data/E/../E/T",sep="")), xmlValue)
ctotal <- sapply(getNodeSet(getThisYear.xml, paste("//Hilltop/Measurement[@SiteName='",csites[1],"']/Data/E/../E/I1",sep="")), xmlValue)
csite <- rep(csites[1],length(cday))

ctotal<-as.numeric(ctotal)
cday<-as.Date(cday)
cday<-cday-1   ## Shift date back by one day. Rainfall is total to midnight and gets filed against yyyymmdd 24:00m, which is the same as yymmdd+1 00:00 

cdd<-as.numeric(format(cday,"%d"))
cmm<-as.numeric(format(cday,"%m"))
cyy<-as.numeric(format(cday,"%Y"))
cddmm<-as.character(format(cday,"%d-%B"))

cdf <- data.frame(cday,ctotal,csite,cddmm,cyy,cmm, stringsAsFactors=FALSE)
as.factor(cdf$cyy)

## PROCESSING LAST YEARS RAINFALL RECORD  -----------------------------------------
url3<- paste("http://hilltopdev.horizons.govt.nz/provisional.hts?service=Hilltop&request=GetData&Site=",as.character(dfscan$sites[siteIndex]),"&Measurement=Rainfall&method=Total&interval=1 day&alignment=0:00:00&&from=2013-07-02&to=2014-07-01",sep="")
getLastYear.xml <- xmlInternalTreeParse(url3)
lsites<-sapply(getNodeSet(getLastYear.xml,"//Measurement/@SiteName"),as.character)


lday <- sapply(getNodeSet(getLastYear.xml, paste("//Hilltop/Measurement[@SiteName='",lsites[1],"']/Data/E/../E/T",sep="")), xmlValue)
ltotal <- sapply(getNodeSet(getLastYear.xml, paste("//Hilltop/Measurement[@SiteName='",lsites[1],"']/Data/E/../E/I1",sep="")), xmlValue)
lsite <- rep(lsites[1],length(lday))

ltotal<-as.numeric(ltotal)
lday<-as.Date(lday)
lday<-lday-1   ## Shift date back by one day. Rainfall is total to midnight and gets filed against yyyymmdd 24:00m, which is the same as yymmdd+1 00:00 

ldd<-as.numeric(format(lday,"%d"))
lmm<-as.numeric(format(lday,"%m"))
lyy<-as.numeric(format(lday,"%Y"))
lddmm<-as.character(format(lday,"%d-%B"))

ldf <- data.frame(lday,ltotal,lsite,lddmm,lyy,lmm, stringsAsFactors=FALSE)
as.factor(ldf$lyy)

## PREPARE DATA FOR PLOTTING BARPLOT --------------------------------------------------------------
d <- read.zoo(df[ ,-3:-6]) # Removing unnecessary columns
e <- read.zoo(cdf[ ,-3:-6]) # Removing unnecessary columns
l <- read.zoo(ldf[ ,-3:-6]) # Removing unnecessary columns


# CUMULATIVE VALUS SELECTED YEARS
# Selecting last years time slice
x <- window(l, start=as.Date(paste(as.character(LastYear-1),"-07-01",sep="")), end=as.Date(paste(as.character(LastYear),"-06-30",sep="")))
index(x) <- 1:length(x)

# Selecting 2003-04 Flood Time slice
y <- window(d, start=as.Date("2003-07-01"), end=as.Date("2004-06-30"))
index(y) <- 1:length(y)

# Selecting 2014-15 year time slice
z <- window(e, start=as.Date("2014-07-01"))
index(z) <- 1:length(z)

# CALCULATING STATS OVER RECORD
# Grabbing 30 years of record
#y30 <- window(d, start=as.Date("1981-01-01"), end=as.Date("2010-12-31")) ## 10957 days
# Calculate monthly totals across the last thirty years
y10sum <- aggregate(d, as.Date(cut(time(d), "month")), sum)          ## 360 (12 months x 30yrs)
y10mean <- monthlyfunction(y10sum, FUN=mean, na.rm=TRUE)                 ## 12 rows
coredata(y10mean)<-coredata(y10mean)[c(7:12,1:6)]
index(y10mean)<-index(y10mean)[c(7:12,1:6)]

# Applying monthlyfunction to this season and last season
yNow  <- window(e, start=as.Date("2014-07-01"), end=as.Date("2015-06-30"))
yLast <- window(l, start=as.Date("2013-07-01"), end=as.Date("2014-06-30"))

yNowSum  <- as.data.frame(monthlyfunction(yNow, FUN=sum, na.rm=TRUE))
yLastSum <- as.data.frame(monthlyfunction(yLast, FUN=sum, na.rm=TRUE))
if(length(yNowSum)!=12){
  n<-as.numeric(length(yNowSum[,1]))
  for(i in (n+1):12){
    yNowSum[i, ]<-0
  }
}

## The next statements binds all vectors together into a dataframe. Each Vector/DF needs
## to be the same length. Might pay to check current year, and extend if necessary
bar<-as.data.frame(list(y10mean,yLastSum,yNowSum))
colnames(bar) <- c("Average","LastSeason","ThisYear")

# No data values for remaining months in year
#bar$ThisYear[10:12] <- 0

## This section can be modified to deal with dates over a Jul-Jun year
melt.bar<-melt(data.frame(month=rownames(bar), bar, stringsAsFactors=FALSE), id.vars="month")
melt.bar$monthnum <- c(1:12)

#melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun",
#                                                                                    "Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

Jul2JunLabels<-c("Jul","Aug","Sep","Oct","Nov","Dec",
                 "Jan","Feb","Mar","Apr","May","Jun")

melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=Jul2JunLabels,ordered=TRUE)

#casting as array
bar <- acast(melt.bar, variable ~ monthnum, sum, margins=FALSE)

## PLOTTING ROUTINE ----------------------------------------------------------------------
## NOTE: Using Base Graphics Package

## PLOT SETTINGS ## ----------------------------------------------------------------------
outputType <- "image" # c("screen","image")
opar<-par()

if(outputType=="image"){
  png(paste("//ares/Environmental Archive/Resource/Rainfall Statistics/",as.character(dfscan$sites[siteIndex]),".","PNG",sep=""),width=1024,height=680,units="px", res=96) ## ,quality=100 -- just for JPEGs
}

par(mfrow=c(3,1),mar=c(2,3,1,3)+0.1,cex=0.9, mgp=c(1.5,0.4,0.0),tck=1,tcl=-0.3)

## PLOT 1 ## --------------------------------------------------------------------------------
## Daily Cummulative Rainfall PLOT
plot(cumsum(y), type="n", axes=FALSE, ylim = c(0, max(dq$p90)*1.2), xlab="", ylab="Accumulated rain (mm)") # bty='n'
title(main = "July to June rainfall accumulation to date (mm)", font.main = 1.5)

i.for <- 1:365
i.back <- 365:1

x.polygon <- c( i.for, i.back )
y1.polygon <- c( dq$p90[i.for] , dq$p10[i.back] )
y2.polygon <- c( dq$p95[i.for] , dq$p5[i.back] )

polygon( x.polygon , y2.polygon , col=rgb(0.1,0.1,0.1,0.2, maxColorValue=1), border = NA)  ##  5-95th %-ile band
polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)  ## 10-90th %-ile band

points(dq$p50,type="l", col="black", lwd=2, lty="solid")   ## Plotting median
points(cumsum(x),type="l", col="cornflowerblue",lwd=2)     ## Plotting last year
points(cumsum(z),type="l", col="coral3", lwd=3)            ## Plotting this year

axis(side=2, outer=FALSE)
axis(side=1, at=seq(14,365,by=31), labels=Jul2JunLabels, tck=0)


if(outputType=="image"){
  legend("topleft",legend= c("10-90%-ile","5-95%-ile","Median","2013-2014","2014-2015"),
         fill = c(rgb(0.2,0.2,0.2,0.3),rgb(0.1,0.1,0.1,0.2), "black", "cornflowerblue", "coral3"),
         cex=1,
         bty="n",
         ncol=5,
         border="black")
} else {
  legend(x=0,y=max(dq$p90)*1.4,legend= c("10-90%-ile","5-95%-ile","Median","2013-2014","2014-2015"),
         fill = c(rgb(0.2,0.2,0.2,0.3),rgb(0.1,0.1,0.1,0.2), "black", "cornflowerblue", "coral3"),
         cex=0.7,
         bty="n",
         ncol=4,
         border="black")
  
}

## PLOT 2 ## --------------------------------------------------------------------------------
## Daily Departure from median rainfall PLOT

# Empty plot to add cumulative rainfall values to.
plot(cumsum(y), type="n", axes=FALSE, xlab="", ylim=c(-700,700),ylab="Difference from median (mm)") # bty='n'
title(main = "Accumulated difference from Daily Median Rainfall (mm)", font.main = 1.5)

cx <- cumsum(x)-dq$p50[1:length(x)]   ## Timeseries data
cxv <- as.vector(cx)                  ## Last years difference between median and the accumulating rainfall total

cz <- cumsum(z)-dq$p50[1:length(z)]   ## Timeseries data
czv <- as.vector(cz)                  ## This years difference between median and the accumulating rainfall total

dczv <- data.frame(dfscan$sites[siteIndex],cday,czv)
names(dczv) <- c("Sitename","Date","RainfallAccumulation")
write.csv(dczv,paste("//ares/Environmental Archive/Resource/Rainfall Statistics/",as.character(dfscan$sites[siteIndex]),".csv",sep=""),row.names=FALSE)

write.csv(dczv[length(dczv[,1]),],paste("//ares/Environmental Archive/Resource/Rainfall Statistics/rainfall_accumulation.csv",sep=""),append=TRUE,row.names=FALSE)
write.csv(dczv[length(dczv[,1]),],paste("Z:/data/RScript/cumulativeRainfall/rainfall_accumulation.csv",sep=""),row.names=FALSE,append=TRUE)

points(seq(0,0,length.out=365),type="l", col="black", lwd=2, lty="solid")
points(cxv,type="l", col="cornflowerblue", lwd=2, lty="solid")
points(czv,type="l", col="coral3", lwd=2, lty="solid")

i.for <- 1:365
i.back <- 365:1

x.polygon <- c( i.for, i.back )
y1.polygon <- c( czv[i.for] , 0[i.back] )
y1.polygon[(length(czv)+1):730] <- 0

polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)

axis(side=2, outer=FALSE)
axis(side=1, at=seq(14,365,by=31), labels=Jul2JunLabels, tck=0)


if(outputType=="image"){
  legend("topleft",legend= c("Median","2013-2014","2014-2015"),
         fill = c("black", "cornflowerblue", "coral3"),
         cex=1,
         bty="n",
         ncol=3,
         border="black")
} else {
  legend(x=0,y=700*1.2,legend= c("Median","2013-2014","2014-2015"),
         fill = c("black", "cornflowerblue", "coral3"),
         cex=1,
         bty="n",
         ncol=3,
         border="black")
  
}

## PLOT 3 ## --------------------------------------------------------------------------------
## Monthly Summary Rainfall PLOT

barplot(as.matrix(bar),  beside = TRUE, col = c("cornsilk3", "cornflowerblue", "coral3"), 
        ylim = c(0, max(as.matrix(bar))*1.3), ylab="Monthly rain (mm)", border=TRUE)
title(main = "Monthly Rainfall Summary", font.main = 1.5)
box()

if(outputType=="image"){
  legend("topleft", legend=c("Average","2013-2014","2014-2015"),
         fill = c("cornsilk3", "cornflowerblue", "coral3"),
         cex=1,
         bty="n",
         ncol=3,
         border="black")
} else {
  legend(x=0, y=max(as.matrix(bar))*1.45, legend=c("Average","2013-2014","2014-2015"),
         fill = c("cornsilk3", "cornflowerblue", "coral3"),
         cex=0.7,
         bty="n",
         ncol=3,
         border="black")
  
}
#par(new=F)
if(outputType=="image"){
  dev.off()
}

par(opar)

###### END #####


## End [myCodeBlock] ----------------



