
####################################################################################
#
#     THIS IS ONE OF TWO SCRIPTS THAT ARE RUN TO DETERMINE DAILY DIFFERENCES AND
#         DAILY CUMULATIVE DIFFERENCES FOR MEDIAN RAINFALL AT A DAILY SCALE.
#
#                 *    DailyRainfall_CreateReferenceData_10yrs.R   *
#                   *    DailyRainfall_Accumulation_Outputs.R    *
#
#####################################################################################
#
#  Name:       DailyRainfall_Accumulation_Outputs.R
#
#  Purpose:    Using pre-determined reference data:
#              1. Retrieve annual data for current and previous Jul-Jun period
#              2. Plot data
#              3. Generate csv's for loading to other systems
#
#  Output:     1. Plots:
#                - Rainfall accumulation to date
#                - Difference to median rainfall
#                - Monthly rainfall summary
#              2. Files
#                - Rainfall accumulation and difference CSV files
#
#  Notes:      1. Written for data in a HilltopServer environment
#              2. Run daily to update plots and csv's
#              3. Code update required after 30-June-2015
#
#
#  Created by: seanlhodges
#  Created on: 27-January-2015
#
#####################################################################################


# Load requried libraries

library(XML)
library(reshape2)
library(zoo)
library(hydroTSM)

### Declare variables --------------------

# Server
myServer  <- "http://hilltopdev.horizons.govt.nz/"     ## Server address
telemetry <- "telemetry.hts"                           ## Telemetry file name registered on the server 
archive   <- "archive.hts"                         ## Archive file name registered on the server
myCollctn <- "zVirtual Rainfall"                     ## Hilltop Collection providing rainfall sitenames:
                                                       ##   Collection needs to be in default project
                                                       ##   file on your Hilltop Server instance

# Measurements
om_tlm_rain  <- "Rainfall [SCADA Rainfall]"               ## Rainfall measurement used in Telemetry file
om_arc_rain  <- "Rainfall [Rainfall]"                     ## Rainfall measurement used in Archive

# File output locations
RDataFilePath <- "//ares/environmental archive/resource/Rainfall Statistics/R/"     ## File path - note the forward slashes: 
csvFilePath   <- "//ares/environmental archive/resource/rainfall statistics/"       ##    R treats a backslash as an escape
imgFilePath   <- csvFilePath                                                        ##    character.
#                                                                                   ##    End path string with foward slash.

# Other
yearStart  <- 2003
LastYear   <- 2014
generateSiteFiles<- TRUE


## STEP 1 - Load Site Reference data
load(file=paste(RDataFilePath,"10YR_RefData_SiteTable.RData",sep=""))  ## this loads in sites data.frame - dfscan

for(siteIndex in 1:length(dfscan[,1])){

  load(file=paste(RDataFilePath,"10YR_qdata ",as.character(dfscan$sites[siteIndex]),".RData",sep=""))  ## this loads in the data quantile data.frame - dq
  load(file=paste(RDataFilePath,"10YR_rdata ",as.character(dfscan$sites[siteIndex]),".RData",sep=""))  ## this loads in rainfall series data.frame - df
  
  if(length(dq[,1])!=0){
    cat(as.character(dfscan$sites[siteIndex]),"\n")
    ## PROCESSING CURRENT YEAR RAINFALL RECORD -----------------------------------------
    url2 <- paste(myServer,telemetry,"?service=Hilltop&request=GetData&Site=",as.character(dfscan$sites[siteIndex]),"&Measurement=",om_tlm_rain,"&method=Total&interval=1 day&alignment=0:00:00&from=2014-07-02&to=2015-07-01",sep="")
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
    url3<- paste(myServer,archive,"?service=Hilltop&request=GetData&Site=",as.character(dfscan$sites[siteIndex]),"&Measurement=",om_arc_rain,"&method=Total&interval=1 day&alignment=0:00:00&&from=2013-07-02&to=2014-07-01",sep="")
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
        png(paste(imgFilePath,as.character(dfscan$sites[siteIndex]),".","PNG",sep=""),width=1024,height=680,units="px", res=96) ## ,quality=100 -- just for JPEGs
      }
      
      #par(mfrow=c(3,1),mar=c(2,3,1,3)+0.1,cex=0.9, mgp=c(1.5,0.4,0.0),tck=1,tcl=-0.3)
      #par(mar=c(2,3,1,3)+0.1,cex=0.9, mgp=c(1.5,0.4,0.0),tck=1,tcl=-0.3)
      par(mar=c(2,4,3,3)+0.1,oma=c(0,0,1.5,0),cex=0.9,tck=1,tcl=-0.3)
      
      layout(matrix(c(1,1,2,2,3,3),3,2,byrow = TRUE), heights= c(2,1,1), TRUE)
      
     
      ## PLOT 1 ## --------------------------------------------------------------------------------
      ## Daily Cummulative Rainfall PLOT

      plot(cumsum(y), type="n", axes=FALSE, ylim = c(0, max(dq$p90)*1.2), xlab="", ylab="Accumulated rain (mm)") # bty='n'
      title(main = "July to June rainfall accumulation to date (mm)", font.main = 1.5)
      
      i.for <- 1:365
      i.back <- 365:1
      
      x.polygon <- c( i.for, i.back )
      y1.polygon <- c( dq$p90[i.for] , dq$p10[i.back] )
      y2.polygon <- c( dq$p95[i.for] , dq$p5[i.back] )
      
      polygon( x.polygon , y2.polygon , col="grey55", border = NA)  ##  5-95th %-ile band   DARK GREY
      polygon( x.polygon , y1.polygon , col="grey85", border = NA)  ## 10-90th %-ile band   LIGHT GREY
      
      
      
      points(dq$p50,type="l", col="black", lwd=2, lty="solid")   ## Plotting median
      points(cumsum(x),type="l", col="cornflowerblue",lwd=2)     ## Plotting last year
      points(cumsum(z),type="l", col="coral3", lwd=3)            ## Plotting this year
      
      axis(side=2, outer=FALSE)
      axis(side=1, at=seq(14,365,by=31), labels=Jul2JunLabels, tck=0)
      
      
      if(outputType=="image"){
        legend("topleft",legend= c("10-90%-ile","5-95%-ile","10 yr Median","2013-2014","2014-2015"),
               fill = c("grey85","grey55", "black", "cornflowerblue", "coral3"),
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
       
      cm <-   cumsum(z) / dq$p50[1:length(z)]     
      cmv <- as.vector(cm)                  ## This years difference between median and the accummulating rainfall as a %
      
      
      dczv <- data.frame(dfscan$sites[siteIndex],cday,czv,cmv)
      names(dczv) <- c("Sitename","Date","RainfallAccumulation","RainfallAccumPct")
      
      ## Create Site files of rainfall data for the current Jul-Jun year
      if(generateSiteFiles){
        write.csv(dczv,paste(csvFilePath,as.character(dfscan$sites[siteIndex]),".csv",sep=""),row.names=FALSE)
      }
      
      if(siteIndex==1){
        lastValues<-data.frame(dczv[length(dczv[,1]),])
      } else{
        lastValues<-rbind.data.frame(lastValues,dczv[length(dczv[,1]),])
      }
      
      points(seq(0,0,length.out=365),type="l", col="black", lwd=2, lty="solid")
      points(cxv,type="l", col="cornflowerblue", lwd=2, lty="solid")
      points(czv,type="l", col="coral3", lwd=2, lty="solid")
      
      #i.for <- 1:365
      #i.back <- 365:1
      
      # adjusting polygon to give a vertical line back to  zero
      i.for <- c(1:length(czv),length(czv),(length(czv)+2):365)
      i.back <-c(365:(length(czv)+2),length(czv),length(czv):1)
      
      x.polygon <- c( i.for, i.back )
      y1.polygon <- c( czv[i.for] , 0[i.back] )
      y1.polygon[(length(czv)+1):730] <- 0
      
      polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)
      
      axis(side=2, outer=FALSE)
      axis(side=1, at=seq(14,365,by=31), labels=Jul2JunLabels, tck=0)
      
      
      if(outputType=="image"){
        legend("topleft",legend= c("10yr Median","2013-2014","2014-2015"),
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
        legend("topleft", legend=c("10yr Average","2013-2014","2014-2015"),
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

      ## PLOT TITLE ###
      mtext(dfscan$sites[siteIndex],outer=TRUE)
      
      #par(new=F)
      if(outputType=="image"){
        dev.off()
      }
      
      par(opar)
      
    } ## END IF statement regarding number of days in last years data  
  } ## END IF Statement regarding legnth of dq
  
  # Clearing out site data loaded through by load()
  rm(dq,df)
  
} ##  END FOR loop for iterating through sites in dfscan


#output LastValues for upload by Hilltop
write.csv(lastValues,paste(csvFilePath,"rainfall_accumulation.csv",sep=""),row.names=FALSE)
