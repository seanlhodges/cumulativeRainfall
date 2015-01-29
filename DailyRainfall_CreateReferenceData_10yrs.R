#####################################################################################
#
#     THIS IS ONE OF TWO SCRIPTS THAT ARE RUN TO DETERMINE DAILY DIFFERENCES AND
#         DAILY CUMULATIVE DIFFERENCES FOR MEDIAN RAINFALL AT A DAILY SCALE.
#
#                 *    DailyRainfall_CreateReferenceData_10yrs.R   *
#                   *    DailyRainfall_Accumulation_Outputs.R    *
#
#####################################################################################
#  
#  Name:       DailyRainfall_CreateReferenceData_10yrs.R
#
#  Purpose:    1. Determine those rainfall sites that have 10years+ available data
#              2. Retrieve 10years daily rainfall totals
#              3. Organise data into Jul-Jun years for output as reference data
#              4. Calculate specified quantiles and output as reference data
#
#  Output:     Site-based reference data files:
#                - Daily quantiles 5,10,50,90,95
#                - Daily rainfall totals over 10 years
#
#  Notes:      1. Written for data in a HilltopServer environment
#              2. Run as needed to update reference data. If you are working with
#                 a fixed reference for time period, should only need to be run once, 
#                 unless new sites are added to your archive at a later date.
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
archive   <- "archive.hts"                             ## Archive file name registered on the server
myCollctn <- "zVirtual Rainfall"                       ## Hilltop Collection providing rainfall sitenames:
#                                                      ##   Collection needs to be in default project
#                                                      ##   file on your Hilltop Server instance

# Measurements
om_tlm_rain  <- "Rainfall [SCADA Rainfall]"               ## Complete Rainfall measurement spec used in Telemetry file
om_arc_rain  <- "Rainfall [Rainfall]"                     ## Complete Rainfall measurement spec used in Archive

# File output locations
RDataFilePath <- "//ares/environmental archive/resource/Rainfall Statistics/R/"     ## File path - note the forward slashes: 
csvFilePath   <- "//ares/environmental archive/resource/rainfall statistics/"       ##    R treats a backslash as an escape
imgFilePath   <- csvFilePath                                                        ##    character.
#                                                                                   ##    End path string with foward slash.

# Other
yearStart  <- 2003

#Extract datasource from om_arc_rain string to use in querying XML later on
a<-regexpr("\\[(.*?)\\]",om_arc_rain)
# adjusting object to extract string inside square brackets
a[1] <- a[1]+1
attr(a,"match.length") <- attr(a,"match.length")-2

datasource <- regmatches(om_arc_rain,a)


## CALCULATE 10-year averages ----------------------------------------------------------------------------

## STEP 1 - Using server API to determine available rainfall sites ---------------------------------------
# Based on the collection for rainfall, specify URL for 10 years
# of daily rainfall record for every active rainfall site

startYear <- paste(yearStart,"-01-01T00:00:00",sep="") 
dataStart <- as.POSIXct(strptime(startYear,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))

endYear   <- paste(yearStart+11,"-01-01T00:00:00",sep="") 
dataEnd  <- as.POSIXct(strptime(endYear,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))


# Build url to return all collections 
url <- paste(myServer,archive,"?service=Hilltop&request=CollectionList",sep="")

# Call url and store xml
getData.xml <- xmlInternalTreeParse(url)
# parse the list of sites
sites <- sapply(getNodeSet(getData.xml, paste("//HilltopProject/Collection[@Name='",myCollctn,"']/Item/SiteName",sep="")), xmlValue)


# With the list of sites returned by the collection, call each one and interrogate
# response given by:
#      service=Hilltop&request=MeasurementList&Site=<sitename>

for(site in 1:length(sites)){
  
  url <- paste(myServer,archive,"?service=Hilltop&request=MeasurementList&site=",sites[site],sep="")
  getData.xml <- xmlInternalTreeParse(url)
  
  
  # First match for these xpath statements will be for tsStd data series
  cat.DateStart <- sapply(getNodeSet(getData.xml, paste("//HilltopServer/DataSource[@Name='",datasource,"'][1]/From",sep="")), xmlValue)
  cat.DateEnd   <- sapply(getNodeSet(getData.xml, paste("//HilltopServer/DataSource[@Name='",datasource,"'][1]/To",sep="")), xmlValue)
  
  # convert text to POSIX date
  cat.DateStart <- as.POSIXct(strptime(cat.DateStart,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))
  cat.DateEnd   <- as.POSIXct(strptime(cat.DateEnd,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))
  
  # build vectors for min and max dates
  if(site==1){
    minday <- cat.DateStart
    maxday <- cat.DateEnd
  } else{
    minday <- c(minday, cat.DateStart)
    maxday <- c(maxday, cat.DateEnd)
  }
  
  rm(cat.DateStart, cat.DateEnd)
  
}


# Setting flags for those sites that have a start date matching dataStart i.e have 10 years record
flag10 <- (minday-dataStart) <= 0

# Create a dataframe to summarise what sites can be used for 10 year statistics
dfscan <- data.frame(sites,flag10,minday,maxday)
# Filter based on flag10=TRUE and maxday=dataEnd
dfscan <- subset(dfscan,dfscan$flag10==TRUE)
dfscan <- subset(dfscan,dfscan$maxday>=dataEnd)

# A bit of a tidy up
rm(sites,flag10,minday,maxday,dataStart,url,getData.xml)


## STEP 2 - RESTful call for data from server to create 10 year data matrix ---------------------------------------
# Using the list of sites, and constructing the requests to deliver the data

for(i in 1:length(dfscan[,1])){
    
  
    url1 <- paste(myServer,archive,"?service=Hilltop&request=GetData&Site=",sep="")
    url2 <- paste("&measurement=",om_arc_rain,"&interval=1 Day&method=Total&from=",yearStart,"-01-02&to=",yearStart+11,"-01-01",sep="")

    url <- paste(url1,as.character(dfscan$sites[i]),url2,sep="")
    getData.xml <- xmlInternalTreeParse(url)

  
    # if a site doesn't have too many gaps in its data then continue processing, else skip it.
    if(length(sapply(getNodeSet(getData.xml, "//Data/Gap"), xmlValue))<=30){
    
        day <- sapply(getNodeSet(getData.xml, "//Data/E/T"), xmlValue)
        total <- sapply(getNodeSet(getData.xml, "//Data/E/I1"), xmlValue)
        
        site <- rep(dfscan$sites[i],length(day))
        
        day<-as.Date(day)
        day<-day-1   ## Shift date back by one day. Rainfall is total to midnight and gets filed against yyyymmdd 24:00m, which is the same as yymmdd+1 00:00 
        
        total<-as.numeric(total)
        
        # creating var's for each day, month and year for running stats later
        dd<-as.numeric(format(day,"%d"))
        mm<-as.numeric(format(day,"%m"))
        yy<-as.numeric(format(day,"%Y"))
        ddmm<-as.character(format(day,"%d-%B"))  ## allows derivation of statistics about an
        ## individual day over many years eg 23-May
        
        # converting data to a data.frame
        df <- data.frame(day,total,site,ddmm,yy,mm, stringsAsFactors=FALSE)
        as.factor(df$yy) 
        
        rm(day, total, site, dd, mm, yy, ddmm, url)
        
        ## STEP 3 - REFERENCE DATA CALCULATION 
        
        # CALCULATION OF RAINFALL QUANTILES FOR LENGTH OF REQUESTED RECORD - 10%, 50% and 90%
        ## for each year, calculate cumulative rainfalls against each day, and store in matrix
        ## Create Matrix - 11 years by 366 days 
        md0 <- matrix(data = NA, nrow = 365, ncol = 11, byrow = FALSE,
                     dimnames = NULL)
        ## Load matrix with cummulative rainfalls
        j <- 1
        #cat(url,"\n")
        #cat(as.character(dfscan$sites[i]),"\n")
        ## Populate Matrix with Jan-Dec data for each year
        for(x in 1:length(unique(df$yy))) {
    
          ds<-subset(df,yy==unique(df$yy)[x])
          a<-ds$total
          #cat(x,length(a),"\n")
          if(unique(df$yy)[x] %% 4 == 0) {   ## If year a leap year [this is valid for range of years expected],
            a <- a[-60]                      ## remove Feb-29
          }
          
          # where a full year's record is not available (typically the last year of the call), fill the rest of the day valuse
          # for the year with NAs
          if(length(a)<365){
            b<-length(a)+1
            a[b:365]<-NA
          }
          # Populate Matrix
          md0[ , j]<-(a)
          j<-j+1
        }
        
        md1 <- md0
        ##  Rearrange array - 
        ##    Move "n" year jan-Jun to "n-1" year jan-jun
        ##    --FOR FUTURE DEVELOPMENT, COULD SPECIFY DIFFERENT WATER YEARS AT THIS STEP--
        for(x in 2:length(md1[1,])) {        # Stepping across columns
          md1[1:181,x-1] <- md1[1:181,x]      # Moving Jan-Jun period back one year in preparation for reordering year to Jul-Dec,Jan-Jun
          md1[,x-1] <- md1[c(182:365,1:181),x-1]  # Reorder column to Jul-Dec,Jan-Jun
        }
           
        ## Drop last year in matrix
        md2 <- md1[,-(length(md1[1,]))]
        
                 
        md <- matrix(data = NA, nrow = 365, ncol = length(md2[1,]), byrow = FALSE,
                     dimnames = NULL) 
        ## adding a place holder at the end of the year to make sure each year vector
        ## is 366 items long - giving it a value of zero so not to affect cumulative
        ## totals in cumulative totals
        #if (length(a)==365)  a<-c(a,0)
        
        ## Accumulate rainfalls within matrix
        for(x in 1:length(md2[1,])) { 
            md[ , x]<-cumsum(md2[ , x])
        }
        
        
        # Calculate quantile values for each day
        for(x in 1:365) {
          if(x==1){
            dq<-data.frame(quantile(md[x, ],prob=c(0.05,0.1,0.5,0.9,0.95),na.rm=TRUE),c(5,10,50,90,95),x)
          } else {
            q<-data.frame(quantile(md[x, ],prob=c(0.05,0.1,0.5,0.9,0.95),na.rm=TRUE),c(5,10,50,90,95),x)
            dq<-rbind(dq,q)
          }
        }
    
        #Rename columns in data.frame
        colnames(dq)<-c("cumulrain","quantile","day")
        
        #reshape for plotting
        #casting as data.frame i.e. Pivot by day x quantile, with values of cumulrain
        dq <- dcast(dq, day ~ quantile, sum, value.var="cumulrain", margins=FALSE)
        #Rename columns in data.frame
        colnames(dq)<-c("day","p5","p10","p50","p90","p95")
        
        # STEP 4 - SAVE SITE REFERENCE DATA to project folder
        save(dq,file=paste(RDataFilePath,"10YR_qdata ",as.character(dfscan$sites[i]),".RData",sep="")) ## accumulated quantile records
        save(df,file=paste(RDataFilePath,"10YR_rdata ",as.character(dfscan$sites[i]),".RData",sep="")) ## Rainfall totals
        
    } else { dfscan$flag10[i] <- FALSE } ## end if for Sites with GAPS. If gaps found, reset flat to FALSE
  
}  ## End for loop for review and stat summary for each site

# Final filter and save based on discovered gaps

# Filter based on flag10=TRUE
dfscan <- subset(dfscan,dfscan$flag10==TRUE)

## Storing Site Reference data for the daily calculation of rainfall departures
save(dfscan,file=paste(RDataFilePath,"10YR_RefData_SiteTable.RData",sep="")) ## Rainfall totals


