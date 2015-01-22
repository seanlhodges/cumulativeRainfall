#####################################################################################################
#  Name:       30 year rainfall normals
#  Desc:       Calculate daily average rainfall based on on 30 years of record for sites with 
#              sufficient available data
#  Created by: seanlhodges
#  Created on: 7 September 2014
#  Dependen.:  1. OGC compliant data servers providing waterml2.0 data streams, and supporting
#                 SOS2 requests
#              2. A priori knowledge of valid featuresOfInterest and observedProperty's
#              3. 
#####################################################################################################


# Load requried libraries

library(XML)
library(reshape2)

# Declare constants



# functions



# Main



#===================================================================================================
## ===============================================================================
## Getting Site Data
#s <- Sys.time()
#ogcServer<-c("http://hilltopserver.horizons.govt.nz/data.hts?")

###########################################################
## Pulling Rainfall Record
##
## Using server API to determine available rainfall sites

# Based on the collection for rainfall, specify URL for 30 years
# of daily rainfall record for every active rainfall site
url <- "http://hilltopdev.horizons.govt.nz/archive.hts?service=Hilltop&request=GetData&Collection=zVirtual%20Rainfall&interval=1%20day&method=Total&from=1981-01-01&to=2011-01-01"

getData.xml <- xmlInternalTreeParse(url)
sites<-sapply(getNodeSet(getData.xml,"//Measurement/@SiteName"),as.character)

minday <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement/Data/E[1]/T",sep="")), xmlValue)
maxday <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement/Data/E[last()]/T",sep="")), xmlValue)

minday <- as.POSIXct(strptime(minday,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))
maxday <- as.POSIXct(strptime(maxday,format="%Y-%m-%dT%H:%M:%S",tz="Pacific/Auckland"))



# Setting flags for those sites that have a start date matching dataStart i.e have 30 years record
flag30 <- (minday-dataStart) == 0

# Create a dataframe to summarise what sites can be used for 30 year statistics
dfscan <- data.frame(sites,flag30,minday,maxday)
# A bit of a tidy up
rm(sites,flag30,minday,maxday,dataStart,url)


# Using the list of sites, and constructing the SOS2.0 requests to deliver the data
# as WaterML2

for(i in 1:length(dfscan[,1])){
  url1 <- "http://hilltopdev.horizons.govt.nz/archive.hts?service=SOS&request=GetObservation&FeatureOfInterest="
  url2 <- "&observedProperty=Daily Rainfall&temporalFilter=om:phenomenon,1981-01-01/2011-01-01"

  #i <- 1
  if(dfscan$flag30[i]==TRUE){
    # construct SOS call
    url <- paste(url1,dfscan$sites[i],url2,sep="")
    getData.xml <- xmlInternalTreeParse(url)
    
    day <- sapply(getNodeSet(getData.xml, paste("//wml2:point/wml2:MeasurementTVP/wml2:time",sep="")), xmlValue)
    total <- sapply(getNodeSet(getData.xml, paste("//wml2:point/wml2:MeasurementTVP/wml2:value",sep="")), xmlValue)
    site <- rep(dfscan$sites[i],length(day))

    day<-as.Date(day)
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
    
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # REFERENCE DATA CALCULATION
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # CALCULATION OF LONG TERM RAINFALL QUANTILES FOR LENGTH OF AVAILABLE RECORD - 10%, 50% and 90%
    ## for each year, calculate cumulative rainfalls against each day, and store in matrix
    ## Create Matrix - 30 years by 366 days
    md <- matrix(data = NA, nrow = 365, ncol = 30, byrow = FALSE,
                 dimnames = NULL)
    ## Load matrix with cummulative rainfalls
    j <- 1
    for(x in 1:length(unique(df$yy))) {
      #print(unique(df$yy)[i])
      ds<-subset(df,yy==unique(df$yy)[x])
      a<-ds$total

      ## Adjust for leap years
      ## - Removing value for the the leap-day, Feb-29, which is the 60th day of a leap year.
      
      if(unique(df$yy)[x] %% 4 == 0) {   ## If year a leap year [this is valid for range of years expected],
        a <- a[-60]                      ## remove Feb-29
      }
      
      
      # Reorder days in year based on a Water year (http://en.wikipedia.org/wiki/Water_year)
      # - The United States Geological Survey defines it as the period between October 1st 
      #   of one year and September 30th of the next
      
      if(length(a)==365){
        # Jul to Jun Year
        a <- a[c(182:365,1:181)]
        # USGS Water year
        a <- a[c(274:365,1:273)]
        
        
        ## adding a place holder at the end of the year to make sure each year vector
        ## is 366 items long - giving it a value of zero so not to affect cumulative
        ## totals in cumulative totals
        #if (length(a)==365)  a<-c(a,0)
        ## Accumulate rainfalls and add to matrix
        md[ , j]<-cumsum(a)
        j<-j+1
      }
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
    

    # save site reference data to project folder
    save(dq,file=paste("refdata",as.character(dfscan$sites[i]),".RData",sep=""))
      

    # DONE
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
  }


}



