library(shiny)
library(XML)
library(reshape2)
library(zoo)
library(hydroTSM)

#===================================================================================================
## ===============================================================================
## Getting Site Data
s <- Sys.time()
#HSERVER<-"hilltopdev"
HSERVER<-c("hilltopdev")
RSite<-c("Pohangina at Makawakawa Divide")

LastYear<-2014

cumulMax<-6000

period<-"AVAILABLE" # "THIRTY YEARS" 

###########################################################
## PROCESSING FULL RAINFALL RECORD
url<- paste("http://",HSERVER,".horizons.govt.nz/archive.hts?service=Hilltop&request=GetData&Site=",RSite,"&Measurement=Rainfall&method=Total&interval=1 day&alignment=0:00:00&&from=1981-01-02&to=2012-01-01",sep="")
getData.xml <- xmlInternalTreeParse(url)
sites<-sapply(getNodeSet(getData.xml,"//Measurement/@SiteName"),as.character)

day <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement[@SiteName='",RSite,"']/Data/E/../E/T",sep="")), xmlValue)
total <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement[@SiteName='",RSite,"']/Data/E/../E/I1",sep="")), xmlValue)
site <- rep(sites[1],length(day))

total<-as.numeric(total)
day<-as.Date(day)
day<-day-1   ## Shift date back by one day. Rainfall is total to midnight and gets filed against yyyymmdd 24:00m, which is the same as yymmdd+1 00:00 
dd<-as.numeric(format(day,"%d"))
mm<-as.numeric(format(day,"%m"))
yy<-as.numeric(format(day,"%Y"))
ddmm<-as.character(format(day,"%d-%B"))

df <- data.frame(day,total,site,ddmm,yy,mm, stringsAsFactors=FALSE)
as.factor(df$yy)

###########################################################
## PROCESSING CURRENT YEAR RAINFALL RECORD
url2 <- paste("http://",HSERVER,".horizons.govt.nz/boo.hts?service=Hilltop&request=GetData&Site=",RSite,"&Measurement=Rainfall [SCADA Rainfall]&method=Total&interval=1 day&alignment=0:00:00&from=2014-07-02&to=2015-07-01",sep="")
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

###########################################################
## PROCESSING LAST YEARS RAINFALL RECORD
url3<- paste("http://",HSERVER,".horizons.govt.nz/archive.hts?service=Hilltop&request=GetData&Site=",RSite,"&Measurement=Rainfall&method=Total&interval=1 day&alignment=0:00:00&&from=2013-07-02&to=2014-07-01",sep="")
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# REFERENCE DATA CALCULATION
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CALCULATION OF LONG TERM RAINFALL QUANTILES FOR LENGTH OF AVAILABLE RECORD - 10%, 50% and 90%
## for each year, calculate cumulative rainfalls against each day, and store in matrix
## Create Matrix - 30 years by 366 days
md0 <- matrix(data = NA, nrow = 365, ncol = 31, byrow = FALSE,
              dimnames = NULL)
## Load matrix with cummulative rainfalls
j <- 1


## Populate Matrix with Jan-Dec data for each year
for(x in 1:length(unique(df$yy))) {
  ds<-subset(df,yy==unique(df$yy)[x])
  a<-ds$total
  cat(x,length(a),"\n")
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

# DONE
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



##############################################################################################
# PREPARE DATA FOR PLOTTING BARPLOT
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
y30sum <- aggregate(d, as.Date(cut(time(d), "month")), sum)          ## 360 (12 months x 30yrs)
y30mean <- monthlyfunction(y30sum, FUN=mean, na.rm=TRUE)                 ## 12 rows
coredata(y30mean)<-coredata(y30mean)[c(7:12,1:6)]
index(y30mean)<-index(y30mean)[c(7:12,1:6)]

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
bar<-as.data.frame(list(y30mean,yLastSum,yNowSum))
colnames(bar) <- c("Average","LastSeason","ThisYear")

# No data values for remaining months in year
#bar$ThisYear[10:12] <- 0

## This section can be modified to deal with dates over a Jul-Jun year
melt.bar<-melt(data.frame(month=rownames(bar), bar, stringsAsFactors=FALSE), id.vars="month")
melt.bar$monthnum <- c(1:12)

#melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun",
#                                                                                    "Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)

melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=c("Jul","Aug","Sep","Oct","Nov","Dec",
                                                                                    "Jan","Feb","Mar","Apr","May","Jun"),ordered=TRUE)

#casting as array
bar <- acast(melt.bar, variable ~ monthnum, sum, margins=FALSE)

############################################################################################
## PLOTTING ROUTINE
## NOTE: Using Base Graphics Package


opar<-par()
par(mfrow=c(3,1),mar=c(2,3,1,3)+0.1,cex=0.9, mgp=c(1.5,0.4,0.0),tck=1,tcl=-0.3)

## PLOT 1 ## --------------------------------------------------------------------------------
## Daily Cummulative Rainfall PLOT
plot(cumsum(y), type="n", axes=FALSE, ylim = c(0, max(dq$p90)*1.2), xlab="", ylab="Accumulated rain (mm)") # bty='n'
title(main = "Accumulated rainfall to date (mm)", font.main = 1.5)

i.for <- 1:365
i.back <- 365:1

x.polygon <- c( i.for, i.back )
y1.polygon <- c( dq$p90[i.for] , dq$p10[i.back] )
y2.polygon <- c( dq$p95[i.for] , dq$p5[i.back] )
#polygon( x.polygon , y2.polygon , col=rgb(0.2,0.2,0.2,0.2, maxColorValue=1), border = NA)
#polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.1, maxColorValue=1), border = NA)
polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)

points(dq$p50,type="l", col="black", lwd=2, lty="solid")

points(cumsum(x),type="l", col="cornflowerblue",lwd=2)     ## Plotting last year
#points(cumsum(y),type="l", col="red", lwd=2)    ## Plotting 2003-2004 year

points(cumsum(z),type="l", col="coral3", lwd=3)   ## Plotting this year

axis(side=2, outer=FALSE)


legend(x=0,y=max(dq$p90)*1.4,legend= c("10-90%-ile","Median",as.character(LastYear),"This year"),
       fill = c(rgb(0.2,0.2,0.2,0.3), "black", "cornflowerblue", "coral3"),
       cex=0.7,
       bty="n",
       ncol=4,
       border="black")


## PLOT 2 ## --------------------------------------------------------------------------------
## Daily Departure from median rainfall PLOT

# Empty plot to add cumulative rainfall values to.
plot(cumsum(y), type="n", axes=FALSE, xlab="", ylim=c(-500,500),ylab="Difference from median (mm)") # bty='n'
title(main = "Accumulated differences from Daily Median Rainfall to date (mm)", font.main = 1.5)

cz <- cumsum(z)-dq$p50[1:length(z)]
czv <- as.vector(cz)

points(seq(0,0,length.out=365),type="l", col="black", lwd=2, lty="solid")
points(czv,type="l", col="coral3", lwd=2, lty="solid")

i.for <- 1:365
i.back <- 365:1

x.polygon <- c( i.for, i.back )
y1.polygon <- c( czv[i.for] , 0[i.back] )
y1.polygon[(length(czv)+1):730] <- 0

polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)

axis(side=2, outer=FALSE)

## PLOT 3 ## --------------------------------------------------------------------------------
## Monthly Summary Rainfall PLOT

barplot(as.matrix(bar),  beside = TRUE, col = c("cornsilk3", "cornflowerblue", "coral3"), 
        ylim = c(0, max(as.matrix(bar))*1.3), ylab="Monthly rain (mm)", border=TRUE)
title(main = "Monthly Rainfall Summary", font.main = 1.5)
box()

legend(x=0, y=max(as.matrix(bar))*1.5, legend=c("Average","Last Year","This Year"),
      fill = c("cornsilk3", "cornflowerblue", "coral3"),
      cex=0.7,
      bty="n",
      ncol=3,
      border="black")

par(new=F)
par(opar)

print(Sys.time()-s)