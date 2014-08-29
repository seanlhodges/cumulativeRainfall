library(shiny)
library(XML)
library(reshape2)
library(zoo)
library(hydroTSM)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  

#===================================================================================================
## ===============================================================================
## Getting Site Data
#s <- Sys.time()
#HSERVER<-"hilltopdev"
HSERVER<-c("hilltopserver")
#RSite<-c("Pohangina at Makawakawa Divide")
#RSite<-c("Oroua at Rangiwahia")
#RSite<-c("Akitio at Toi Flat")
#pickYear<-1987

# Generate a plot of the requested variable against mpg and only 
# include outliers if requested
output$dodgebarplot <- renderPlot({
  
  cumulMax<-as.numeric(input$cumulymax)
  barPlotMax<-cumulMax/5
  
  if(input$recLength=="Available record"){
    availableRecord <- TRUE
  } else {
    availableRecord <- FALSE
  }
  
  ###########################################################
  ## PROCESSING FULL RAINFALL RECORD
  url <- paste("http://",HSERVER,".horizons.govt.nz/archive.hts?service=Hilltop&request=GetData&Site=",input$raingauge,"&Measurement=Rainfall [Rainfall]&method=Total&interval=1 day&to=2013-06-30",sep="")

  getData.xml <- xmlInternalTreeParse(url)
  cat(url,"\n")

  sites<-sapply(getNodeSet(getData.xml,"//Measurement/@SiteName"),as.character)
  
  day <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement[@SiteName='",sites[1],"']/Data/E/../E/T",sep="")), xmlValue)
  total <- sapply(getNodeSet(getData.xml, paste("//Hilltop/Measurement[@SiteName='",sites[1],"']/Data/E/../E/I1",sep="")), xmlValue)
  site <- rep(sites[1],length(day))
  
  total<-as.numeric(total)
  day<-as.Date(day)
  
  dd<-as.numeric(format(day,"%d"))
  mm<-as.numeric(format(day,"%m"))
  yy<-as.numeric(format(day,"%Y"))
  ddmm<-as.character(format(day,"%d-%B"))
  
  df <- data.frame(day,total,site,ddmm,yy, stringsAsFactors=FALSE)
  as.factor(df$yy)
  
  ###########################################################
  ## PROCESSING CURRENT YEAR RAINFALL RECORD
  url <- paste("http://",HSERVER,".horizons.govt.nz/boo.hts?service=Hilltop&request=GetData&Site=",input$raingauge,"&Measurement=Rainfall Total (1 Day)&from=2013-07-01&to=2014-06-30",sep="")
  getThisYear.xml <- xmlInternalTreeParse(url)
  cat(url,"\n")

  csites<-sapply(getNodeSet(getThisYear.xml,"//Measurement/@SiteName"),as.character)
  
  
  cday <- sapply(getNodeSet(getThisYear.xml, paste("//Hilltop/Measurement[@SiteName='",csites[1],"']/Data/E/../E/T",sep="")), xmlValue)
  ctotal <- sapply(getNodeSet(getThisYear.xml, paste("//Hilltop/Measurement[@SiteName='",csites[1],"']/Data/E/../E/I1",sep="")), xmlValue)
  csite <- rep(sites[1],length(cday))
  
  ctotal<-as.numeric(ctotal)
  cday<-as.Date(cday)
  
  cdd<-as.numeric(format(cday,"%d"))
  cmm<-as.numeric(format(cday,"%m"))
  cyy<-as.numeric(format(cday,"%Y"))
  cddmm<-as.character(format(cday,"%d-%B"))
  
  cdf <- data.frame(cday,ctotal,csite,cddmm,cyy,cmm, stringsAsFactors=FALSE)
  as.factor(cdf$cyy)
  
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # CALCULATION OF LONG TERM RAINFALL QUANTILES BETWEEN 1981-2010 - 10%, 50% and 90%
  ## for each year, calculate cumulative rainfalls against each day, and store in matrix
  ## Create Matrix - Available years by 365 days
  md <- matrix(data = NA, nrow = 365, ncol = 60, byrow = FALSE,
               dimnames = NULL)
  ## Load matrix with cummulative rainfalls
  j <- 1
  for(i in 1:length(unique(df$yy))) {
    
    if(availableRecord==TRUE) {
      ds<-subset(df,yy==unique(df$yy)[i])
      a<-ds$total
      ## adjust for leap years
      
      ## Option to remove leap years
      if(unique(df$yy)[i] %% 4 == 0) {
        a <- a[-60]
      }
      
      if(length(a)==365){
        a <- a[c(182:365,1:181)]
        
        ## adding a place holder at the end of the year to make sure each year vector
        ## is 366 items long - giving it a value of zero so not to affect cumulative
        ## totals in cumulative totals
        #if (length(a)==365)  a<-c(a,0)
        ## Accumulate rainfalls and add to matrix
        md[ , j]<-cumsum(a)
        j<-j+1
      }
    
    } else {
    
      if(unique(df$yy)[i]>=1981&&unique(df$yy)[i]<=2010) {
        ds<-subset(df,yy==unique(df$yy)[i])
        a<-ds$total
        ## adjust for leap years
        
        ## Option to remove leap years
        if(unique(df$yy)[i] %% 4 == 0) {
          a <- a[-60]
        }
        
        a <- a[c(182:365,1:181)]
        
        ## adding a place holder at the end of the year to make sure each year vector
        ## is 366 items long - giving it a value of zero so not to affect cumulative
        ## totals in cumulative totals
        #if (length(a)==365)  a<-c(a,0)
        
        ## Accumulate rainfalls and add to matrix
        md[ , j]<-cumsum(a)
        j<-j+1
      }
    }   
  }
  
  
  # Calculate quantile values for each day
  if(availableRecord==TRUE){
    # Calculate quantile values for each day
    for(i in 1:365) {
      if(i==1){
        dq<-data.frame(quantile(md[i, ],prob=c(0.05,0.1,0.5,0.9,0.95),na.rm=TRUE),c(5,10,50,90,95),i)
      } else {
        q<-data.frame(quantile(md[i, ],prob=c(0.05,0.1,0.5,0.9,0.95),na.rm=TRUE),c(5,10,50,90,95),i)
        dq<-rbind(dq,q)
      }
    }
  } else{ 
    
    # Calculate quantile values for each day
    for(i in 1:365) {
      if(i==1){
        dq<-data.frame(quantile(md[i, ],prob=c(0.05,0.1,0.5,0.9,0.95)),c(5,10,50,90,95),i)
      } else {
        q<-data.frame(quantile(md[i, ],prob=c(0.05,0.1,0.5,0.9,0.95)),c(5,10,50,90,95),i)
        dq<-rbind(dq,q)
      }
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

  
 #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # PREPARE DATA FOR PLOTTING BARPLOT
  d <- read.zoo(df[ ,-3:-6]) # Removing unnecessary columns
  e <- read.zoo(cdf[ ,-3:-6]) # Removing unnecessary columns
  
  # CUMULATIVE VALUES SELECTED YEARS
  # Selecting last years time slice
  if(input$yoi!="None"){
  x <- window(d, start=as.Date(paste(as.character(as.numeric(input$yoi)-1),"-07-01",sep="")), end=as.Date(paste(as.character(input$yoi),"-06-30",sep="")))
  index(x) <- 1:length(x)
  }

  # Selecting 2003-04 Flood Time slice
  y <- window(d, start=as.Date("2003-07-01"), end=as.Date("2004-06-30"))
  index(y) <- 1:length(y)
  
  # Selecting 2013-14 year time slice
  z <- window(e, start=as.Date("2013-07-01"))
  index(z) <- 1:length(z)
  
  # CALCULATING STATS OVER RECORD
  if(availableRecord==TRUE){
    # Grabbing 30 years of record
    y30 <- window(d, start=as.Date("1981-01-01"), end=as.Date("2010-12-31")) ## 10957 days
    # Calculate monthly totals across the last thirty years
    y30sum <- aggregate(y30, as.Date(cut(time(y30), "month")), sum)          ## 360 (12 months x 30yrs)
  } else {  
    y30sum <- aggregate(d, as.Date(cut(time(d), "month")), sum)          ## 360 (12 months x 30yrs)
  }
  
  y30mean <- monthlyfunction(y30sum, FUN=mean, na.rm=TRUE)                 ## 12 rows
  coredata(y30mean)<-coredata(y30mean)[c(7:12,1:6)]
  index(y30mean)<-index(y30mean)[c(7:12,1:6)]
  
  # Applying monthlyfunction to this season and last season
  yNow  <- window(e, start=as.Date("2013-07-01"), end=as.Date("2014-06-30"))
  yLast <- window(d, start=as.Date("2012-07-01"), end=as.Date("2013-06-30"))
  
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
    
     ## This section can be modified to deal with dates over a Jul-Jun year
    melt.bar<-melt(data.frame(month=rownames(bar), bar, stringsAsFactors=FALSE), id.vars="month")
    melt.bar$monthnum <- c(1:12)
    melt.bar$monthnum<-factor(as.character(melt.bar$monthnum), levels=c(1:12), labels=c("Jul","Aug","Sep","Oct","Nov","Dec",
                                                                                      "Jan","Feb","Mar","Apr","May","Jun"),ordered=TRUE)
  
    #casting as array
    bar <- acast(melt.bar, variable ~ monthnum, sum, margins=FALSE)
    
    ############################################################################################
    ## Using Base Graphics Package
    
    
    opar<-par()
    par(mfrow=c(1,1),mar=c(2,3,1,3)+0.1,cex.axis=0.6, cex.main=0.75, cex.lab=0.75, mgp=c(1.5,0.4,0.0),tck=1,tcl=-0.3)
    
    barplot(as.matrix(bar),  beside = TRUE, col = c("cornsilk3", "cornflowerblue", "coral3"), 
            ylim = c(0, barPlotMax), ylab="Monthly rainfall totals (mm)", border=NA)
    title(main = paste("Monitoring Station: ",input$raingauge,": record from ",min(df$yy),sep=""), font.main = 1)
    
    par(new=T)
    
    plot(cumsum(y), type="n", axes=FALSE,ylim = c(0, cumulMax), xlab="", ylab="")
    
    
    i.for <- 1:365
    i.back <- 365:1
    
    x.polygon <- c( i.for, i.back )
    y1.polygon <- c( dq$p90[i.for] , dq$p10[i.back] )
    y2.polygon <- c( dq$p95[i.for] , dq$p5[i.back] )
    #polygon( x.polygon , y2.polygon , col=rgb(0.2,0.2,0.2,0.2, maxColorValue=1), border = NA)
    #polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.1, maxColorValue=1), border = NA)
    polygon( x.polygon , y1.polygon , col=rgb(0.2,0.2,0.2,0.3, maxColorValue=1), border = NA)
    
    points(dq$p50,type="l", col="black", lwd=2, lty="solid") # color was white
    
    if(input$yoi!="None"){
	points(cumsum(x),type="l", col="blue",lwd=2)
    }

    #points(cumsum(y),type="l", col="red", lwd=2)
    points(cumsum(z),type="l", col="red", lwd=3) # color was black
    legend(x=cumulMax,
           c("Average","Last Year","This Year"),
           fill = c("cornsilk3", "cornflowerblue", "coral3"),
           cex=0.8,
           bty="n",
           title="Monthly rain",
           ncol=2,
           border="white")
    
#    legend(x=cumulMax/4*3+0.1*cumulMax,
#           c("10-90%-ile","Median",as.character(input$yoi),"2004","This year"),
#           fill = c(rgb(0.2,0.2,0.2,0.3), "black", "blue","red","black"),
#           cex=0.8,
#           bty="n",
#           title="Accumulated rain",
#           ncol=2,
#           border="white")


  if(input$yoi!="None"){
    legend(x=cumulMax/4*3+0.1*cumulMax,
           c("10-90%-ile","Median",as.character(input$yoi),"This year"),
           fill = c(rgb(0.2,0.2,0.2,0.3), "black", "blue","red"),
           cex=0.8,
           bty="n",
           title="Accumulated rain",
           ncol=2,
           border="white")
  } else {
    legend(x=cumulMax/4*3+0.1*cumulMax,
           c("10-90%-ile","Median","This year"),
           fill = c(rgb(0.2,0.2,0.2,0.3), "black", "red"),
           cex=0.8,
           bty="n",
           title="Accumulated rain",
           ncol=2,
           border="white")
  }
   
    axis(side=4, outer=FALSE)
    mtext("Accumulated rainfall (mm)", side=4,line=2, cex=0.75)
    par(new=F)
    par(opar)
  })
})
