plotAverages <- function(ds.cur,input,pType='Time series',type) {
  cat(type,'\n')
  if(type == 'No filter') return(0)
  switch(type,
         
         "Period average" = 
         {
           p.options <- list(
             AP = '30 days', #svalue(gcbgAveragingPeriod), 
             AvgPer = 30,  #AvgPer[which(AvgPer$AP %in% svalue(gcbgAveragingPeriod)),'Value'],
             UserDefined = ''  #svalue(gePerUser)
           )
                             
           cat('type 2\n')
           print(input$daterange1)
           createandPlotAverage(ds.cur,input,pType=pType,
             avgtype=type,p.options = p.options,
             t.start=as.POSIXct(input$daterange1[1]),
             t.end=as.POSIXct(input$daterange1[2])
           )
         },
         
         "Moving average" = 
         {
           p.options <- list(
             AP = '30 days',  #svalue(gcbgAveragingPeriod), 
             AvgPer = 30,  #AvgPer[which(AvgPer$AP %in% svalue(gcbgAveragingPeriod)),'Value'],
             UserDefined = ''  #svalue(gePerUser)
           )
           cat('type 3\n')
           createandPlotAverage(ds.cur,input,pType=pType,
             avgtype=type,p.options = p.options,
             t.start=as.POSIXct(input$daterange1[1]),
             t.end=as.POSIXct(input$daterange1[2])
           )
         }

       )  # end of switch block
  
}

a.filter <- function(d.df,type='No filter') {

#  msg.Type <- paste(' Applying',type,'filter ...')
#  cat(msg.Type)
#  svalue(sb) <- msg.Type
  
  if(type == 'Godin') {
    
    # needs filled series
     if(exists('df.ExpOK')) {
       if(!('Filled' %in% names(df.ExpOK)))
         df.ExpOK$Filled <<- fillGaps.ssa(d.df)$dfrm$Filled
     } else
         df.ExpOK <<- fillGaps.ssa(d.df)$dfrm
     
      if(!any(is.na(df.ExpOK$Filled))){
        x.filt <- createandPlotGodin()
        df.ExpOK$Godin <- x.filt
      } 
      else
        return(ds.ExpOK)
    svalue(sb) <- ' Ready'
    enabled(gbPlotGodin) <- T
    
  } else if(type == 'Butterworth') {

    # needs filled series
    if(exists('df.ExpOK')) {
      if(!('Filled' %in% names(df.ExpOK)))
        df.ExpOK$Filled <<- fillGaps.ssa(d.df)$dfrm$Filled
    } else
      df.ExpOK <<- fillGaps.ssa(d.df)$dfrm
    
    if(!any(is.na(df.ExpOK$Filled))){
      x.filt <- createandPlotButter()
      df.ExpOK$Butterworth <- x.filt
    } 
    else
      return(ds.ExpOK)
    svalue(sb) <- ' Ready'
    enabled(gbPlotButterworth) <- T
    
  } else if(type == 'Harmonic') {
    cat('Using Harmonic filter\n')
    x.filt <- d.df$ResultU

    cat('Applying Harmonic filter ...')
#    df.ExpOK <<- fillGaps(d.df)$dfrm
    if(!any(is.na(df.ExpOK$Filled)))
#      x.filt <- createandPlotHarmonic()
      x.filt <- df.ExpOK$ResultU
      return(ds.ExpOK)

    cat('Done\n')
    svalue(sb) <- ' Ready'
    df.ExpOK$Harmonic <- x.filt
    enabled(gbPlotHarmonic) <- T
        
  } else if(type == 'Gap analysis') {
    
    # needs filled series
  
    if(!exists('df.ExpOK'))
      df.ExpOK <<- createandPlotGapAnalysis(d.df)
      
    enabled(gbGapStats) <- enabled(gbGapFill) <- T
    svalue(sb) <- ' Ready'
    
    return(df.ExpOK)
    
  } else {  # no filtering
    cat('No filtering\n')
    x.filt <- d.df$ResultU
  }
  return(df.ExpOK)
  
}

createandPlotGodin <- function() {
  source("Tgodinfn.R")
  if(!exists("df.ExpOK")) {
    return("Run gap analysis first")
  }
  xfilt <- Tgodinfn(df.ExpOK[,"dateTime"],df.ExpOK[,"Filled"])  
  return(xfilt)
}

createandPlotButter <- function() {
  source("Tbutterfn.R")
  if(!exists("df.ExpOK")) {
    return("Run gap analysis first")
  }
  xfilt<-Tbutterfn(df.ExpOK[,"dateTime"],df.ExpOK[,"Filled"])  
  return(xfilt)
}

createandPlotGapAnalysis <- function(d.df) {
  xfilled <- fillGaps.ssa(d.df)$dfrm
  return(xfilled)
}

createandPlotAverage <- function(d.cur,input,pType,avgtype='Period average',
  p.options=NULL,mingap=15,maxgap=60,t.start,t.end,cols){

  AP <- p.options$AP
  AvgPers <- p.options$AvgPer  #AvgPer[which(AvgPer$AP %in% AP),'Value']
  if('User defined' %in% AP) {
    gep <- paste(p.options$UserDefined,collapse=',')
    gep <- strsplit(gep,',')
    AvgPers[which(AP == 'User defined')] <- gep
    AvgPers <- as.numeric(unlist(AvgPers))
    AP <- c(AP,paste(unlist(gep),' days',sep=''))
    AP <- AP[-which(AP=='User defined')]
  }
  AvgPers <- as.numeric(unlist(AvgPers))
  
#  svalue(sb) <- svalue(glNoData) <- paste('Creating ',avgtype,'...',sep=' ')
#  browser()
  # make color pallete and define number of cuts
  if(!exists('cols')) {
    cols <- rev(brewer.pal(length(AvgPers), 'Spectral'))  
    cols.palette <- colorRampPalette(cols)
  }
  # vrbl <- svalue(gcbgParameter)
  # v <- pList[[vrbl]]
  # vUnits <- v$units; Log <- ''
  # cat('getting date range ...')
  print(head(d.cur[,1:4]))
  print(t.start)
  print(t.end)
  d.cur <- d.cur[which(d.cur$dateTime >= t.start & d.cur$dateTime <= t.end),]
  if(nrow(d.cur)>0 ) {
    
    d.cur <- d.cur[which(d.cur[,4] > 0),]
    sf <- 1
    limits <- as.numeric(c(input$yMin,input$yMax))
    if(log10(max(d.cur$ResultU)) >= 4) {
      sf <- 1000
      sScale <- 'x 1000'
      limits <- limits/sf
    }
  
    iPlt <<- iAvg <<- 1
    st.u <- unique(d.cur$site_no)
    
    cols <- getCols(length(st.u))
    parallel.col <<- matrix(rep(NA,times=(length(st.u)*(length(AvgPers)+1))),ncol=length(st.u))
    parallel.col[1,] <<- cols
#    
    sapply(AvgPers,function(wa) {  # send the averaging period or other parameter to sort out the average
      
      iSt <<- 1

      sapply(st.u,function(st) {   # separate by site
        
        w.per <- seq.POSIXt(d.cur[1,3],max(d.cur[,3]),by=wa*86400)
        if(length(st.u) > 1) 
          cols.avg.pal <- colorRampPalette(c(cols[iSt],'grey90'),interpolate='linear',length(AvgPers)+3)
        else
          cols.avg.pal <- colorRampPalette(rev(brewer.pal(max(3,length(AvgPers)), 'Spectral')))

        cols.avg <- (cols.avg.pal(length(AvgPers) + 1))
        if(iSt == 1) col.leg <<- cols.avg
        
        if(pType == 'Time series') {   # may want to bundle all of the parameters into one unit
          
#          svalue(sb) <- paste('Building ',avgtype,' ...',sep='')
          d.avg <- d.cur[d.cur$site_no == st,]
          if(AP=='Annual') {
            cat('Annual\n')
            
            d.mean <- by(d.avg[,"dateTime"],IND=list(d.avg$WaterYear,d.avg$site_no),FUN=mean,na.rm=T)
            d.mean <- matrix(as.numeric(d.mean),nrow=length(unique(d.avg$WaterYear)))
            pMean <<- by(d.avg[,"ResultU"] ,IND=list(d.avg$WaterYear,d.avg$site_no),FUN=mean,na.rm=T)
            pMean <<- cbind(d.mean,matrix(as.numeric(pMean),nrow=length(unique(d.avg$WaterYear))))
            
#            pMean <- by(d.avg$ResultU,IND=list(d.avg$WaterYear,d.avg$site_no),FUN=mean, na.rm=T)
            
          } else {
            
            tmp <<- tempfile()
            pMean <<- getAppropriateAverage(d.avg,avgtype,wa,w.per,st=st,ap=wa)
          }
          
          pch <- c(19,21,22,23,24,25,26)
          Pch <- -1
          lines(pMean[,2]/sf~pMean[,1],pch=Pch[iAvg+0],
                col=cols.avg[iAvg+0],lwd=1) 
          if(svalue(gchkAnalysisUsePoints)) {
            Pch <- pch[1:length(st.u)]; cat(iAvg,': ',Pch[iAvg],'\n')
            points(pMean[,2]/sf~pMean[,1],pch=Pch[iAvg],
                   col=cols.avg[iAvg],cex=1) 
          }
#          svalue(sb) <- 'Ready'
          
        } else if(pType == 'Frequency') {
          
          #      pPlotxy(d.cur[,4],grid=T,fgrid=T,col='black',xlab=paste(vrbl,', ',vUnits),Log=Log,yLim=c(-4.5,4.5),
          #              main=paste(vrbl,' at ',d.cur$site_no[1],'\nAveraging Period = ',
          #                         paste(round(AvgPer,3),collapse=', '),
          #                          ' day',sep=''),cex.axis=.666)
          #        w.per <- seq.POSIXt(d.cur[1,3],max(d.cur[,3]),by=wa*86400)
          # pPlotxy(
          #   pMean <- sapply(1:(length(w.per)-1),function(ip) {
          #     sInd <- which(between2(d.cur[,3],w.per[ip],w.per[ip+1],flgInclU=F))
          #     mean(d.cur[sInd,4],na.rm=T)
          #   }),col=cols[iPlt],addData=T,cex=.75,Log=Log)
          # pMean
        }
        
        iPlt <<- iPlt + 1
        parallel.col[iAvg+1,iSt] <<- cols.avg[iAvg+0]
        iSt <<- iSt + 1
        
      })
      iAvg <<- iAvg + 1
      
    })
    
    
    if(input$legpos) {    
      legPos <- input$selleg; if(length(unique(d.cur$site_no)) > 1) legPos <- 'topright'
      leg <- as.character(AP)
      col.leg <- col.leg[-1]
      
#      cat('before\n'); print(parallel.col)
      
      if(svalue(gchkAnalysisShowData)) {
        leg <- c('15-min',leg)
        col.leg <- c(cols[1],col.leg)
      } else {
        parallel.col <<- matrix(parallel.col[-1,],ncol=length(st.u),byrow=F)
      }
      
      cat('after\n'); print(parallel.col)
      
      # Legend(legPos,legend=leg,title='Averaging Period',
      #        fill=col.leg,border=col.leg,lty=-1,pch=-1,bty='n',
      #        cex=0.8,inset=c(0.02,0.05),parallel.col=parallel.col) #0.015)
      #        
      if(avgtype %in% c('Period average','Moving average'))
        legTitle <- 'Averaging period'
      else if(avgtype == 'Loess')
      legTitle <- paste(avgtype,' span, fraction',sep='')
        
      Legend(legPos,legend=leg,title=legTitle,
             fill=col.leg,border=col.leg,lty=-1,pch=-1,bty='o',bg=rgb(1,1,1,.5),
             box.col=rgb(1,1,1,.5),cex=0.8,inset=c(0.02,0.05),
             parallel.col=parallel.col)
    }
    
    # legend(legPos,legend=c('15-min',paste(round(AvgPers,3),'days')),title='Averaging Period',
    #        col=c('black',cols),lty=-1,pch=19,bty='n',cex=0.8,inset=0.015)
    #      dev.off()
  }
#  svalue(sb) <- ' Ready '
#  svalue(glNoData) <- 'Status\nOk'
  
  return(pMean)
}

getAppropriateAverage <- function(d.cur,avgtype,wa,w.per,st,ap) {
  
  
    # here's the guts of it - if it works, this is all we'll need
    # 
    if(avgtype == 'Period average') {                                  # Period average
      
      pMean <<- data.frame()
      iPM <<- 1
      if(wa == 365.25) {  # annual average by water year
        
        d.cur$WaterYear <- as.factor(d.cur$WaterYear)
        d.mean <- by(d.cur[,"dateTime"],IND=d.cur$WaterYear,FUN=mean,na.rm=T)
        cat('pm 0a: ','\n'); browser()
        pMean <- by(d.cur[,"ResultU"],IND=d.cur$WaterYear,FUN=mean,na.rm=T)
        d.mean <- matrix(as.numeric(d.mean),nrow=length(unique(d.cur$WaterYear)))
#cat('pm 0b: ','\n'); print(pMean); browser()
        pMean <- cbind(d.mean,matrix(as.numeric(pMean),nrow=length(unique(d.cur$WaterYear))))
        
      } else if(wa == 365.25/4) {  
        
        d.cur$Season <- as.factor(getSeason.2(d.cur$dateTime))
        pMean <<- by(d.cur[,"ResultU"],IND=d.cur$Season,FUN=mean,na.rm=T)
        
      } else if(wa == 365.25/12) {
        d.cur$Week <- as.factor(as.POSIXlt(d.cur$dateTime)$week)
        pMean <<- by(d.cur[,"ResultU"],IND=d.cur$Week,FUN=mean,na.rm=T)
        
      } else {
        
      sapply(1:(length(w.per)-1),function(ip) {
        sInd <- which(between2(d.cur[,'dateTime'],w.per[ip],w.per[ip+1],flgIncl=F) &
                    d.cur$site_no == st)
        pMean[iPM,1] <<- as.POSIXct(mean(d.cur[sInd,'dateTime'],na.rm=T),origin='1970-01-01')
        if(svalue(gcbLogY))
          pMean[iPM,2] <<- 10^(mean(log10(d.cur[sInd,4]),na.rm=T))
        else
          pMean[iPM,2] <<- mean(d.cur[sInd,4],na.rm=T)
          
        iPM <<- iPM + 1
          
      })
      }
      
    } else if(avgtype == 'Moving average') {                           # Moving average
      
#      if(svalue(gchkMovingInterpolate)) {   # fill all gaps (for now) and use filter
        
#        iGap <- which(diff(d.cur$dateTime) > 60)
#        d.tmp <<- data.frame()
#        af <- approxfun(d.cur$dateTime, d.cur$ResultU)
#        d.int <- seq.POSIXt(min(d.cur$dateTime),max(d.cur$dateTime),by="15 min")
#        y.int <- af(d.int)
#        wa.step <- 4*24*wa        # assuming 15 minute base interval
#        pMean <<- data.frame(DT=d.int,Y=stats::filter(y.int,rep(1/wa.step,times=wa.step)))
        
#      } else {
        
        d.cur <- findGaps(d.cur,maxgapsize=60)$d.df
        d.tmp <<- data.frame()
        d.cur$ResultU[d.cur$gap] <- NA
        af <- approxfun(d.cur$dateTime, d.cur$ResultU)
        d.int <- d.cur$dateTime   #seq.POSIXt(min(d.cur$dateTime),max(d.cur$dateTime),by="15 min")
        y.int <- d.cur$ResultU  #af(d.int)
        wa.step <- 4*24*wa        # assuming 15 minute base interval
        y.ma <- stats::filter(y.int,rep(1/wa.step,time=wa.step))
#        cat('ma\n'); browser()
        if(svalue(gcbLogY)) {
          pMean <<- data.frame(DT=d.int,Y=10^stats::filter(log10(y.int),rep(1/wa.step,times=wa.step)))
        } else {
          pMean <<- data.frame(DT=d.int,Y=stats::filter(y.ma,rep(1/wa.step,times=wa.step)))
        }
      #}

    } else if(avgtype == 'Loess') {                           # Loess

      # if(svalue(gchkMovingInterpolate)) {   # fill all gaps (for now) and use filter
      #   
      #   iGap <- which(diff(d.cur$dateTime) > 60)
      #   d.tmp <<- data.frame()
      #   af <- approxfun(d.cur$dateTime, d.cur$ResultU)
      #   d.int <- seq.POSIXt(min(d.cur$dateTime),max(d.cur$dateTime),by="15 min")
      #   y.int <- af(d.int)
      #   pMean <<- data.frame(DT=d.int,Y=loess(y.int~as.numeric(d.int),span=sp)$fitted)
      #   
      # } else {
        
        d.cur <- findGaps(d.cur,maxgapsize=60)$d.df
        d.tmp <<- data.frame()
        d.cur$ResultU[d.cur$gap] <- NA
        af <- approxfun(d.cur$dateTime, d.cur$ResultU)
        d.int <- d.cur$dateTime   #seq.POSIXt(min(d.cur$dateTime),max(d.cur$dateTime),by="15 min")
        y.int <- d.cur$ResultU  #af(d.int)
        cat('span=',ap,'\n')
        y.loess <- loess(y.int~as.numeric(d.int),span=ap)
        pMean <<- data.frame(DT=d.int,Y=predict(y.loess,d.int))
        
#      }
    }
    # to here
    # 
  
  return(pMean)
}

setupFiltering <- function() {
  
  AvgPer <- AvgMov <- data.frame(AP=c('6 hours','12 hours','1 day',
                                      '2 days','7 days','30 days','Annual','Seasonal',
                                      'Monthly','User defined'),
                                 Value=c(1/4,1/2,1,2,7,30,365.25,365.25/4,365.25/12,1),stringsAsFactors = F)     #days
  LoessSpan <- data.frame(AP=c('0.001','0.01','0.025','0.05','0.1','0.2','0.5','User defined'),
                          Value=c(1/1000,1/100,1/40,1/20,1/10,1/5,1/2,1))
  
  HarmonicModel <- data.frame(AP=c('4 parameter','7 parameter',
                                   '37 parameter','60 parameter',
                                   '114 parameter'),
                              Value=c('hc4','hc7','hc37','hc60','hc114'),
                              stringsAsFactors = F)
  FrequencyModel <- data.frame(AP=c('4 parameter','7 parameter',
                                    '37 parameter','60 parameter',
                                    '114 parameter'),
                               Value=c('hc4','hc7','hc37','hc60','hc114'),
                               stringsAsFactors = F)
  ButterworthModel <- data.frame(AP=c('foo1','foo2'),
                                 Value=c('1','2'),
                                 stringsAsFactors = F)
  BreakpointModel <- data.frame(AP=c('OCUS','ME'),
                                Value=c('ocus','me'),
                                stringsAsFactors = F)
  return(list(AvgPer=AvgPer,AvgMov=AvgMov,Loess=LoessSpan,Harmonic=HarmonicModel,
              Frequency=FrequencyModel,Butterworth=ButterworthModel,
              Breakpoint=BreakpointModel))
  
}
