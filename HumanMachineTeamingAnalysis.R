### data analysis script for examining human-machine teaming
### e.g., selecting cues, determining similarity
### A lot is commented out, but this is if signal-detection tests are desired
library("ggplot2")
library("Rmisc")
library("plyr")
library("extrafont")



#uses a window (defined below) to average data
average.by.window <- function(currentData, avgWndw, cond){
	
	iter <- ceiling(nrow(currentData[which(currentData$Run == 1),])/avgWndw)
	out <- matrix(NA, nrow=iter, ncol=1)
	for (windowNum in 1:iter){

		newData <- currentData[ which (currentData$averageWindow == windowNum) ,]
		windowAvg <- sum(newData$ACC)/nrow(newData)
		out[windowNum, ] <- c(windowAvg)
	}	
	out <- cbind(out, cond = cond)
	return(out)
}

determine.strategy <- function(data){
  for (i in 1:nrow(data)){
    if(data$revealed_feature1[i]==-1){#if nothing is selected, outcome agnostic
      data$strategy[i] <- 1
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==-1 & data$revealed_feature3[i]==-1){#{f1} revealed, outcome agnostic
      data$strategy[i] <- 2
    }
    if(data$revealed_feature1[i]==-1 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==-1){#{f2} revealed, outcome agnostic
      data$strategy[i] <- 3
    }
    if(data$revealed_feature1[i]==-1 & data$revealed_feature2[i]==-1 & data$revealed_feature3[i]==1){#{f3} revealed, outcome agnostic
      data$strategy[i] <- 4
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==-1 & data$feature1[i]==1){#{f1 f2} revealed, f1=1
      data$strategy[i] <- 5
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==-1 & data$feature1[i]==0){#{f1 f2} revealed, f1=0
      data$strategy[i] <- 6
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==-1 & data$revealed_feature3[i]==2 & data$feature1[i]==1){#{f1 f3} revealed, f1=1
      data$strategy[i] <- 7
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==-1 & data$revealed_feature3[i]==2 & data$feature1[i]==0){#{f1 f3} revealed, f1=0
      data$strategy[i] <- 8
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==3 & data$feature1[i]==1 & data$feature2[i]==1){#{f1 f2 f3} revealed, f1=1, f2=1
      data$strategy[i] <- 9
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==3 & data$feature1[i]==1 & data$feature2[i]==0){#{f1 f2 f3} revealed, f1=1, f2=0
      data$strategy[i] <- 10
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==3 & data$feature1[i]==0 & data$feature2[i]==1){#{f1 f2 f3} revealed, f1=0, f2=1
      data$strategy[i] <- 11
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==3 & data$feature1[i]==0 & data$feature2[i]==0){#{f1 f2 f3} revealed, f1=0, f2=0
      data$strategy[i] <- 12
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==2 & data$feature1[i]==1 & data$feature3[i]==1){#{f1 f3 f2} revealed, f1=1, f2=1
      data$strategy[i] <- 13
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==2 & data$feature1[i]==1 & data$feature3[i]==0){#{f1 f3 f2} revealed, f1=1, f2=0
      data$strategy[i] <- 14
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==2 & data$feature1[i]==0 & data$feature3[i]==1){#{f1 f3 f2} revealed, f1=0, f2=1
      data$strategy[i] <- 15
    }
    if(data$revealed_feature1[i]==1 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==2 & data$feature1[i]==0 & data$feature3[i]==0){#{f1 f3 f2} revealed, f1=0, f2=0
      data$strategy[i] <- 16
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==-1 & data$feature2[i]==1){#{f2 f1} revealed, f2=1
      data$strategy[i] <- 17
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==-1 & data$feature2[i]==0){#{f2 f1} revealed, f2=0
      data$strategy[i] <- 18
    }
    if(data$revealed_feature1[i]==-1 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==2 & data$feature2[i]==1){#{f2 f3} revealed, f2=1
      data$strategy[i] <- 19
    }
    if(data$revealed_feature1[i]==-1 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==2 & data$feature2[i]==0){#{f2 f3} revealed, f2=0
      data$strategy[i] <- 20
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==3 & data$feature2[i]==1 & data$feature1[i]==1){#{f2 f1 f3} revealed, f2=1, f1=1
      data$strategy[i] <- 21
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==3 & data$feature2[i]==1 & data$feature1[i]==0){#{f2 f1 f3} revealed, f2=1, f1=0
      data$strategy[i] <- 22
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==3 & data$feature2[i]==0 & data$feature1[i]==1){#{f2 f1 f3} revealed, f2=0, f1=1
      data$strategy[i] <- 23
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==3 & data$feature2[i]==0 & data$feature1[i]==0){#{f2 f1 f3} revealed, f2=1, f1=0
      data$strategy[i] <- 24
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==2 & data$feature2[i]==1 & data$feature3[i]==1){#{f2 f3 f1} revealed, f2=1, f3=1
      data$strategy[i] <- 25
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==2 & data$feature2[i]==1 & data$feature3[i]==0){#{f2 f3 f1} revealed, f2=1, f3=0
      data$strategy[i] <- 26
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==2 & data$feature2[i]==0 & data$feature3[i]==1){#{f2 f3 f1} revealed, f2=0, f3=1
      data$strategy[i] <- 27
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==1 & data$revealed_feature3[i]==2 & data$feature2[i]==0 & data$feature3[i]==0){#{f2 f3 f1} revealed, f2=0, f3=0
      data$strategy[i] <- 28
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==-1 & data$revealed_feature3[i]==1 & data$feature3[i]==1){#{f3 f1} revealed, f3=1
      data$strategy[i] <- 29
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==-1 & data$revealed_feature3[i]==1 & data$feature3[i]==0){#{f3 f1} revealed, f3=0
      data$strategy[i] <- 30
    }
    if(data$revealed_feature1[i]==-1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==1 & data$feature3[i]==1){#{f3 f2} revealed, f3=1
      data$strategy[i] <- 31
    }
    if(data$revealed_feature1[i]==-1 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==1 & data$feature3[i]==0){#{f3 f2} revealed, f3=0
      data$strategy[i] <- 32
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==1 & data$feature3[i]==1 & data$feature1[i]==1){#{f3 f1 f2} revealed, f3=1, f1=1
      data$strategy[i] <- 33
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==1 & data$feature3[i]==1 & data$feature1[i]==0){#{f3 f1 f2} revealed, f3=1, f1=0
      data$strategy[i] <- 34
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==1 & data$feature3[i]==0 & data$feature1[i]==1){#{f3 f1 f2} revealed, f3=0, f1=1
      data$strategy[i] <- 35
    }
    if(data$revealed_feature1[i]==2 & data$revealed_feature2[i]==3 & data$revealed_feature3[i]==1 & data$feature3[i]==0 & data$feature1[i]==0){#{f3 f1 f2} revealed, f3=0, f1=0
      data$strategy[i] <- 36
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==1 & data$feature3[i]==1 & data$feature2[i]==1){#{f3 f2 f1} revealed, f3=1, f2=1
      data$strategy[i] <- 37
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==1 & data$feature3[i]==1 & data$feature2[i]==0){#{f3 f2 f1} revealed, f3=1, f2=0
      data$strategy[i] <- 38
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==1 & data$feature3[i]==0 & data$feature2[i]==1){#{f3 f2 f1} revealed, f3=0, f2=1
      data$strategy[i] <- 39
    }
    if(data$revealed_feature1[i]==3 & data$revealed_feature2[i]==2 & data$revealed_feature3[i]==1 & data$feature3[i]==0 & data$feature2[i]==0){#{f3 f2 f1} revealed, f3=0, f2=0
      data$strategy[i] <- 40
    }
  }
  return(data)
}#end of determine.strategy function


#used to generate pattern matches
get.comp <- function(nV,c1,c2,c3,val1,val2,val3,resp){
	
	comp <- 0
	if(nV==1 & c1==1 & val2==0 & resp==0)
		{comp <- 1
	}
	if(nV>=2 & c1==1 & val2==1 & c2==2 & val3==0 & resp==0)
		{comp <- 1
	}
	if(nV>=2 & c1==1 & val2==1 & c2==2 & val3==1 & resp==1)
		{comp <- 1
	}
	if(nV==1 & c1==2 & val3==0 & resp==0)
		{comp <- 1
	}
	if(nV>=2 & c1==2 & val3==1 & c2==1 & val2==0 & resp==0)
		{comp <- 1
	}
	if(nV>=2 & c1==2 & val3==1 & c2==1 & val2==1 & resp==1)
		{comp <- 1
	}
	return(comp)
}

#used to generate pattern matches w/ costs
get.comp.cost <- function(nV,c1,c2,c3,val1,val2,val3,resp){
  
  comp <- 0
  if(nV==0 & resp==0){
    comp <-1
  }
  if(nV==1 & c1==1 & val2==0 & resp==0)
  {comp <- 1
  }
  if(nV>=2 & c1==1 & val2==1 & c2==2 & val3==0 & resp==0)
  {comp <- 1
  }
  if(nV>=2 & c1==1 & val2==1 & c2==2 & val3==1 & resp==1)
  {comp <- 1)
  }
  return(comp)
}

iterate.mod.comp.ez <- function(mdata){
	for(r in 1:nrow(mdata)){
	  #print(mdata[r,])
	  mdata$FFT.comp[r] <- get.comp(mdata$Cues.Viewed[r], 
									mdata$revealed_feature1[r], 
									mdata$revealed_feature2[r], 
									mdata$revealed_feature3[r],
									mdata$feature1[r], 
									mdata$feature2[r], 
									mdata$feature3[r], 
									mdata$response[r])
	  #print(r)
	}
	mdata$FFT.comp <- as.integer(mdata$FFT.comp)
	return(mdata)

}

iterate.mod.comp.cost <- function(mdata){
  for(r in 1:nrow(mdata)){
    #print(mdata[r,])
    mdata$FFT.comp[r] <- get.comp.cost(mdata$Cues.Viewed[r], 
                                  mdata$revealed_feature1[r], 
                                  mdata$revealed_feature2[r], 
                                  mdata$revealed_feature3[r],
                                  mdata$feature1[r], 
                                  mdata$feature2[r], 
                                  mdata$feature3[r], 
                                  mdata$response[r])
    #print(r)
  }
  mdata$FFT.comp <- as.integer(mdata$FFT.comp)
  return(mdata)
  
}

clean.Revealed <- function(data){
  for(i in 1:nrow(data)){
    if(is.na(data$revealed_feature1[i])){
      data$revealed_feature1[i] <- -1 }
    if(is.na(data$revealed_feature2[i])){
      data$revealed_feature2[i] <- -1 }
    if(is.na(data$revealed_feature3[i])){
      data$revealed_feature3[i] <- -1 }
  }
  
  return(data)
}

cue.Count <- function(data){
  
  for(i in 1:nrow(data)){
    cueRevealed <- 0
    if(!is.na(data$revealed_feature1[i])){
      cueRevealed <- cueRevealed +1 }
    if(!is.na(data$revealed_feature2[i])){
      cueRevealed <- cueRevealed +1 }
    if(!is.na(data$revealed_feature3[i])){
      cueRevealed <- cueRevealed +1 }
    data$Cues.Viewed[i] <- cueRevealed
    if(cueRevealed !=0){
      data$RT.by.Cues[i] <- data$responseTime[i]/data$Cues.Viewed[i]
    }
    else{
      data$RT.by.Cues[i] <- data$responseTime[i]
    }
  }
  return(data)
}

## SDT Analyses ###############################################################
#s1.fa.rate  <- sum(data1$fa)/sum(sum(data1$fa),sum(data1$cr))
#s1.hit.rate <- sum(data1$hit)/sum(sum(data1$hit),sum(data1$miss))
#s2.fa.rate  <- sum(data2$fa)/sum(sum(data2$fa),sum(data2$cr))
#s2.hit.rate <- sum(data2$hit)/sum(sum(data2$hit),sum(data2$miss))
#s3.fa.rate  <- sum(data3$fa)/sum(sum(data3$fa),sum(data3$cr))
#s3.hit.rate <- sum(data3$hit)/sum(sum(data3$hit),sum(data3$miss))
#s4.fa.rate  <- sum(data4$fa)/sum(sum(data4$fa),sum(data4$cr))
#s4.hit.rate <- sum(data4$hit)/sum(sum(data4$hit),sum(data4$miss))
#s5.fa.rate  <- sum(data5$fa)/sum(sum(data5$fa),sum(data5$cr))
#s5.hit.rate <- sum(data5$hit)/sum(sum(data5$hit),sum(data5$miss))



############################################################################################################################
## Model


##Import 80% 30Cost Data
setwd("C:/Users/1513290957/Desktop/old/Data/CogHumanData/Jan17/UDRI Multi-Attribute Choice Task/Data/BAL_80_30Cost")
files = list.files(pattern="*.csv")
m.80Cost30 = do.call(rbind, lapply(files, function(x) read.table(x, sep="\t", header=TRUE)))

##Import 80% NoCost Data
setwd("C:/Users/1513290957/Desktop/old/Data/CogHumanData/Jan17/UDRI Multi-Attribute Choice Task/Data/BAL_80_NoCost")
files = list.files(pattern="*.csv")
m.80NoCost = do.call(rbind, lapply(files, function(x) read.table(x, sep="\t", header=TRUE)))

##Import 90% 30Cost Data
setwd("C:/Users/1513290957/Desktop/old/Data/CogHumanData/Jan17/UDRI Multi-Attribute Choice Task/Data/BAL_90_30Cost")
files = list.files(pattern="*.csv")
m.90Cost30 = do.call(rbind, lapply(files, function(x) read.table(x, sep="\t", header=TRUE)))

##Import 90% NoCost Data
setwd("C:/Users/1513290957/Desktop/old/Data/CogHumanData/Jan17/UDRI Multi-Attribute Choice Task/Data/BAL_90_NoCost")
files = list.files(pattern="*.csv")
m.90NoCost = do.call(rbind, lapply(files, function(x) read.table(x, sep="\t", header=TRUE)))


block.trials <- 30

average.window <- 3
trialNum <- 267


m.90Cost30$block <- ceiling(m.90Cost30$trialNumber/block.trials)
m.90NoCost$block <- ceiling(m.90NoCost$trialNumber/block.trials)
m.80NoCost$block <- ceiling(m.80NoCost$trialNumber/block.trials)
m.80Cost30$block <- ceiling(m.80Cost30$trialNumber/block.trials)

m.90Cost30 <- cue.Count(m.90Cost30)
m.90NoCost <- cue.Count(m.90NoCost)
m.80NoCost <- cue.Count(m.80NoCost)
m.80Cost30 <- cue.Count(m.80Cost30)

m.90Cost30 <- clean.Revealed(m.90Cost30)
m.90NoCost <- clean.Revealed(m.90NoCost)
m.80NoCost <- clean.Revealed(m.80NoCost)
m.80Cost30 <- clean.Revealed(m.80Cost30)

m.90Cost30$scaledScore <- (m.90Cost30$runningScore/1000)
m.90NoCost$scaledScore <- (m.90NoCost$runningScore/1000)
m.80NoCost$scaledScore <- (m.80NoCost$runningScore/1000)
m.80Cost30$scaledScore <- (m.80Cost30$runningScore/1000)

#m.90NoCost$averageWindow <- ceiling(m.90NoCost$Trial/average.window)
#m.90Cost30$averageWindow <- ceiling(m.90Cost30$Trial/average.window)
#m.80NoCost$averageWindow <- ceiling(m.80NoCost$Trial/average.window)
#m.80Cost30$averageWindow <- ceiling(m.80Cost30$Trial/average.window)

#m.90NoCost <- get.cueValues(m.90NoCost, trialNumber)
#m.90Cost30 <- get.cueValues(m.90Cost30, trialNumber)
#m.80NoCost <- get.cueValues(m.80NoCost, trialNumber)
#m.80Cost30 <- get.cueValues(m.80Cost30, trialNumber)

m.90NoCost <- iterate.mod.comp.ez(m.90NoCost)
m.90Cost30 <- iterate.mod.comp.cost(m.90Cost30)
m.80NoCost <- iterate.mod.comp.ez(m.80NoCost)
m.80Cost30 <- iterate.mod.comp.cost(m.80Cost30)

#m.90NoCost <- cumulative.score(m.90NoCost)
#m.90Cost30 <- cumulative.score(m.90Cost30)
#m.80NoCost <- cumulative.score(m.80NoCost)
#m.80Cost30 <- cumulative.score(m.80Cost30)

m.90NoCost$cond <- "m.90NoCost"
m.90Cost30$cond <- "m.90Cost30"
m.80NoCost$cond <- "m.80NoCost"
m.80Cost30$cond <- "m.80Cost30"

m.90NoCost$lineType <- "A"
m.90Cost30$lineType <- "B"
m.80NoCost$lineType <- "A"
m.80Cost30$lineType <- "B"

m.90NoCost$lineColor <- "A"
m.90Cost30$lineColor <- "A"
m.80NoCost$lineColor <- "B"
m.80Cost30$lineColor <- "B"

m.90NoCost <- determine.strategy(m.90NoCost)
m.90Cost30 <- determine.strategy(m.90Cost30)
m.80NoCost <- determine.strategy(m.80NoCost)
m.80Cost30 <- determine.strategy(m.80Cost30)


m.80NoCost.Bar <- count(m.80NoCost, vars=c("block", "strategy"))
m.80Cost30.Bar <- count(m.80NoCost, vars=c("block", "strategy"))
m.90NoCost.Bar <- count(m.80NoCost, vars=c("block", "strategy"))
m.90Cost30.Bar <- count(m.80NoCost, vars=c("block", "strategy"))

#################################
## Add Block column
#m.80NoCost$Trial <-as.integer(m.80NoCost$Trial)
#m.80NoCost$block <- ceiling(m.80NoCost$Trial/block.trials)
#m.RV70.ACTRML$Trial <-as.integer(m.RV70.ACTRML$Trial)
#m.RV70.ACTRML$block <- ceiling(m.RV70.ACTRML$Trial/block.trials)

## Make block numbers for 2nd Environment
#m.80NoCost[m.80NoCost$Halves==2,]$block = m.80NoCost[m.80NoCost$Halves==1,]$block + 5 #63 #10
#m.RV70.ACTRML[m.RV70.ACTRML$Halves==2,]$block = m.RV70.ACTRML[m.RV70.ACTRML$Halves==1,]$block + 5 

## Add blended.binary column
#nonstat$blended.binary <-ifelse(nonstat$Blended.P=='NIL',0,1)

## if errors (R thinks ACC column is a factor):
 #nonstat$block <- as.numeric(nonstat$block)
 #nonstat$ACC <- as.numeric(nonstat$ACC)
################################
 


#m.90Cost30$Thresh <- as.numeric(levels(m.90Cost30$Thresh))[m.90Cost30$Thresh]
#m.80NoCost$Thresh <- as.numeric(levels(m.80NoCost$Thresh))[m.80NoCost$Thresh]
#m.80Cost30$Thresh <- as.numeric(levels(m.80Cost30$Thresh))[m.80Cost30$Thresh]


# p85data$ACC <- as.numeric(p85data$ACC)
# p85data$ACC <- p85data$ACC - 1
 

all.m.80 <- rbind(m.80NoCost, m.80Cost30)
all.m.90 <- rbind(m.90NoCost, m.90Cost30)

all.m.noCost <- rbind(m.80NoCost, m.90NoCost)
all.m.cost <- rbind(m.80Cost30, m.90Cost30)

all.m.data <- rbind(m.80NoCost, m.80Cost30, m.90NoCost, m.90Cost30)

#################################
#all.m.data$revealed_cue1 <- ifelse(all.m.data$revealed_cue1=="NIL",0,all.m.data$revealed_cue1) 
#all.m.data$revealed_cue2 <- ifelse(all.m.data$revealed_cue2=="NIL",0,all.m.data$revealed_cue2)
#all.m.data$revealed_cue3 <- ifelse(all.m.data$revealed_cue3=="NIL",0,all.m.data$revealed_cue3)

#all.m.data$revealed_cue1 <- ifelse(all.m.data$First.cue=="NIL",0,all.m.data$First.cue) 
#all.m.data$revealed_cue2 <- ifelse(all.m.data$Second.cue=="NIL",0,all.m.data$Sec.cue)
#all.m.data$revealed_cue3 <- ifelse(all.m.data$Third.cue=="NIL",0,all.m.data$Third.cue)  

#all.m.data$checked_cue1 <- ifelse(all.m.data$revealed_cue1 > 0,1,0) 
#all.m.data$checked_cue2 <- ifelse(all.m.data$revealed_cue2 > 0,1,0)
#all.m.data$checked_cue3 <- ifelse(all.m.data$revealed_cue3 > 0,1,0)  


#all.m.data$nCues.checked <- all.data$checked_cue1 + all.data$checked_cue2 + all.data$checked_cue3
#works up to here "all.m.data[2000:21000,]"
####################################
 
################################################################
## Signal Detection Analysis:
# hit_env1 <- aggregate(nonstat_env1["Hit"], by=nonstat_env1["block"], FUN=sum)
# miss_env1 <- aggregate(nonstat_env1["Miss"], by=nonstat_env1["block"], FUN=sum)
# fa_env1 <- aggregate(nonstat_env1["FA"], by=nonstat_env1["block"], FUN=sum)
# cr_env1 <- aggregate(nonstat_env1["CR"], by=nonstat_env1["block"], FUN=sum)

# hit_env2 <- aggregate(nonstat_env2["Hit"], by=nonstat_env2["block"], FUN=sum)
# miss_env2 <- aggregate(nonstat_env2["Miss"], by=nonstat_env2["block"], FUN=sum)
# fa_env2 <- aggregate(nonstat_env2["FA"], by=nonstat_env2["block"], FUN=sum)
# cr_env2 <- aggregate(nonstat_env2["CR"], by=nonstat_env2["block"], FUN=sum)

#nonstat_env1 <- subset(all.m.data,block==2 & cond=="m.80NoCost")
# Correct Instruction block 2:
#hit_env1.2 <- sum(nonstat_env1["Hit"])
#miss_env1.2 <- sum(nonstat_env1["Miss"])
#fa_env1.2 <- sum(nonstat_env1["FA"])
#cr_env1.2 <- sum(nonstat_env1["CR"])

#nonstat_env1 <- subset(all.m.data,block==3 & cond=="m.80NoCost")

# Correct Instruction block 3:
#hit_env1.3 <- sum(nonstat_env1["Hit"])
#miss_env1.3 <- sum(nonstat_env1["Miss"])
#fa_env1.3 <- sum(nonstat_env1["FA"])
#cr_env1.3 <- sum(nonstat_env1["CR"])


#nonstat_env1 <- subset(all.m.data,block==2 & cond=="m.80Cost30")
# No Instruction
#hit_env2.2 <- sum(nonstat_env1["Hit"])
#miss_env2.2 <- sum(nonstat_env1["Miss"])
#fa_env2.2 <- sum(nonstat_env1["FA"])
#cr_env2.2 <- sum(nonstat_env1["CR"])

#nonstat_env1 <- subset(all.m.data,block==3 & cond=="m.80Cost30")
#hit_env2.3 <- sum(nonstat_env1["Hit"])
#miss_env2.3 <- sum(nonstat_env1["Miss"])
#fa_env2.3 <- sum(nonstat_env1["FA"])
#cr_env2.3 <- sum(nonstat_env1["CR"])


# #### Hit Rate and False Alarm Rate for Environments 1 & 2
# e1.2.hit.rate <- c()
# e1.2.fa.rate <- c()
# e2.2.hit.rate <- c()
# e2.2.fa.rate <- c()
# 
# e1.3.hit.rate <- c()
# e1.3.fa.rate <- c()
# e2.3.hit.rate <- c()
# e2.3.fa.rate <- c()
# 
# 
# e1.hit.rate.2 <- hit_env1.2/sum(hit_env1.2, miss_env1.2)
# e1.fa.rate.2 <- fa_env1.2/sum(fa_env1.2,cr_env1.2)
# e2.hit.rate.2 <- hit_env2.2/sum(hit_env2.2, miss_env2.2)
# e2.fa.rate.2 <- fa_env2.2/sum(fa_env2.2,cr_env2.2)
# 
# e1.hit.rate.3 <- hit_env1.3/sum(hit_env1.3, miss_env1.3)
# e1.fa.rate.3 <- fa_env1.3/sum(fa_env1.3,cr_env1.3)
# e2.hit.rate.3 <- hit_env2.3/sum(hit_env2.3, miss_env2.3)
# e2.fa.rate.3 <- fa_env2.3/sum(fa_env2.3,cr_env2.3)



##########################################
# for (i in 1:nrow(hit_env1)){
#	# e1.hit.rate[i] <- hit_env1$Hit[i]/sum(hit_env1$Hit[i],miss_env1$Miss[i])
# }
#
# for (i in 1:nrow(fa_env1)){
#	# e1.fa.rate[i] <- fa_env1$FA[i]/sum(fa_env1$FA[i],cr_env1$CR[i])
# }
#
# for (i in 1:nrow(hit_env2)){
#	# e2.hit.rate[i] <- hit_env2$Hit[i]/sum(hit_env2$Hit[i],miss_env2$Miss[i])
# }
#
# for (i in 1:nrow(fa_env2)){
#	# e2.fa.rate[i] <- fa_env2$FA[i]/sum(fa_env2$FA[i],cr_env2$CR[i])
# }
# #### Calculating d' and beta for Environments 1 & 2
# e1_d_prime.2 <- dprime(e1.hit.rate.2,e1.fa.rate.2)
# e1_b.2 <- beta(e1.hit.rate.2,e1.fa.rate.2)
# e2_d_prime.2 <- dprime(e2.hit.rate.2,e2.fa.rate.2)
# e2_b.2 <- beta(e2.hit.rate.2,e2.fa.rate.2)
# 
# e1_d_prime.3 <- dprime(e1.hit.rate.3,e1.fa.rate.3)
# e1_b.3 <- beta(e1.hit.rate.3,e1.fa.rate.3)
# e2_d_prime.3 <- dprime(e2.hit.rate.3,e2.fa.rate.3)
# e2_b.3 <- beta(e2.hit.rate.3,e2.fa.rate.3)


#setwd("C:/Users/1513290957/Desktop/ICCM_Data_and_Paper/randomSeed8")
############################################################################################################################
## Plotting

## Model Summary ###########################################################################################################

# png("ROC80p.png")
# a<-0:1000/1000
# contour(z=outer(a,a,"dprime"))
# points(e1.fa.rate.2,e1.hit.rate.2,col="green",lwd=2)
# points(e1.fa.rate.3,e1.hit.rate.3,col="darkgreen",lwd=2)
# points(e2.fa.rate.2,e2.hit.rate.2,col="gray",lwd=2)
# points(e2.fa.rate.3,e2.hit.rate.3,col="black",lwd=2)
# 
# legend("topright",legend=c("no DSS","Correct DSS"), lty=c(1,1,1,2,1,2,1,2), col=c("gray","green"),
# 	cex=1.5, bty="n", lwd=2)
# 
# dev.off()



#################
## ggplot test ##
#################

#ACC.80.summary <- summarySE(m.80NoCost, measurevar="accuracy", groupvars=c("block", "cond", "lineType", "lineColor"))
#ruleAd.80.summary <- summarySE(m.80NoCost, measurevar="FFT.comp", groupvars=c("block", "cond", "lineType", "lineColor"))

ACC.summary <- summarySE(all.m.data, measurevar="accuracy", groupvars=c("block", "cond", "lineType", "lineColor"))
RT.summary <- summarySE(all.m.data, measurevar="responseTime", groupvars=c("block", "cond", "lineType", "lineColor"))
RTbyCues.summary <- summarySE(all.m.data, measurevar = "RT.by.Cues", groupvars = c("block", "cond", "lineType", "lineColor"))
ruleAd.summary <- summarySE(all.m.data, measurevar="FFT.comp", groupvars=c("block", "cond", "lineType", "lineColor"))
cuesView.summary <- summarySE(all.m.data, measurevar="Cues.Viewed", groupvars=c("block", "cond", "lineType", "lineColor"))
#score.summary <- summarySE(all.m.data, measurevar="runningScore", groupvars=c("block", "cond", "lineType", "lineColor"))
score.summary <- summarySE(all.m.data, measurevar="scaledScore", groupvars=c("block", "cond", "lineType", "lineColor"))
reward.summary <- summarySE(all.m.data, measurevar="reward", groupvars=c("block", "cond", "lineType", "lineColor"))
#score.summary <- summarySE(all.m.data, measurevar="runningScore", groupvars=c("block", "cond"))

### ALL ACC ###
###############
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
d <- ggplot(ACC.summary, aes(x=block, y=accuracy, shape=lineColor)) +
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #geom_segment(aes(x=2.5, y=0.3, xend=2.5, yend=0.9), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("Accuracy") +
  scale_color_hue(name="",
                   breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost", "m.90noCost"),
                   l=40) +
  ggtitle("") +
  expand_limits(y=0.3) +
  scale_y_continuous(limits = c(0.3, 1.0), expand = c(0,0), breaks=0:12*0.1) +
  scale_x_continuous(breaks=1:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))

d
#d + scale_color_manual(values=c("#007f85"))

### ALL RT ###
##############
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
ggplot(RT.summary, aes(x=block, y=responseTime, shape=lineColor)) +
  geom_errorbar(aes(ymin=responseTime-se, ymax=responseTime+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #geom_segment(aes(x=2.5, y=1, xend=2.5, yend=6), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("Response Time (s)") +
  scale_color_hue(name="",
                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
                  l=40) +
  ggtitle("") +
  expand_limits(y=0) +
  scale_y_continuous(limits = c(1, 9), expand = c(0,0), breaks=0:9*1) +
  scale_x_continuous(breaks=1:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))

### ALL RTbyCue ###
##############
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
ggplot(RTbyCues.summary, aes(x=block, y=RT.by.Cues, shape=lineColor)) +
  geom_errorbar(aes(ymin=RT.by.Cues-se, ymax=RT.by.Cues+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #geom_segment(aes(x=2.5, y=1, xend=2.5, yend=6), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("RT by Cues Viewed") +
  scale_color_hue(name="",
                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
                  l=40) +
  ggtitle("") +
  expand_limits(y=0) +
  scale_y_continuous(limits = c(1, 5), expand = c(0,0), breaks=0:12*1) +
  scale_x_continuous(breaks=1:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))

### ALL Rule Adherence ###
##########################
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
ggplot(ruleAd.summary, aes(x=block, y=FFT.comp, shape=lineColor)) +
  geom_errorbar(aes(ymin=FFT.comp-se, ymax=FFT.comp+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #geom_segment(aes(x=2.5, y=0.0, xend=2.5, yend=1.0), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("Proportion of Rule Adherence") +
  scale_color_hue(name="",
                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
                  l=40) +
  ggtitle("") +
  expand_limits(y=0) +
  scale_y_continuous(limits = c(0.0, 1.0), expand = c(0,0), breaks=0:12*0.2) +
  scale_x_continuous(breaks=1:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))

### ALL Cues Viewed ###
#######################
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
ggplot(cuesView.summary, aes(x=block, y=Cues.Viewed, shape=lineColor)) +
  geom_errorbar(aes(ymin=Cues.Viewed-se, ymax=Cues.Viewed+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #geom_segment(aes(x=2.5, y=0, xend=2.5, yend=3), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("Cues Viewed") +
  scale_color_hue(name="",
                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
                  l=40) +
  ggtitle("") +
  expand_limits(y=0) +
  scale_y_continuous(limits = c(0.0, 3.5), expand = c(0,0), breaks=0:3*1) +
  scale_x_continuous(breaks=1:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))


### Total Score ###
#######################
#windowsFonts(Times=windowsFont("Asap"))
#pd <- position_dodge(0.07)
#ggplot(score.summary, aes(x=block, y=scaledScore, shape=lineColor)) +
#  geom_errorbar(aes(ymin=scaledScore-se, ymax=scaledScore+se, color=lineColor), size=1.2, width=.6, position=pd) +
#  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
#  geom_point(aes(color=lineColor), size=5, position=pd) +
#  geom_segment(aes(x=2.5, y=0, xend=2.5, yend=12000), size=1.2, color="black", linetype="dashed") +
#  xlab("Block") +
#  ylab("Cumulative Score (in thousands") +
#  scale_color_hue(name="",
#                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
#                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
#                  l=40) +
#  ggtitle("") +
#  expand_limits(y=0) +
#  #expand_limits(x=0) +
#  scale_y_continuous(limits = c(20000, 40000), expand = c(0,0), breaks=1:40*4000) +
#  scale_x_continuous(breaks=1:10*1) +
#  #scale_x_continuous(limits=c(0,9), expand= c(0,0), breaks=0:10*1) +
#  theme_bw() +
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.background = element_blank(),
#        panel.border = element_blank(),
#        text = element_text(family="Times"),
#        legend.justification=c(1,0),
#        legend.position="none",
#        legend.text=element_text(size=48),
#        axis.ticks=element_line(size=3),
#        axis.ticks.length=unit(0.5, "cm"),
#        axis.line.x = element_line(size=2, linetype='solid'),
#        axis.line.y = element_line(size=2, linetype='solid'),
#        axis.text.x = element_text(size=48),
#        axis.text.y = element_text(size=48),
#        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
#        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))


### Total Score ###
#######################
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
ggplot(score.summary, aes(x=block, y=scaledScore, shape=lineColor)) +
  geom_errorbar(aes(ymin=scaledScore-se, ymax=scaledScore+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #  geom_segment(aes(x=2.5, y=0, xend=2.5, yend=12000), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("Cumulative Score (in thousands)") +
  scale_color_hue(name="",
                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
                  l=40) +
  ggtitle("") +
  expand_limits(y=0) +
  #expand_limits(x=0) +
  scale_y_continuous(limits = c(20, 40), expand = c(0,0), breaks=1:40*4) +
  scale_x_continuous(breaks=1:10*1) +
  #scale_x_continuous(limits=c(0,9), expand= c(0,0), breaks=0:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))


### Reward by Trial ###
#######################
windowsFonts(Times=windowsFont("Asap"))
pd <- position_dodge(0.07)
ggplot(reward.summary, aes(x=block, y=reward, shape=lineColor)) +
  geom_errorbar(aes(ymin=reward-se, ymax=reward+se, color=lineColor), size=1.2, width=.6, position=pd) +
  geom_line(aes(color=lineColor, linetype=lineType), position=pd, size=1.7) +
  geom_point(aes(color=lineColor), size=5, position=pd) +
  #  geom_segment(aes(x=2.5, y=0, xend=2.5, yend=12000), size=1.2, color="black", linetype="dashed") +
  xlab("Block") +
  ylab("Average Reward by Block") +
  scale_color_hue(name="",
                  breaks=c("m.80Cost30", "m.90Cost30", "m.80NoCost"),
                  labels=c("No DSS", "Incorrect DSS", "Correct DSS"),
                  l=40) +
  ggtitle("") +
  expand_limits(y=0) +
  #expand_limits(x=0) +
  scale_y_continuous(limits = c(-60, 100), expand = c(0,0), breaks=-20:10*20) +
  scale_x_continuous(breaks=1:10*1) +
  #scale_x_continuous(limits=c(0,9), expand= c(0,0), breaks=0:10*1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family="Times"),
        legend.justification=c(1,0),
        legend.position="none",
        legend.text=element_text(size=48),
        axis.ticks=element_line(size=3),
        axis.ticks.length=unit(0.5, "cm"),
        axis.line.x = element_line(size=2, linetype='solid'),
        axis.line.y = element_line(size=2, linetype='solid'),
        axis.text.x = element_text(size=48),
        axis.text.y = element_text(size=48),
        axis.title.x = element_text(size=50, margin = margin(5, 0, 0, 0)),
        axis.title.y = element_text(size=50, margin = margin(0, 20, 0, 0)))

ggplot(m.80Cost30, aes(m.80Cost30$strategy)) + geom_histogram() + facet_grid(~block)
ggplot(m.80NoCost, aes(m.80NoCost$strategy)) + geom_histogram() + facet_grid(~block)
ggplot(m.90Cost30, aes(m.90Cost30$strategy)) + geom_histogram() + facet_grid(~block)
ggplot(m.90NoCost, aes(m.90NoCost$strategy)) + geom_histogram() + facet_grid(~block)

ggplot(data=m.80NoCost.Bar, aes(freq)) + geom_bar() + facet_wrap(~ block) + ylim(0, 10)
ggplot(data=m.90Cost30.Bar, aes(freq)) + geom_bar() + facet_wrap(~ block) + ylim(0, 10)
ggplot(data=m.90NoCost.Bar, aes(freq)) + geom_bar() + facet_wrap(~ block) + ylim(0, 10)
