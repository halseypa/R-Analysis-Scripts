library(mvnormtest)
library(car)
library(MASS)
library(dplyr)
library(tsDyn)
library(Hmisc)
library(PerformanceAnalytics)

#####################################IMPORT THE DATA#########################################

#Import 80% 30Cost Data
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

all.m.data <- rbind(m.80NoCost, m.80Cost30, m.90NoCost, m.90Cost30)
#######################################END OF DATA IMPORT########################@@@##########

group <- all.m.data$eid
test.variables <- cbind(all.m.data$runningScore, all.m.data$responseTime, all.m.data$accuracy)


#############################MANOVA ASSUMPTION TESTS###########################################

#test normality of variables
#shapiro.test(rawData$NAMES)
#shapiro.test(rawData$NAMES)
#shapiro.test(rawData$NAMES)

#visual plots of homogeneity of variance
plot(responseTime ~ group, data=all.m.data)
# and all other visualizations #

#test of homogeneity of variance
leveneTest(responseTime ~ group, data = all.m.data)

#i have no idea what additional variable to do. some just aren't going to have normal variance
#leveneTest(cueCount ~ group, data = all.m.data)

#to do accuracy, would have to do cumulative by person to test normal distribution
#leveneTest(accuracy.summary ~ group, data = all.m.data)

#visual test of linearity
res <- rcorr(as.matrix(test.variables))
signif(res$r, 2)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res$r, res$P)
chart.Correlation(test.variables, histogram=TRUE, pch=19)
#test of linearity
#TVAR.LRtest(data, lag=2, mTh=1, thDelay=1:2, nboot=3, plot=FALSE, trim=0.1, test="1vs")

################################END OF MANOVA ASSUMPTIONS###########################################

n <- length(group)

MV <- lm(test.variables~group, data=all.m.data)
#summary(MV)
summary(MV, test = "Hotelling-Lawley")
summary(MV, test = "Roy")
summary(MV, test = "Pillai")
summary(MV, test = "Wilks")

# other tests for MANOVA
summary.aov(MV)