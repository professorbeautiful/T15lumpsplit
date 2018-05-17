####   Lump-Split-crossvalidation
####    Illustrating cross-validation with the Dark-Light dataset

DLdata <<- matrix(c(3,5,2,90),nrow=2)
dimnames(DLdata) = list(feature=c("D","L"), outcome=c("R","N"))
#DLlong = data.frame(expand.grid(dimnames(DLdata)))
#DLlong$n = c(DLdata)
#t(DLdata)
leaveOneOut <<- function(weight=1/2, feature, outcome,
                       penaltyFunction = function(outcome, prediction)
                         switch(outcome, R=(1-prediction)^2, N=prediction^2)) {
  smallerDataSet = DLdata
  smallerDataSet[feature=feature, outcome=outcome] =
    smallerDataSet[feature=feature, outcome=outcome] - 1
  proportionOverall = sum(smallerDataSet[ , 'R'])/sum(smallerDataSet)
  proportionThisGroup =  sum(smallerDataSet[ feature, 'R'])/sum(smallerDataSet[feature, ])
  prediction = weight*proportionThisGroup + (1-weight)*proportionOverall
  penalty = penaltyFunction(outcome, prediction) * DLdata[feature, outcome]
  return(penalty)
}

totalPenalty <<- function(weight, ...)
  leaveOneOut(weight=weight, feature='D', outcome='R', ...) +
  leaveOneOut(weight=weight, feature='D', outcome='N', ...) +
  leaveOneOut(weight=weight, feature='L', outcome='R', ...) +
  leaveOneOut(weight=weight, feature='L', outcome='N', ...)

# LOSS: LEAST SQUARES
penaltyFunction <<- function(outcome, prediction)
                         switch(outcome, R=(1-prediction)^2,
                                N=prediction^2)
# penaltyFunction = function(outcome, prediction)
#                          switch(outcome, R=(1-prediction)^2, N=prediction^2))
# LOSS: EXPONENTIAL
# penaltyFunction =  function(outcome, prediction)
#                          exp(-switch(outcome, R=1, N=-1)*prediction))
# LOSS: LOG LIKELIHOOD X (-1)
# penaltyFunction = function(outcome, prediction)
#   -log(dbinom((outcome=='R'), size = 1, prob=prediction))
weights<-seq(0,1,length=100)
estimators = weights*0.60+(1-weights)*0.08

penaltyVector = sapply(weights, totalPenalty,
                       penalty= penaltyFunction)

#####  plot with two vertical axes #####
savedPar <- par(mai = c(1.2, .8, .2, .8))
# A numerical vector of the form c(bottom, left, top, right)
# which gives the margin size specified in inches.
# Increasing the 4th entry allows room for a right-side label.
plot(x=weights,
     y=penaltyVector,
     xlab='weights',
     ylab="",
#     ylim=c(0,max(penaltyVector)),
     type='l', col='red', axes=F)
axis(1)
axis(2, col='red', col.axis='red')
mtext(text = 'penalty', side = 2, col='red',line=2)
optimalWeight = weights[which(penaltyVector==min(penaltyVector))]  [1]
cat('optimalWeight = ', optimalWeight, '\n')
proportionOverall = sum(DLdata[ , 'R'])/sum(DLdata)
proportionThisGroup =  sum(DLdata[ 'D', 'R'])/sum(DLdata['D', ])
optimalEstimate = estimators[which(weights==optimalWeight)]
cat('optimal estimate for dark = ', optimalEstimate, '\n')
points(optimalWeight,
       totalPenalty(optimalWeight, penalty=penaltyFunction),
       col='red', pch=17, cex=2)
text(optimalWeight,
       totalPenalty(optimalWeight, penalty=penaltyFunction),
       col='red', pos=3,
     labels = round(digits=2, optimalWeight))
abline(h=min(penaltyVector),col='red')
title('cross-validation optimization',
      'lump <---------------------------------> split')

#####  adding a right-hand-side vertical axis #####
par(new=T)
plot(weights, estimators, axes=F, type='l', lty=2,
      col='darkgreen', ylab='', ylim=c(0.05, 0.60))
axis(4, col='darkgreen', col.axis='darkgreen')
mtext('estimate Pr(R|D)', side = 4, col='darkgreen', line = 2)
points(optimalWeight, optimalEstimate,
       col='darkgreen', pch=17, cex=2)
text(0, 0.08, "0.08", col='darkgreen', adj=0)
text(1, 0.60, "0.60", col='darkgreen', adj=1)
text(optimalWeight,
     estimators[which(weights==optimalWeight)],
    round(digits=2, optimalEstimate),
    col='darkgreen', pos = 3)
par(savedPar)  # restore original plot settings
