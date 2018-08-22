####   Lump-Split-crossvalidation
####    Illustrating cross-validation with the Dark-Light dataset

output$crossvalidationPlot = renderPlot({
    theData=rValues$DLdata
    proportionOverall = sum(theData['R', ])/sum(theData)
    proportionThisGroup =  sum(theData['R', 'D'])/sum(theData[ , 'D' ])

    leaveOneOut <<- function(
      weight=1/2, outcome, feature,
      penaltyFunction = function(outcome, prediction)
        switch(outcome, R=(1-prediction)^2, N=prediction^2)) {
      smallerDataSet = theData
      smallerDataSet[outcome=outcome, feature=feature] =
        smallerDataSet[outcome=outcome, feature=feature] - 1
      proportionOverallSmaller = sum(smallerDataSet['R', ])/sum(smallerDataSet)
      proportionThisGroupSmaller =  sum(smallerDataSet['R', feature])/sum(smallerDataSet[ , feature ])
      prediction = weight*proportionThisGroupSmaller + (1-weight)*proportionOverallSmaller
      penalty = penaltyFunction(outcome, prediction) * theData[outcome, feature]
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
    estimators = weights*proportionThisGroup+(1-weights)*proportionOverall

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
         type='l', col='orange', axes=F)
    axis(1)
    axis(2, col='orange', col.axis='orange')
    mtext(text = 'penalty', side = 2, col='orange',line=2)
    optimalWeight = weights[which(penaltyVector==min(penaltyVector))]  [1]
    #cat('optimalWeight = ', optimalWeight, '\n')
    proportionOverall = sum(theData[ 'R', ])/sum(theData)
    proportionThisGroup =  sum(theData[ 'R', 'D'])/sum(theData[ , 'D'])
    optimalEstimate = estimators[which(weights==optimalWeight)]
    rValues$CVoptimalEstimate = optimalEstimate
    #cat('optimal estimate for dark = ', optimalEstimate, '\n')
    points(optimalWeight,
           totalPenalty(optimalWeight, penalty=penaltyFunction),
           col='orange', pch=17, cex=2)
    text(optimalWeight,
         totalPenalty(optimalWeight, penalty=penaltyFunction),
         col='orange', pos=3,
         labels = round(digits=2, optimalWeight))
    abline(h=min(penaltyVector),col='orange')
    title('cross-validation optimization',
          'lump <-------------------------------------------> split')

    #####  adding a right-hand-side vertical axis #####
    par(new=T)
    plot(weights, estimators, axes=F, type='l', lty=2,
         col='blue', ylab='', ylim=c(0.0, proportionThisGroup*1.05))
    axis(4, col='blue', col.axis='blue')
    mtext('estimate Pr(R|D)', side = 4, col='blue', line = 2)
    points(optimalWeight, optimalEstimate,
           col='blue', pch=17, cex=2)
    text(0, proportionOverall,
         as.character(round(digits=2, proportionOverall)),
         col='blue', adj=0)
    text(1, proportionThisGroup,
         as.character(round(digits=2, proportionThisGroup)),
         col='blue', adj=1)
    text(optimalWeight,
         estimators[which(weights==optimalWeight)],
         round(digits=2, optimalEstimate),
         col='blue', pos = 3)
    par(savedPar)  # restore original plot settings
  }
)
