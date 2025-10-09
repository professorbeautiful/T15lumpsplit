####   Lump-Split-crossvalidation
####    Illustrating cross-validation with the Dark-Light dataset

penaltyFunction = function(outcome, prediction) {

}

doCVanalysis <- function(thisData ) {
   penaltychoice = 'LEAST SQUARES'
  # LOSS: LEAST SQUARES
  if(penaltychoice == 'LEAST SQUARES')
    penaltyFunction =  function(outcome, prediction)
      switch(outcome, R=(1-prediction)^2, N=prediction^2)
  # LOSS: EXPONENTIAL
  if(penaltychoice == 'EXPONENTIAL')
    penaltyFunction =  function(outcome, prediction)
      exp(-switch(outcome, R=1, N=-1)*prediction)
  # LOSS: LOG LIKELIHOOD X (-1)
  if(penaltychoice == 'LOG LIKELIHOOD X (-1)')
    penaltyFunction = function(outcome, prediction)
      -log(dbinom((outcome=='R'), size = 1, prob=prediction))

  #cat("mychoice data", paste(getDLdata(myChoice=TRUE, analysisName) ), '\n')
  #cat("thisData", paste(thisData ), '\n')

  proportionOverall = sum(thisData['R', ])/sum(thisData)
  proportionThisGroup =  sum(thisData['R', 'D'])/sum(thisData[ , 'D' ])

  leaveOneOut <<- function(weight=1/2, outcome, feature) {
    smallerDataSet = thisData
    smallerDataSet[outcome=outcome, feature=feature] =
      smallerDataSet[outcome=outcome, feature=feature] - 1
    proportionOverallSmaller = sum(smallerDataSet['R', ])/sum(smallerDataSet)
    proportionThisGroupSmaller =  sum(smallerDataSet['R', feature])/sum(smallerDataSet[ , feature ])
    prediction = weight*proportionThisGroupSmaller + (1-weight)*proportionOverallSmaller
    penalty = penaltyFunction(outcome, prediction) * thisData[outcome, feature]
    return(penalty)
  }

  totalPenalty <<- function(weight, ...)
    leaveOneOut(weight=weight, feature='D', outcome='R', ...) +
    leaveOneOut(weight=weight, feature='D', outcome='N', ...) +
    leaveOneOut(weight=weight, feature='L', outcome='R', ...) +
    leaveOneOut(weight=weight, feature='L', outcome='N', ...)

  weights<-seq(0,1,length=100)
  estimators = weights*proportionThisGroup+(1-weights)*proportionOverall

  penaltyVector = sapply(weights, totalPenalty)

  optimalWeight = weights[which(penaltyVector==min(penaltyVector))]  [1]
  optimalWeightRounded = round(optimalWeight, digits = 2)
  proportionOverall = sum(thisData[ 'R', ])/sum(thisData)
  proportionThisGroup =  sum(thisData[ 'R', 'D'])/sum(thisData[ , 'D'])
  optimalEstimate = estimators[which(weights==optimalWeight)] [1]
  optimalEstimateRounded = round(optimalEstimate, digits = 2)
  CVoptimalEstimate <<- optimalEstimate
  summaryText = paste(
    'CV opt est=', signif(optimalEstimate, digits=2),
    ' (Lump ', signif(1-optimalWeight, digits=2), ', Split ', optimalWeightRounded, ')')
  #shinyjs::alert(summaryText)
  penaltyAtOpt = totalPenalty(optimalWeight)

  # objects =   c('summaryText', 'optimalWeight', 'weights', 'penaltyVector',
  #               'penaltyAtOpt', 'CVoptimalEstimate')
  # result = lapply(objects, get)
  # names(result) = objects
  result = list(summaryText=summaryText, optimalWeight=optimalWeight,
                weights=weights, penaltyVector=penaltyVector,
                estimators=estimators,
                penaltyAtOpt=penaltyAtOpt, CVoptimalEstimate=CVoptimalEstimate,
                proportionOverall=proportionOverall,
                proportionThisGroup=proportionThisGroup)
  return(result)
}


output$crossvalidationPlot = renderPlot({
  analysisName = 'crossvalidationPlot'
  source(analysisReactiveSetup_DTC, local=TRUE)

  result = doCVanalysis(thisData)
  #print(names(result))
  for(ob in names(result))
    assign(ob, result[[ob]])
  #####  plot with two vertical axes #####
  savedPar <- par(mai = c(1.2, .8, .2, .8))
  # A numerical vector of the form c(bottom, left, top, right)
  # which gives the margin size specified in inches.
  # Increasing the 4th entry allows room for a right-side label.

  plot(x=weights,
       y=penaltyVector,
         xlab='weights',
         ylab="",
         #     ylim=range(penaltyVector)),
         type='l', col='purple', axes=F)
    axis(1)
    axis(2, col='purple', col.axis='purple')
    mtext(text = 'penalty', side = 2, col='purple',line=2)

    points(optimalWeight,
           penaltyAtOpt,
           col='purple', pch=17, cex=2)
    text(optimalWeight,
         penaltyAtOpt,
         col='purple', pos=3,
         labels = round(digits=2, optimalWeight))
    abline(h=min(penaltyVector),col='purple')
    title(paste('cross-validation optimization'),
          'lump <-------------------------------------------> split')

    #####  adding a right-hand-side vertical axis #####
    par(new=T)  ## Which of course means "new=F"!
    plot(weights, estimators, axes=F, type='l', lty=2,
         col='black', ylab='', ylim=c(0.0, proportionThisGroup*1.05))
    axis(4, col='black', col.axis='black')
    mtext('estimate Pr(R|D)', side = 4, col='black', line = 2)
    points(optimalWeight, CVoptimalEstimate,
           col='black', pch=17, cex=2)
    text(0, proportionOverall,
         as.character(round(digits=2, proportionOverall)),
         col='black', adj=0)
    text(1, proportionThisGroup,
         as.character(round(digits=2, proportionThisGroup)),
         col='black', adj=1)
    text(x = optimalWeight,
         y = estimators[which(weights==optimalWeight)],
         labels = round(digits=2, CVoptimalEstimate),
         col='black', pos = 3)
    mtext(summaryText, side = 1, line=5)
    par(savedPar)  # restore original plot settings
  }
)
