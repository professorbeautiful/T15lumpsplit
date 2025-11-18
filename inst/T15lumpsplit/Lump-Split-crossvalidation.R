####   Lump-Split-crossvalidation
####    Illustrating cross-validation with the Dark-Light dataset

penaltyFunction = function(outcome, prediction) {

}


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

leaveOneOut <<- function(splitWeight=1/2, outcome, feature) {
  ## subtract one from the outcome,feature cell.

  smallerDataSet = thisData
  smallerDataSet[outcome=outcome, feature=feature] =
    smallerDataSet[outcome=outcome, feature=feature] - 1
  proportionOverallSmaller = sum(smallerDataSet['R', ])/sum(smallerDataSet)
  # proportion of responses in the entire dataset
  proportionThisGroupSmaller =  sum(smallerDataSet['R', feature])/sum(smallerDataSet[ , feature ])
  # proportion of responses in the D group
  prediction = splitWeight*proportionThisGroupSmaller + (1-splitWeight)*proportionOverallSmaller
  # shrinking towards proportionOverallSmaller
  penalty = penaltyFunction(outcome, prediction) * thisData[outcome, feature]
  return(penalty)
}


### totalPenalty adds over each cell, not each patient.
# one could also weight these penalties by the number in each cell.
totalPenalty <<- function(splitWeight, ...)
  leaveOneOut(splitWeight=splitWeight, feature='D', outcome='R', ...) +
  leaveOneOut(splitWeight=splitWeight, feature='D', outcome='N', ...) +
  leaveOneOut(splitWeight=splitWeight, feature='L', outcome='R', ...) +
  leaveOneOut(splitWeight=splitWeight, feature='L', outcome='N', ...)

doCVanalysis <- function(thisData, splitWeights=seq(0,1,length=100) ) {
  #cat("mychoice data", paste(getDLdata(myChoice=TRUE, analysisName) ), '\n')
  #cat("thisData", paste(thisData ), '\n')

  proportionOverall = sum(thisData['R', ])/sum(thisData)
  proportionThisGroup =  sum(thisData['R', 'D'])/sum(thisData[ , 'D' ])

  estimators = splitWeights*proportionThisGroup+(1-splitWeights)*proportionOverall

  penaltyVector = sapply(splitWeights, totalPenalty)

  optimalWeight = splitWeights[which(penaltyVector==min(penaltyVector))]  [1]
  optimalWeightRounded = round(optimalWeight, digits = 2)
  proportionOverall = sum(thisData[ 'R', ])/sum(thisData)
  proportionThisGroup =  sum(thisData[ 'R', 'D'])/sum(thisData[ , 'D'])
  optimalEstimate = estimators[which(splitWeights==optimalWeight)] [1]
  optimalEstimateRounded = round(optimalEstimate, digits = 2)
  CVoptimalEstimate <<- optimalEstimate
  summaryText = paste(
    'CV opt est=', signif(optimalEstimate, digits=2),
    ' (Lump ', signif(1-optimalWeight, digits=2), ', Split ', optimalWeightRounded, ')')
  #shinyjs::alert(summaryText)
  penaltyAtOpt = totalPenalty(optimalWeight)

  # objects =   c('summaryText', 'optimalWeight', 'splitWeight', 'penaltyVector',
  #               'penaltyAtOpt', 'CVoptimalEstimate')
  # result = lapply(objects, get)
  # names(result) = objects
  result = list(summaryText=summaryText, optimalWeight=optimalWeight,
                splitWeights=splitWeights, penaltyVector=penaltyVector,
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

  plot(x=splitWeights,
       y=penaltyVector,
         xlab='weight for Split',
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
    plot(splitWeights, estimators, axes=F, type='l', lty=2,
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
         y = estimators[which(splitWeights==optimalWeight)],
         labels = round(digits=2, CVoptimalEstimate),
         col='black', pos = 3)
    mtext(summaryText, side = 1, line=5)
    par(savedPar)  # restore original plot settings
  }
)
