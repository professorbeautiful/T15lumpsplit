###   bigDataComponent

source('jumpBackWithPanel_bigDataController.R', local=TRUE)

printBDCProgress =  TRUE

# Usage:  bigDataComponent(analysisName='qValue')

createBigDataParamObserver <- function(analysisName) {
  myName = paste0('bigDataParamObserver_', analysisName)
  analysisNumber = match(analysisName, names(jumpList_BDC))
#  theBigDataController = paste0('BigDataController_ID_', analysisNumber)
  thisTauTrueID = get_thisTauTrueID(analysisNumber)
  ####  Initially, only tauTrue.
  assign(myName,
         pos=1,
         observeEvent(
           label=myName,
           eventExpr = input[[thisTauTrueID]], ## tauTrueID
           handlerExpr = {
             cat('START: handler for ', myName)
             try(silent = FALSE, {
               isolate({
                 currentBDCnumber = mapAnalysisToBDCnumber[analysisName]

               }) ### End of isolate()
             }) ### End of try()
           }
         )
  )
}   ### End of createDLdataChoiceObserver()

#### bigDataComponent ####
bigDataComponent = function( analysisName) {
  createBigDataParamObserver(analysisName)
  analysisNumber = which(analysisName == names(jumpList_BDC))
  thisBDCNumber = nextNumber(sequenceType = "BDC")
  if(printBDCProgress ) cat('Creating bigDataComponent thisBDCNumber = ', thisBDCNumber, '\n')
  if(printBDCProgress ) cat('BDCNumber=', thisBDCNumber, '   analysisNumber=', analysisNumber, '\n')
  outputIdThisBDC = paste0('outputBDC', thisBDCNumber)
  panelIdThisBDC = paste0('idPanelBDC', thisBDCNumber)
  resetIdThisBDC = paste0('idResetBDC', thisBDCNumber)
  myChoiceIdThisBDC = paste0('idMyChoiceBDC', thisBDCNumber)

  #### resetIdThisBDC button ####
  'When resetIdThisBDC Button  is clicked, set tauTrue to zero.'
  myName = paste0('observeEvent_resetIdThisBDC_', thisBDCNumber)
  assign(myName,
         pos=1,
         observeEvent(label = myName,
                      eventExpr = input[[resetIdThisBDC]],
                      handlerExpr =  {
                        isolate({
                          cat('Handler:  Pressed ', resetIdThisBDC, '\n')
                          currentBDCnumber = mapAnalysisToBDCnumber[analysisName]

                        })
                      })
  )

  #### myChoiceIdThisBDC button -- update numericInputs restoring MyChoice data ####
  myName = paste0('observeEvent_myChoiceIdThisBDC_', thisBDCNumber)
  assign(myName, pos=1,
         observeEvent(
           label = myName,
           eventExpr = input[[myChoiceIdThisBDC]],
           handlerExpr =  {
             isolate({
               cat('Handler: Pressed ', myChoiceIdThisBDC, '\n')
               #enable(resetIdThisBDC)
               #disable(myChoiceIdThisBDC)
               currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
             })
           })
  )
  #### Output of bigDataComponent ####
  if(printBDCProgress )
      cat('bigDataComponent for ', analysisName, ' ', thisBDCNumber, '\n')
  output[[outputIdThisBDC]] = renderUI({
    fluidRow(
      column(6,
             numericInput(thisTauTrueID, 'Variance of the Odds Ratio ',
                          value = 0, min=0,  step=0.1)),
      column(6,
             jumpBackWithPanel_BDC(analysisNumber, thisBDCNumber)

    ) )
  })
  uiOutput(outputId = outputIdThisBDC)
}
