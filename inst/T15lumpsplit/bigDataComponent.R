###   bigDataComponent


printBDCProgress =  FALSE

printBDCProgress = FALSE

# Usage:  bigDataComponent(analysisName='qValue')

handlerForobserveEvent_myChoiceIdThisBDC = function(
  myChoiceIdThisBDC, analysisName, thisOmegaID){
  if(printBDCProgress)
    cat('Handler: Pressed ', myChoiceIdThisBDC, '\n')
  #enable(resetIdThisBDC)
  #disable(myChoiceIdThisBDC)
  currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
  BigDataMyChoice = getBigData(analysisName, myChoice=TRUE)
  BigData_Omega_MyChoice = getBigData_Omega(analysisName, myChoice=TRUE)
  if(printBDCProgress)
    cat('   Copying BigData_Omega_MyChoice into numericinput: ',
      paste(BigData_Omega_MyChoice), '\n')
  updateNumericInput(
    session,
    thisOmegaID,
    value = BigData_Omega_MyChoice
  )
  setBigData(BigDataMyChoice, analysisName)
  BigDataLastUsed <<- BigDataMyChoice
  setBigData_Omega(BigData_Omega_MyChoice, analysisName)
  OmegaLastUsed <<- BigData_Omega_MyChoice
  if(printBDCProgress)
    cat('   So now OmegaLastUsed is the same: ',
      paste(OmegaLastUsed), '\n')

}  #### End of handlerForobserveEvent_myChoiceIdThisBDC

createBigDataParamChoiceObserver <- function(analysisName) {
  myName = paste0('bigDataParamChoiceObserver_', analysisName)
  analysisNumber = match(analysisName, names(jumpList_BDC))
  thisOmegaID = get_thisOmegaID(analysisNumber)
  #cat(myName, ': thisOmegaID=', thisOmegaID, '\n')
  ####  Initially, only Omega.
  assign(myName,
         pos=1,
         observeEvent(
           label=myName,
           eventExpr = input[[thisOmegaID]], ## OmegaID
           handlerExpr = {
             if(printBDCProgress)
               cat('START: handler for ', myName, ' new input tau = /',
                 input[[thisOmegaID]], '/\n')
             try(silent = FALSE, {
               isolate({
                 if(printBDCProgress)
                   cat('updateBigDataParamMyChoice: changing MyChoice')
                 currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
                 if( ! exists('saved_BigDataMyChoice')) {
                   if(printBDCProgress)
                     cat(': Responding to numericInput for Omega: ',
                       (input[[thisOmegaID ]]))
                   if( is.null(getBigData(analysisName, myChoice=TRUE)))
                     BigDataMyChoice = BigDataOriginal
                   else
                     BigDataMyChoice = getBigData(analysisName, myChoice=TRUE)
                   newValue = makeSureOmegaIsGood(input[[thisOmegaID]])
                   #newValue = round(newValue)
                   if(newValue == 0) set.seed(45)
                   BigDataMyChoice = makeBigDataWithFeatures(
                     DLdataOriginal,
                     Omega=newValue)
                   setBigData_Omega(value=newValue, analysisName=analysisName)
                   setBigData_Omega(value=newValue, analysisName=analysisName, myChoice=TRUE)
                   setBigData(BigDataMyChoice, analysisName)
                   setBigData(BigDataMyChoice, analysisName, myChoice=TRUE)
                   if(printBDCProgress) {
                     cat('BigDataMyChoice for ',  analysisName, ' is:\n')
                     print(BigDataMyChoice[1:2,1:3])
                   }
                 }
                 else {
                   saved_BigDataMyChoice_location = find('saved_BigDataMyChoice')
                   if(printBDCProgress)
                     cat(': Restoring saved_BigDataMyChoice ',
                       #paste(saved_BigDataMyChoice),
                       'from ',
                       saved_BigDataMyChoice_location)
                   setBigData(saved_BigDataMyChoice, analysisName, myChoice=TRUE)
                   rm('saved_BigDataMyChoice', pos='.GlobalEnv')
                   if(printBDCProgress)
                     cat('Back to Original; but saved_BigDataMyChoice is saved.\n')
                   #if(exists('saved_BigDataMyChoice')) browser()
                 }
                 BigDataLastUsed <<- getBigData(analysisName, myChoice=TRUE)
                 if(printBDCProgress) {
                   cat('\nEND: handler for ', myName, ' BigDataLastUsed is now: \n')
                 print(BigDataLastUsed[1:2, 1:4])
                 }
               }) ### End of isolate()
             }) ### End of try()
           }
         )
  )
}   ### End of createBigDataChoiceObserver()

#### bigDataComponent ####
bigDataComponent = function(analysisName) {
  createBigDataParamChoiceObserver(analysisName)
  analysisNumber = which(analysisName == names(jumpList_BDC))
  thisBDCNumber = nextNumber(sequenceType = "BDC")
  if(printBDCProgress ) cat('Creating bigDataComponent thisBDCNumber = ', thisBDCNumber, '\n')
  if(printBDCProgress ) cat('BDCNumber=', thisBDCNumber, '   analysisNumber=', analysisNumber, '\n')
  outputIdThisBDC = paste0('outputBDC', thisBDCNumber)
  panelIdThisBDC = paste0('idPanelBDC', thisBDCNumber)
  resetIdThisBDC = paste0('idResetBDC', thisBDCNumber)
  myChoiceIdThisBDC = paste0('idMyChoiceBDC', thisBDCNumber)

    #  theBigDataController = paste0('BigDataController_ID_', analysisNumber)
  thisOmegaID = get_thisOmegaID(analysisNumber)

  #### resetIdThisBDC button ####
  'When resetIdThisBDC Button  is clicked, update OmegaId input to zero and copy to OmegaLastUsed.'
  myName = paste0('observeEvent_resetIdThisBDC_', thisBDCNumber)
  assign(myName,
         pos=1,
         observeEvent(label = myName,
                      eventExpr = input[[resetIdThisBDC]],
                      handlerExpr =  {
                        isolate({
                          if(printBDCProgress)
                            cat('Handler:  Pressed ', resetIdThisBDC, '\n')
                          currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
                          saved_BigDataMyChoice <<- getBigData(analysisName, myChoice=TRUE)
                          if(printBDCProgress)
                            cat('saved_BigDataMyChoice:', paste(saved_BigDataMyChoice), '\n')
                          updateNumericInput(
                            session,
                            thisOmegaID,
                            value = 0)
                          setBigData(BigDataOriginal, analysisName)
                          BigDataLastUsed <<- BigDataOriginal
                          setBigData(saved_BigDataMyChoice, analysisName, myChoice=TRUE)
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
             handlerForobserveEvent_myChoiceIdThisBDC(
               myChoiceIdThisBDC, analysisName, thisOmegaID)
           }
         )
  )

  ### When features are regenerated, re-do Ps, BH and Qvalues for both original and modified BigData.
  observeEvent(list(input$regenerateFeatures, input[[thisOmegaID]]), {
    try( {
      Omega = makeSureOmegaIsGood(input[[thisOmegaID]])
      if(printBDCProgress)
        cat('BDC:  Omega: ', Omega, '  analysisName=', analysisName, '\n')
      OmegaLastUsed <<- Omega
      isolate({
        setBigData(
          makeBigDataWithFeatures(DLdata = DLdataOriginal,
                                  Omega = Omega),
          analysisName, myChoice=FALSE)
      })
      #generateAllPvalues(rValues$BigDataDFwithFeatures)
      #not necessary; observer in place. observer_Pvalues_all_features
    })
  })
  #### Output of bigDataComponent ####
  if(printBDCProgress )
      cat('bigDataComponent for ', analysisName, ' ', thisBDCNumber, '\n')
  output[[outputIdThisBDC]] = renderUI({
    fluidRow(
      column(6,
             numericInput(thisOmegaID, 'Omega: the variance of the Log Odds Ratio',
                          value = 0, min=0,  step=0.1),

             ##```{r regenerateFeatures button}
             actionButton(inputId = 'regenerateFeatures', label = 'regenerate Features')
             ),
      column(6, br(), br(),
             #disabled(  #Start disabled. Doesn't seem to work.
             actionButton(inputId = resetIdThisBDC,
                          label = "Reset Omega and multi-feature data to original") ,
             actionButton(inputId = myChoiceIdThisBDC,
                          label = "Reset Omega and multi-feature data to my choice"),
             br(),
             jumpBackWithPanel_BDC(analysisNumber, thisBDCNumber)

    ) )
  })
  uiOutput(outputId = outputIdThisBDC)
}

