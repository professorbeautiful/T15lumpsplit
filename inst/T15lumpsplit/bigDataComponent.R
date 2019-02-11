###   bigDataComponent


printBDCProgress =  TRUE

# Usage:  bigDataComponent(analysisName='qValue')


createBigDataParamChoiceObserver <- function(analysisName) {
  myName = paste0('bigDataParamChoiceObserver_', analysisName)
  analysisNumber = match(analysisName, names(jumpList_BDC))
  thisTauTrueID = get_thisTauTrueID(analysisNumber)
  cat(myName, ': thisTauTrueID=', thisTauTrueID, '\n')
  ####  Initially, only tauTrue.
  assign(myName,
         pos=1,
         observeEvent(
           label=myName,
           eventExpr = input[[thisTauTrueID]], ## tauTrueID
           handlerExpr = {
             cat('START: handler for ', myName, ' new input tau = ',
                 input[[thisTauTrueID]], '\n')
             try(silent = FALSE, {
               isolate({
                 cat('updateBigDataParamMyChoice: changing MyChoice')
                 currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
                 if( ! exists('saved_BigDataMyChoice')) {
                   cat(': Responding to numericInput for tauTrue: ',
                       as.numeric(input[[thisTauTrueID ]]))
                   if( is.null(getBigData(analysisName, myChoice=TRUE)))
                     BigDataMyChoice = BigDataOriginal
                   else
                     BigDataMyChoice = getBigData(analysisName, myChoice=TRUE)
                   newValue = as.numeric(input[[thisTauTrueID ]])
                   if( ! is.na(newValue) & (newValue >= 0 ) ) {
                     newValue = round(newValue)
                     BigDataMyChoice = makeBigDataWithFeatures(DLdataOriginal)
                   }
                   setBigData(BigDataMyChoice, analysisName)
                   setBigData(BigDataMyChoice, analysisName, myChoice=TRUE)
                   #cat(paste(BigDataMyChoice), ' ', analysisName)
                 }
                 else {
                   saved_BigDataMyChoice_location = find('saved_BigDataMyChoice')
                   cat(': Restoring saved_BigDataMyChoice ',
                       #paste(saved_BigDataMyChoice),
                       'from ',
                       saved_BigDataMyChoice_location)
                   setBigData(saved_BigDataMyChoice, analysisName, myChoice=TRUE)
                   rm('saved_BigDataMyChoice', pos='.GlobalEnv')
                   cat('Back to Original; but saved_BigDataMyChoice is saved.\n')
                   if(exists('saved_BigDataMyChoice')) browser()
                 }
                 BigDataLastUsed <<- getBigData(analysisName, myChoice=TRUE)
                 cat('\nEND: handler for ', myName, ' BigDataLastUsed is now: ')
                 paste(head(BigDataLastUsed,4))
                 #print(summary(BigDataDFwithFeatures_original))
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
  thisTauTrueID = get_thisTauTrueID(analysisNumber)

  #### resetIdThisBDC button ####
  'When resetIdThisBDC Button  is clicked, update tauTrueId input to zero and copy to tauTrueLastUsed.'
  myName = paste0('observeEvent_resetIdThisBDC_', thisBDCNumber)
  assign(myName,
         pos=1,
         observeEvent(label = myName,
                      eventExpr = input[[resetIdThisBDC]],
                      handlerExpr =  {
                        isolate({
                          cat('Handler:  Pressed ', resetIdThisBDC, '\n')
                          currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
                          saved_BigDataMyChoice <<- getBigData(analysisName, myChoice=TRUE)
                          cat('saved_BigDataMyChoice:', paste(saved_BigDataMyChoice), '\n')
                          updateNumericInput(
                            session,
                            thisTauTrueID,
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
             isolate({
               cat('Handler: Pressed ', myChoiceIdThisBDC, '\n')
               #enable(resetIdThisBDC)
               #disable(myChoiceIdThisBDC)
               currentBDCnumber = mapAnalysisToBDCnumber[analysisName]
               BigDataMyChoice = getBigData(analysisName, myChoice=TRUE)
               BigData_tauTrue_MyChoice = getBigData_tauTrue(analysisName, myChoice=TRUE)
               cat('   Copying BigData_tauTrue_MyChoice into numericinput: ',
                   paste(BigData_tauTrue_MyChoice), '\n')
               updateNumericInput(
                   session,
                   thisTauTrueID,
                   value = BigData_tauTrue_MyChoice
                 )
               setBigData(BigDataMyChoice, analysisName)
               BigDataLastUsed <<- BigDataMyChoice
               setBigData_tauTrue(BigData_tauTrue_MyChoice, analysisName)
               tauTrueLastUsed <<- BigDataBigData_tauTrue_MyChoiceMyChoice
               cat('   So now tauTrueLastUsed is the same: ',
                   paste(tauTrueLastUsed), '\n')
             })
           })
  )

  ### When features are regenerated, re-do Ps, BH and Qvalues for both original and modified BigData.
  observeEvent(list(input$regenerateFeatures, input[[thisTauTrueID]]), {
    try( {
      tauTrue = as.numeric(input[[get_thisTauTrueID(analysisNumber)]])
      cat('BDC:  tauTrue: ', tauTrue, '  analysisName=', analysisName, '\n')
      if(length(tauTrue)==0) tauTrue = 0
      tauTrueLastUsed <<- tauTrue
      isolate({
        rValues$BigDataDFwithFeatures =
          makeBigDataWithFeatures(DLdata = DLdataOriginal,
                                  tauTrue = tauTrue)
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
             numericInput(thisTauTrueID, 'Variance of the Odds Ratio ',
                          value = 0, min=0,  step=0.1),

             ##```{r regenerateFeatures button}
             actionButton(inputId = 'regenerateFeatures', label = 'regenerate Features')
             ),
      column(6, br(), br(),
             #disabled(  #Start disabled. Doesn't seem to work.
             actionButton(inputId = resetIdThisBDC,
                          label = "Reset tauTrue and multi-feature data to original") ,
             actionButton(inputId = myChoiceIdThisBDC,
                          label = "Reset tauTrue and multi-feature data to my choice"),
             br(),
             jumpBackWithPanel_BDC(analysisNumber, thisBDCNumber)

    ) )
  })
  uiOutput(outputId = outputIdThisBDC)
}

