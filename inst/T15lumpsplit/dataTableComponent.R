source('jumpBackWithPanel.R', local=TRUE)

printDTCProgress = FALSE

cellNames = c('RD', 'ND', 'RL', 'NL')

# disable = function(id) {
#   addClass(id, class='disabled')
#   removeClass(id, class='enabled')
#   shinyjs::disable(id)
# }
# enable = function(id) {
#   addClass(id, class='enabled')
#   removeClass(id, class='disabled')
#   shinyjs::enable(id)
# }

#### If the user changes a number,
#### then update rValues$DLdataMyChoice and rValues$DLdataLastUsed
createDLdataChoiceObserver <- function(analysisName) {
  myName = paste0('updateDLdataMyChoice_', analysisName)
  if(printDTCProgress ) cat('Creating ', myName, '\n')
  analysisNumber = match(analysisName, names(jumpList))
  theCellIds = paste0('m', cellNames, 'idPanelDTC', analysisNumber)
  assign(myName,
         pos=1,
         observeEvent(
           label=myName,
           eventExpr = #c(input$mRD, input$mRL, input$mND, input$mNL)
             c(input[[theCellIds[[1]] ]], # RD. Must by 1,2,3,4
               input[[theCellIds[[2]] ]], #ND
               input[[theCellIds[[3]] ]], #RL
               input[[theCellIds[[4]] ]]), #NL
           handlerExpr = {
             cat('START: handler for ', myName, ' new inputs: ',
                 paste(input[[theCellIds[[1]]]],input[[theCellIds[[2]]]],
                       input[[theCellIds[[3]]]],input[[theCellIds[[4]]]]), '\n')
             try(silent = FALSE, {
               isolate({
                 cat('updateDLdataMyChoice: changing MyChoice')
                 currentDTCnumber = mapAnalysisToDTCnumber[analysisName]
                 if( ! exists('saved_DLdataMyChoice')) {
                   cat(': Responding to numericInputs: ')
                   if( is.null(getDLdata(analysisName, myChoice=TRUE)))
                     DLdataMyChoice = DLdataOriginal
                   else
                     DLdataMyChoice = getDLdata(analysisName, myChoice=TRUE)
                   DLdataMyChoice[1,1] =  as.numeric(input[[theCellIds[[1]] ]])
                   DLdataMyChoice[2,1] =  as.numeric(input[[theCellIds[[2]] ]])
                   DLdataMyChoice[1,2] =  as.numeric(input[[theCellIds[[3]] ]])
                   DLdataMyChoice[2,2] =  as.numeric(input[[theCellIds[[4]] ]])
                   setDLdata(DLdataMyChoice, analysisName)
                   setDLdata(DLdataMyChoice, analysisName, myChoice=TRUE)
                   cat(paste(DLdataMyChoice), ' ', analysisName)
                 }
                 else {
                   saved_DLdataMyChoice_location = find('saved_DLdataMyChoice')
                   cat(': Restoring saved_DLdataMyChoice ',
                       paste(saved_DLdataMyChoice),
                       'from ',
                       saved_DLdataMyChoice_location)
                   setDLdata(saved_DLdataMyChoice, analysisName, myChoice=TRUE)
                   rm('saved_DLdataMyChoice', pos='.GlobalEnv')
                   cat('Back to Original; but saved_DLdataMyChoice is saved.\n')
                   if(exists('saved_DLdataMyChoice')) browser()
                 }
                 DLdataLastUsed <<- getDLdata(analysisName, myChoice=TRUE)
                 cat('\nEND: handler for ', myName, ' DLdataLastUsed is now: ',
                     paste(DLdataLastUsed), '\n')
               }) ### End of isolate()
             }) ### End of try()
           }
         )
  )
}   ### End of createDLdataChoiceObserver()

#### dataTableComponent ####
dataTableComponent = function(showhide='show', analysisName) {
  createDLdataChoiceObserver(analysisName)
  analysisNumber = which(analysisName == names(jumpList))
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  if(printDTCProgress ) cat('Creating dataTableComponent thisDTCNumber = ', thisDTCNumber, '\n')
  if(printDTCProgress ) cat('DTCNumber=', thisDTCNumber, '   analysisNumber=', analysisNumber, '\n')
  outputIdThisDTC = paste0('outputDTC', thisDTCNumber)
  panelIdThisDTC = paste0('idPanelDTC', thisDTCNumber)
  resetIdThisDTC = paste0('idResetDTC', thisDTCNumber)
  myChoiceIdThisDTC = paste0('idMyChoiceDTC', thisDTCNumber)

  theCellIds = as.list(paste0('m', cellNames, 'idPanelDTC',
                      getDTCnumber(analysisNumber)) )
  names(theCellIds) = cellNames

  #### resetIdThisDTC button ####
  'When resetIdThisDTC Button  is clicked,
    update numericInputs and copy DLdataOriginal to DLdataLastUsed.'
  myName = paste0('observeEvent_resetIdThisDTC_', thisDTCNumber)
  assign(myName,
         pos=1,
         observeEvent(label = myName,
                      eventExpr = input[[resetIdThisDTC]],
                      handlerExpr =  {
                        isolate({
                          cat('Handler:  Pressed ', resetIdThisDTC, '\n')
                          #disable(resetIdThisDTC)
                          #enable(myChoiceIdThisDTC)
                          ### PREVENT placing DLdataOriginal into
                          #eval(paste0('updateDLdataMyChoice_', analysisName)
                          currentDTCnumber = mapAnalysisToDTCnumber[analysisName]

                          saved_DLdataMyChoice <<- getDLdata(analysisName, myChoice=TRUE)
                          cat('saved_DLdataMyChoice:', paste(saved_DLdataMyChoice), '\n')
                          # updater = get(paste0('updateDLdataMyChoice_', analysisName))
                          # counter= 0
                          # while(updater$.suspended == FALSE)
                          #   {
                          #   updater$suspend()
                          #   counter = counter + 1
                          #   cat('  counter for suspending =', counter,'\n')
                          # }
                          for(cellnum in 1:4)
                            updateNumericInput(
                              session,
                              theCellIds[[cellnum]],
                              value = DLdataOriginal[cellnum])
                          setDLdata(DLdataOriginal, analysisName)
                          DLdataLastUsed <<- DLdataOriginal
                          setDLdata(saved_DLdataMyChoice, analysisName, myChoice=TRUE)
                        })  ### end of the isolate() call
                        cat('END: handler for ', resetIdThisDTC, ' DLdataLastUsed is now: ',
                            paste(DLdataLastUsed), '\n')
                      })
  )

  #### myChoiceIdThisDTC button -- update numericInputs restoring MyChoice data ####
  myName = paste0('observeEvent_myChoiceIdThisDTC_', thisDTCNumber)
  assign(myName, pos=1,
         observeEvent(
           label = myName,
           eventExpr = input[[myChoiceIdThisDTC]],
           handlerExpr =  {
             isolate({
               cat('Handler: Pressed ', myChoiceIdThisDTC, '\n')
               #enable(resetIdThisDTC)
               #disable(myChoiceIdThisDTC)
               currentDTCnumber = mapAnalysisToDTCnumber[analysisName]
               DLdataMyChoice = getDLdata(analysisName, myChoice=TRUE)
               cat('   Copying DLdataMyChoice into numericinputs: ',
                   paste(DLdataMyChoice), '\n')
               for(cellnum in 1:4)
                 updateNumericInput(
                   session,
                   theCellIds[[cellnum]],
                   value = DLdataMyChoice [cellnum]
                 )
               setDLdata(DLdataMyChoice, analysisName)
               DLdataLastUsed <<- DLdataMyChoice
               cat('   So now DLdataLastUsed is the same: ',
                   paste(DLdataLastUsed), '\n')
             })
           })
  )
  #### Output of dataTableComponent ####
  if(printDTCProgress )
      cat('dataTableComponent for ', analysisName, ' ', thisDTCNumber, '\n')
  output[[outputIdThisDTC]] = renderUI({
    fluidRow(
      column(6,
             br(),
             panelOfData(panelIdThisDTC=panelIdThisDTC,
                         resetIdThisDTC=resetIdThisDTC,
                         myChoiceIdThisDTC=myChoiceIdThisDTC,
                         showhide=showhide)
      ),
      column(6, br(), br(),
             #disabled(  #Start disabled. Doesn't seem to work.
             actionButton(inputId = resetIdThisDTC,
                            label = "Reset data to original") ,
             actionButton(inputId = myChoiceIdThisDTC,
                            label = "Reset data to my choice"),
             br(),
             jumpBackWithPanel(analysisNumber, thisDTCNumber)
      )
    )
  })
  uiOutput(outputId = outputIdThisDTC)
}
includeCSS('rotate-text.css')
dataRowLabel = function(html, angle=360-40, color='green') {
  ### rotating works wiht div and p but not with span
  HTML(paste("<div style='
               color:", color, ";",
             "height:80px; width:120px;
               vertical-align:bottom; horizontal-align:right;",
             paste0(collapse=" ",
                    "-", c("webkit","ms","moz","o"),
                    "-transform:rotate(", angle, "deg);"),
             "'>",
             html, "</div>"
  ))
}
### Do not call panelOfData() directly. Use dataTableComponent().

panelOfData = function(panelIdThisDTC, resetIdThisDTC, myChoiceIdThisDTC,
                       showhide='show') {
  span(
    conditionalPanelWithCheckbox(
      initialValue = (showhide=='show'),
      labelString = HTML(paste("Response by Predictor ____(",
                          gsub("idPanelDTC","",panelIdThisDTC),
                          ")____")),
      ##   conditionalPanelWithCheckbox needs to extract the unique ID.
      ## But we added the removal of __ and beyond in conditionalPanelWithCheckbox, so OK.
      ##  It will strip __.* to look nice.
      html = div(
        splitLayout(style='color:green;', "",
                    HTML("Group '<strong>D</strong>'"),
                    HTML("Group '<strong>L</strong>'"),
                    cellWidths = c("34%",'36%','30%')),
        splitLayout(cellWidths = c("30%",'35%','35%'),
                    tagAppendAttributes(
                      style="color:green;",
                      div(HTML("<br>Outcome<br><b>'R'</b><br>"))) ,
                    numericInput(paste0('mRD', panelIdThisDTC),
                                 '#RD', DLdataOriginal['R', 'D'],
                                 min=0) ,
                    numericInput(paste0('mRL', panelIdThisDTC),
                                 '#RL', DLdataOriginal['R', 'L'],
                                 min=0)
        ),
        splitLayout(cellWidths = c("30%",'35%','35%'),
                    tagAppendAttributes(
                      style="color:green;",
                      div(HTML("<br>Outcome<br><b>'N'</b><br>"))) ,
                    numericInput(paste0('mND', panelIdThisDTC),
                                 '#ND', DLdataOriginal['N', 'D'],
                                 min=0) ,
                    numericInput(paste0('mNL', panelIdThisDTC),
                                 '#NL', DLdataOriginal['N', 'L'],
                                 min=0)
        ),
        br(),
        uiOutput(outputId = panelIdThisDTC)
      )
    )
  )
}

