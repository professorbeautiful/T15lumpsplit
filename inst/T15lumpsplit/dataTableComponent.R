'
When a user changes a data number:
  rValues$isResetting=TRUE
  rValues$DLdata is changed
  rValues$DLdataMyChoice is changed
  rValues$isLoopingSync == TRUE
  All corresponding numericInput cells are changed.
  rValues$isLoopingSync == FALSE
  rValues$isResetting=FALSE
When a user clicks "Reset":
  rValues$isResetting=TRUE
  DLdataOriginal is copied to rValues$DLdata
    (plots all respond)
    (DLdataMyChoice is NOT changed)
  All corresponding numericInput cells are changed; no downstream.
  rValues$isResetting=FALSE
When a user clicks "MyChoice":
  rValues$isResetting=TRUE
  rValues$DLdataMyChoice  is copied to rValues$DLdata
    (plots all respond)
    (DLdataMyChoice is NOT changed)
  All corresponding numericInput cells are changed; no downstream .
  rValues$isResetting=FALSE

'
cellNames = c('RD', 'RL', 'ND', 'NL')
firstCellIds = paste0('m', cellNames, 'idPanelDTC', '1')
names(firstCellIds) = cellNames

updateDLnumericInputs = function(data, isResetting=FALSE) {
  DLdataMyChoice = rValues$DLdataMyChoice
  cat('updateDLnumericInputs:' , firstCellIds, '\n')
  cat('updateDLnumericInputs:' , firstCellIds, '\n')
  cat('updateDLnumericInputs: data = ', data, '\n')
  firstCellIds = as.vector(firstCellIds)
  updateNumericInput(session, firstCellIds[1], value=data['R','D'])
  updateNumericInput(session, firstCellIds[2], value=data['N','D'])
  updateNumericInput(session, firstCellIds[3], value=data['R','L'])
  updateNumericInput(session, firstCellIds[4], value=data['N','L'])
  if( ! isResetting)
    rValues$DLdataMyChoice = DLdataMyChoice
}

#### dataTableComponent ####
dataTableComponent = function() {
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  cat('Creating dataTableComponent thisDTCNumber = ', thisDTCNumber, '\n')
  outputIdThisDTC = paste0('outputDTC', thisDTCNumber)
  panelIdThisDTC = paste0('idPanelDTC', thisDTCNumber)
  resetIdThisDTC = paste0('idResetDTC', thisDTCNumber)
  syncIdThisDTC = paste0('syncIdDTC', thisDTCNumber)
  myChoiceIdThisDTC = paste0('idMyChoiceDTC', thisDTCNumber)

  #### resetIdThisDTC ####
  myName = paste0('observeEvent_resetIdThisDTC_', resetIdThisDTC)
  assign(myName,
    pos=1,
    observeEvent(label =
                   paste0('observeEvent resetIdThisDTC #', resetIdThisDTC),
                 eventExpr = input[[resetIdThisDTC]],
                 handlerExpr =  {
                   #updateDLdataMyChoice$suspend()
                   cat(myName, '\n')
                   isolate({
                     rValues$isResetting <<- TRUE
                     cat(myName, ':   rValues$isResetting = TRUE\n')
                     updateDLnumericInputs(data = DLdataOriginal, isResetting=TRUE)
                   })
                   #updateDLdataMyChoice$resume()
                 })
  )

  #### myChoiceIdThisDTC --  restore MyChoice data ####
  myName =
    paste0('observeEvent_myChoiceIdThisDTC_', resetIdThisDTC)
  assign(myName, pos=1,
    observeEvent(
      label = myName,
      eventExpr = input[[myChoiceIdThisDTC]],
      #priority = 1,
      handlerExpr =  {
        cat(myName, '\n')
        isolate({
          cat(myName, '  isResetting=', rValues$isResetting, '\n')
          updateDLnumericInputs(data = rValues$DLdataMyChoice, isResetting=FALSE)
        })
      })
  )

  #### synchronizeOtherDataTables ####
  #  If a cell in this table changes, sync all the corresponding cells in other tables.
  createSyncActor = function(cell, syncIdThisDTC=syncIdThisDTC){
    thisDTCNumber = getSequenceLength(sequenceType = "DTC")
    thisCellId = paste0(cell, panelIdThisDTC)
    myName = paste0(
      'observeEvent_synchronizeOtherDataTables_ThisDTC_', thisDTCNumber)
    assign(myName,
      pos=1,
      observeEvent(label = myName,
                   eventExpr = input[[thisCellId]],
                   #priority = 3,
                   handlerExpr =  {
                     ### NOTE; there could be more DTC's, so don't use thisDTCNum for loop.
                     #cat('Creating ', label, '\n')
                     rValues$isLoopingSync = TRUE
                     for( anyDTCnum in 1:(getSequenceLength(sequenceType = "DTC"))) {
                       if(anyDTCnum != thisDTCNumber) {
                         # suspenderExpression = parse(text=
                         #                               paste0())
                         otherCellId = paste0(cell, 'idPanelDTC', anyDTCnum)
                         rValues$isResetting = TRUE
                         updateNumericInput(session, inputId = otherCellId,
                                            value=input[[thisCellId]])
                         rValues$isResetting = FALSE
                         #updateDLnumericInputs(data = rValues$DLdataMyChoice, isResetting=TRUE)
                       }
                     }
                     rValues$isLoopingSync = FALSE
                   })
    )
  }
  for(cell in paste0('m', c('RD', 'ND', 'RL', 'NL')))
    createSyncActor(cell, syncIdThisDTC=syncIdThisDTC)

  #### updateDLdata -- ####
  cellNames = c('RD', 'RL', 'ND', 'NL')
  theCellIds = paste0('m', cellNames, 'idPanelDTC', thisDTCNumber)
  names(theCellIds) = cellNames
  myName = paste0('updateDLdata_ThisDTC_', thisDTCNumber)
  assign(myName,
    pos=1,
    observeEvent(
      #priority = 2,
      label=myName,
      eventExpr = #c(input$mRD, input$mRL, input$mND, input$mNL)
        c(input[[theCellIds[1]]], # RD. Must by 1,2,3,4
          input[[theCellIds[2]]], #RL
          input[[theCellIds[3]]], #ND
          input[[theCellIds[4]]]), #NL
      handlerExpr = {
        cat(myName, "\n")
        if(rValues$isLoopingSync == FALSE)
          try(silent = FALSE, {
            isolate({
              cat('      input$mRD=', input[[theCellIds[1]]], '\n')
              # rValues$DLdata[1,1] =  input[[firstCellIds['RD']]]
              # rValues$DLdata[2,1] =  input[[firstCellIds['RL']]]
              # rValues$DLdata[1,2] =  input[[firstCellIds['ND']]]
              # rValues$DLdata[2,2] =  input[[firstCellIds['NL']]]
              rValues$DLdata[1,1] =  input[[theCellIds[1]]]
              rValues$DLdata[2,1] =  input[[theCellIds[2]]]
              rValues$DLdata[1,2] =  input[[theCellIds[3]]]
              rValues$DLdata[2,2] =  input[[theCellIds[4]]]
              if(rValues$isResetting ==  FALSE) {
                print('changing DLdataMyChoice also')
                for(feature in 1:2) for(outcome in 1:2)
                  rValues$DLdataMyChoice[outcome, feature] =
                    rValues$DLdata[outcome, feature]
              }
            })
          }) ### End of try()
      })
  )
  #### Output of dataTableComponent ####
  output[[outputIdThisDTC]] = renderUI({

    panelOfData(panelIdThisDTC=panelIdThisDTC,
                resetIdThisDTC=resetIdThisDTC,
                myChoiceIdThisDTC=myChoiceIdThisDTC)
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
panelOfData = function(panelIdThisDTC, resetIdThisDTC, myChoiceIdThisDTC) {
      span(
        actionButton(inputId = resetIdThisDTC, label = "Reset data to original"),
        actionButton(inputId = myChoiceIdThisDTC,
                     label = "Reset data to my choice"),
        conditionalPanelWithCheckbox(
          labelString = paste("Response by Predictor Table ", panelIdThisDTC),
          html = div(
            # checkboxInput('toggleShowData', 'Show/Hide the Data Panel', FALSE),
            # conditionalPanel(
            #   'input.toggleShowData',
            splitLayout(style='color:green;', "",
                        HTML("Group '<strong>D</strong>'"),
                        HTML("Group '<strong>L</strong>'"),
                        cellWidths = c("40%",'30%','30%')),
            fluidRow(
              column(4, dataRowLabel( "<b>R</b>esponders")),
              column(4, numericInput(paste0('mRD', panelIdThisDTC),
                                     '#RD', DLdataOriginal['R', 'D'])),
              column(4, numericInput(paste0('mRL', panelIdThisDTC),
                                     '#RL', DLdataOriginal['R', 'L']))
            ),
            fluidRow(
              column(4, dataRowLabel( "<b>N</b>onResponders")),
              column(4, numericInput(paste0('mND', panelIdThisDTC),
                                     '#ND', DLdataOriginal['N', 'D'])),
              column(4, numericInput(paste0('mNL', panelIdThisDTC),
                                     '#NL', DLdataOriginal['N', 'L']))
            ),
            br(),
            uiOutput(outputId = panelIdThisDTC)
          )
      )
  )
}

