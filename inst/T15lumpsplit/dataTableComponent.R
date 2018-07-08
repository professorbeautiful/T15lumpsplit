'
When numericInput[n] changes,
     update numericInput[1]
When numericInput[1] changes,
      update numericInput[n].
When numericInput[1] changes,
    if [ORANGE path] not blocked,
      rValues$DLdataMyChoice is changed,
    else
      unblock ORANGE path.
When   rValues$DLdataMyChoice is changed
or  Button MyChoice is clicked,
      copy rValues$DLdataMyChoice to rValues$DLdata.
When   rValues$DLdata is changed
    block the ORANGE path,
    THEN  update numericInputs for firstCellIds.
When  Button reset is clicked,
    copy DLdataOriginal to rValues$DLdata.
'

#### firstCellIds ####
cellNames = c('RD', 'ND', 'RL', 'NL')
firstCellIds = paste0('m', cellNames, 'idPanelDTC', '1')
#names(firstCellIds) = cellNames

'When numericInput[1] changes,
  if [ORANGE path] not blocked, rValues$DLdataMyChoice is changed,
  else unblock ORANGE path.'
### Respond to firstCellIds, Change DLdataMyChoice ####
myName = ('updateDLdataMyChoice')
assign(myName,
       pos=1,
       observeEvent(
         #priority = 2,
         label=myName,
         eventExpr = #c(input$mRD, input$mRL, input$mND, input$mNL)
           c(input[[firstCellIds[1]]], # RD. Must by 1,2,3,4
             input[[firstCellIds[2]]], #ND
             input[[firstCellIds[3]]], #RL
             input[[firstCellIds[4]]]), #NL
         handlerExpr = {
           cat('START updateDLdataMyChoice: isLoopingSync=',
               rValues$isLoopingSync, ' isResetting=', rValues$isResetting, "\n")
           if(rValues$isLoopingSync == FALSE
              & rValues$isResetting == FALSE) {
             try(silent = FALSE, {
               isolate({
                 cat('updateDLdataMyChoice: changing MyChoice', '\n')
                 rValues$DLdataMyChoice[1,1] =  as.numeric(input[[firstCellIds[1]]])
                 rValues$DLdataMyChoice[2,1] =  as.numeric(input[[firstCellIds[2]]])
                 rValues$DLdataMyChoice[1,2] =  as.numeric(input[[firstCellIds[3]]])
                 rValues$DLdataMyChoice[2,2] =  as.numeric(input[[firstCellIds[4]]])
                 rValues$isResetting = FALSE
               })
             }) ### end of try
           } ### end of if
           cat('END updateDLdataMyChoice: isLoopingSync=',
               rValues$isLoopingSync, ' isResetting=', rValues$isResetting, "\n")
         }
       )
)

'When   rValues$DLdataMyChoice is changed
or  Button MyChoice is clicked,
copy rValues$DLdataMyChoice to rValues$DLdata.'
#### copy rValues$DLdataMyChoice to rValues$DLdata. ####
myName = ('copyDLdataMyChoice2DLdata')
assign(myName,
       pos=1,
       observeEvent(
         #priority = 2,
         label=myName,
         eventExpr =
           rValues$DLdataMyChoice,
         handlerExpr = {
           cat('copyDLdataMyChoice2DLdata', "\n")
             try(silent = FALSE, {
               isolate({
                 rValues$DLdata = rValues$DLdataMyChoice
               })
             }) ### End of try()
         }
       )
)

#### updateDLnumericInputs from DLdata ####
'When   rValues$DLdata is changed
    block the ORANGE path,
    THEN  update numericInputs for firstCellIds.'
myName = ('updateDLnumericInputs')
assign(myName,
       pos=1,
       observeEvent(
         #priority = 2,
         label=myName,
         eventExpr = rValues$DLdata,
         handlerExpr = {
           cat('updateDLnumericInputs: setting isResetting to TRUE', "\n")
           rValues$isResetting = TRUE
           try(silent = FALSE, {
             isolate({  ### isolates should not be necessary in handlerExpr.
               firstCellIds = as.vector(firstCellIds)
               updateNumericInput(session, firstCellIds[1], value=rValues$DLdata['R','D'])
               updateNumericInput(session, firstCellIds[2], value=rValues$DLdata['N','D'])
               updateNumericInput(session, firstCellIds[3], value=rValues$DLdata['R','L'])
               updateNumericInput(session, firstCellIds[4], value=rValues$DLdata['N','L'])
             })
           }) ### End of try()
           cat('updateDLnumericInputs:isResetting is now ', rValues$isResetting, "\n")
         }
       )
)

#### dataTableComponent ####
dataTableComponent = function() {
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  #cat('Creating dataTableComponent thisDTCNumber = ', thisDTCNumber, '\n')
  outputIdThisDTC = paste0('outputDTC', thisDTCNumber)
  panelIdThisDTC = paste0('idPanelDTC', thisDTCNumber)
  resetIdThisDTC = paste0('idResetDTC', thisDTCNumber)
  syncIdThisDTC = paste0('syncIdDTC', thisDTCNumber)
  myChoiceIdThisDTC = paste0('idMyChoiceDTC', thisDTCNumber)

  theCellIds = paste0('m', cellNames, 'idPanelDTC', thisDTCNumber)
  names(theCellIds) = cellNames

  #### resetIdThisDTC ####
  'When  Button reset is clicked,
    copy DLdataOriginal to rValues$DLdata.'
  myName = paste0('observeEvent_resetIdThisDTC_', thisDTCNumber)
  assign(myName,
    pos=1,
    observeEvent(label = myName,
                 eventExpr = input[[resetIdThisDTC]],
                 handlerExpr =  {
                   #updateDLdataMyChoice$suspend()
                   cat(myName, '\n')
                   isolate({
                     rValues$isResetting <<- TRUE
                     cat(myName, ':   rValues$isResetting = TRUE\n')
                     rValues$DLdata = DLdataOriginal
                     # rValues$isResetting <<- FALSE #Not necessary?
                   })
                   #updateDLdataMyChoice$resume()
                 })
  )

  #### myChoiceIdThisDTC --  restore MyChoice data ####
  myName =
    paste0('observeEvent_myChoiceIdThisDTC_', thisDTCNumber)
  assign(myName, pos=1,
    observeEvent(
      label = myName,
      eventExpr = input[[myChoiceIdThisDTC]],
      #priority = 1,
      handlerExpr =  {
        cat(myName, '\n')
        isolate({
          cat(myName, '  isResetting=', rValues$isResetting, '\n')
          rValues$DLdata = rValues$DLdataMyChoice
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
          initialValue = TRUE,
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
                                     '#RD', DLdataOriginal['R', 'D']),
                    min=0),
              column(4, numericInput(paste0('mRL', panelIdThisDTC),
                                     '#RL', DLdataOriginal['R', 'L']),
                     min=0)
            ),
            fluidRow(
              column(4, dataRowLabel( "<b>N</b>onResponders")),
              column(4, numericInput(paste0('mND', panelIdThisDTC),
                                     '#ND', DLdataOriginal['N', 'D']),
                     min=0),
              column(4, numericInput(paste0('mNL', panelIdThisDTC),
                                     '#NL', DLdataOriginal['N', 'L']),
                     min=0)
            ),
            br(),
            uiOutput(outputId = panelIdThisDTC)
          )
      )
  )
}

