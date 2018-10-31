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
           if(trackupdateDLdata)
             cat('START updateDLdataMyChoice: isLoopingSync=',
                 rValues$isLoopingSync, ' isResetting=', rValues$isResetting, "\n")
           if(rValues$isLoopingSync == FALSE
              & rValues$isResetting == FALSE) {
             try(silent = FALSE, {
               isolate({
                 if(trackupdateDLdata)
                   cat('updateDLdataMyChoice: changing MyChoice', '\n')
                 rValues$DLdataMyChoice[1,1] =  as.numeric(input[[firstCellIds[1]]])
                 rValues$DLdataMyChoice[2,1] =  as.numeric(input[[firstCellIds[2]]])
                 rValues$DLdataMyChoice[1,2] =  as.numeric(input[[firstCellIds[3]]])
                 rValues$DLdataMyChoice[2,2] =  as.numeric(input[[firstCellIds[4]]])
                 rValues$isResetting = FALSE
               })
             }) ### end of try
           } ### end of if
           if(trackupdateDLdata)
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
           if(trackupdateDLdata)
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
           if(trackupdateDLdata)
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
           if(trackupdateDLdata)
             cat('updateDLnumericInputs:isResetting is now ', rValues$isResetting, "\n")
         }
       )
)

#### dataTableComponent ####
dataTableComponent = function(showhide='show') {
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
                        if(trackupdateDLdata)
                          cat(myName, '\n')
                        isolate({
                          rValues$isResetting <<- TRUE
                          if(trackupdateDLdata)
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
             #cat(myName, '\n')
             isolate({
               #cat(myName, '  isResetting=', rValues$isResetting, '\n')
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
                          if(trackupdateDLdata)
                            cat('.label', ': Suspending all ',
                              'observeEvent_synchronizeOtherDataTables_ThisDTC_', '\n')
                          rValues$isLoopingSync = TRUE
                          for( anyDTCnum in 1:(getSequenceLength(sequenceType = "DTC"))) {
                            #if(anyDTCnum != thisDTCNumber) {
                            syncName = paste0(
                              'observeEvent_synchronizeOtherDataTables_ThisDTC_',
                              anyDTCnum)
                            suspenderExpression =
                              parse(text=paste0(syncName, '$suspend()') )
                            # print(suspenderExpression)
                            # print(find(syncName))
                            # print(eval(suspenderExpression))
                            #Causes this observer to stop scheduling flushes
                            #(re-executions) in\n response to invalidations. If
                            #the observer was invalidated prior to this\n call
                            #but it has not re-executed yet (because it waits
                            #until onFlush is\n called) then that re-execution
                            #will still occur, because the flush is\n already
                            #scheduled." .suspended <<- TRUE
                          }
                          if(trackupdateDLdata)
                            cat(substitute(myName), ': Updating all ',
                              'observeEvent_synchronizeOtherDataTables_ThisDTC_', '\n')
                          rValues$isResetting = TRUE
                          if(trackupdateDLdata)
                            cat('trackupdateDLdata: thisCellId:', thisCellId,
                                ' thisDTCNumber:', thisDTCNumber,
                                ' value:', input[[thisCellId]], '\n')
                          for( anyDTCnum in 1:(getSequenceLength(sequenceType = "DTC"))) {
                            otherCellId = paste0(cell, 'idPanelDTC', anyDTCnum)
                            updateNumericInput(session, inputId = otherCellId,
                                               value=input[[thisCellId]])

                            #updateDLnumericInputs(data = rValues$DLdataMyChoice, isResetting=TRUE)
                          }
                          rValues$isResetting = FALSE

                          rValues$isLoopingSync = FALSE
                          #cat(label, ': Resuming all ',
                          #    'observeEvent_synchronizeOtherDataTables_ThisDTC_', '\n')

                          # for( anyDTCnum in 1:(getSequenceLength(sequenceType = "DTC"))) {
                          #   #if(anyDTCnum != thisDTCNumber) {
                          #   resumerExpression =
                          #     parse(text=paste0(
                          #       'observeEvent_synchronizeOtherDataTables_ThisDTC_',
                          #       anyDTCnum, '$resume()') )
                          #   print(resumerExpression)
                          #   eval(resumerExpression)
                          #   # }
                          # }
                        }
           )
    )
  }
  for(cell in paste0('m', c('RD', 'ND', 'RL', 'NL')))
    createSyncActor(cell, syncIdThisDTC=syncIdThisDTC)


  #### Output of dataTableComponent ####
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
             actionButton(inputId = resetIdThisDTC, label = "Reset data to original"),
             actionButton(inputId = myChoiceIdThisDTC,
                          label = "Reset data to my choice"),
             br(),
             #getwd(),
             inclRmd('jumpBack.Rmd'))
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
      labelString = paste("Response by Predictor Table ", panelIdThisDTC),
      #labelString = "Response by Predictor Table ",
      ##    This breaks the JS!  conditionalPanelWithCheckbox needs to extract the unique ID.
      html = div(
        # checkboxInput('toggleShowData', 'Show/Hide the Data Panel', FALSE),
        # conditionalPanel(
        #   'input.toggleShowData',
        splitLayout(style='color:green;', "",
                    HTML("Group '<strong>D</strong>'"),
                    HTML("Group '<strong>L</strong>'"),
                    cellWidths = c("40%",'30%','30%')),
        #fluidRow(
        splitLayout(cellWidths = c("30%",'35%','35%'),
                    #dataRowLabel( "<b>N</b><br>non-<br>responders")),
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
                    #dataRowLabel( "<b>R</b>esponders")),
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

