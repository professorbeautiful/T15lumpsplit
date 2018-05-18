
updateTableCells = function(data, isResetting=FALSE) {
  DLdataMyChoice = rValues$DLdataMyChoice
  updateNumericInput(session, 'mRD', value=data['D','R'])
  updateNumericInput(session, 'mND', value=data['D','N'])
  updateNumericInput(session, 'mRL', value=data['L','R'])
  updateNumericInput(session, 'mNL', value=data['L','N'])
  if( ! isResetting)
    rValues$DLdataMyChoice = DLdataMyChoice
}
dataTableComponent = function() {
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  cat('Creating dataTableComponent thisDTCNumber = ', thisDTCNumber, '\n')
  outputIdThisDTC = paste0('outputDTC', thisDTCNumber)
  panelIdThisDTC = paste0('idPanelDTC', thisDTCNumber)
  resetIdThisDTC = paste0('idResetDTC', thisDTCNumber)
  syncIdThisDTC = paste0('syncIdDTC', thisDTCNumber)

  myChoiceIdThisDTC = paste0('idMyChoiceDTC', thisDTCNumber)
  #### resetIdThisDTC ####
  observeEvent(label =
                 paste0('observeEvent resetIdThisDTC #', resetIdThisDTC),
    eventExpr = input[[resetIdThisDTC]],
    handlerExpr =  {
      #updateDLdataMyChoice$suspend()
      isolate({
        rValues$resetting = TRUE
        print('        rValues$resetting = TRUE')
        updateTableCells(data = DLdataOriginal, isResetting=TRUE)
      })
      #updateDLdataMyChoice$resume()
    })
  #### myChoiceIdThisDTC ####
  observeEvent(label = paste0('observeEvent myChoiceIdThisDTC #', myChoiceIdThisDTC),
    eventExpr = input[[myChoiceIdThisDTC]],
    priority = 1,
    handlerExpr =  {
      #cat('Creating ', label, '\n')
      isolate({
        updateTableCells(data = rValues$DLdataMyChoice, isResetting=FALSE)
      })
    })

  #### synchronizeOtherDataTables ####
  #  If a cell in this table changes, symc all the corresponding cells in other tables.
  createSyncActor = function(cell, syncIdThisDTC=syncIdThisDTC){
    thisDTCNumber = getSequenceLength(sequenceType = "DTC")
    thisCellId = paste0(cell, panelIdThisDTC)
    observeEvent(label = paste0('synchronizeOtherDataTables_',
                                thisDTCNumber),
               eventExpr = input[[thisCellId]],
               priority = 1,
               handlerExpr =  {
                 #rValues$isResetting = FALSE
                 ### NOTE; there could be more DTC's, so don't use thisDTCNum for loop.
                 #cat('Creating ', label, '\n')
                 for( anyDTCnum in 1:(getSequenceLength(sequenceType = "DTC"))) {
                   if(anyDTCnum != thisDTCNumber) {
                     otherCellId = paste0(cell, 'idPanelDTC', anyDTCnum)
                     updateNumericInput(session, inputId = otherCellId,
                                        value=input[[thisCellId]])
                     #updateTableCells(data = rValues$DLdataMyChoice, isResetting=TRUE)
                   }
                 }
               })
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
              column(4, numericInput(paste0('mRD', panelIdThisDTC), '#RD', DLdata[1,1])),
              column(4, numericInput(paste0('mRL', panelIdThisDTC), '#RL', DLdata[2,1]))
            ),
            fluidRow(
              column(4, dataRowLabel( "<b>N</b>onResponders")),
              column(4, numericInput(paste0('mND', panelIdThisDTC), '#ND', DLdata[1,2])),
              column(4, numericInput(paste0('mNL', panelIdThisDTC), '#NL', DLdata[2,2]))
            ),
            br(),
            uiOutput(outputId = panelIdThisDTC)
          )
      )
  )
}

