source('jumpBackWithPanel.R', local=TRUE)

cellNames = c('RD', 'ND', 'RL', 'NL')

createDLdataChoiceObserver <- function(analysisName) {
  myName = paste0('updateDLdataMyChoice_', analysisName)
  analysisNumber = match(analysisName, names(jumpList))
  theCellIds = paste0('m', cellNames, 'idPanelDTC', analysisNumber)
  assign(myName,
         pos=1,
         observeEvent(
           label=myName,
           eventExpr = #c(input$mRD, input$mRL, input$mND, input$mNL)
             c(input[[theCellIds[1]]], # RD. Must by 1,2,3,4
               input[[theCellIds[2]]], #ND
               input[[theCellIds[3]]], #RL
               input[[theCellIds[4]]]), #NL
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
                   rValues$DLdataMyChoice[1,1] =  as.numeric(input[[theCellIds[1]]])
                   rValues$DLdataMyChoice[2,1] =  as.numeric(input[[theCellIds[2]]])
                   rValues$DLdataMyChoice[1,2] =  as.numeric(input[[theCellIds[3]]])
                   rValues$DLdataMyChoice[2,2] =  as.numeric(input[[theCellIds[4]]])
                   rValues$DLdata[[mapAnalysisToDTCnumber[analysisName] ]] = rValues$DLdataMyChoice
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
}

#### dataTableComponent ####
dataTableComponent = function(showhide='show', analysisName) {
  createDLdataChoiceObserver(analysisName)
  analysisNumber = which(analysisName == names(jumpList))
  thisDTCNumber = nextNumber(sequenceType = "DTC")
  cat('Creating dataTableComponent thisDTCNumber = ', thisDTCNumber, '\n')
  cat('DTCNumber=', thisDTCNumber, '   analysisNumber=', analysisNumber, '\n')
  outputIdThisDTC = paste0('outputDTC', thisDTCNumber)
  panelIdThisDTC = paste0('idPanelDTC', thisDTCNumber)
  resetIdThisDTC = paste0('idResetDTC', thisDTCNumber)
  syncIdThisDTC = paste0('syncIdDTC', thisDTCNumber)
  myChoiceIdThisDTC = paste0('idMyChoiceDTC', thisDTCNumber)

  theCellIds = paste0('m', cellNames, 'idPanelDTC',
                      getDTCnumber(analysisNumber))
  names(theCellIds) = cellNames

  #### resetIdThisDTC ####
  'When  Button reset is clicked,
    copy DLdataOriginal to rValues$DLdataLastUsed.'
  myName = paste0('observeEvent_resetIdThisDTC_', thisDTCNumber)
  assign(myName,
         pos=1,
         observeEvent(label = myName,
                      eventExpr = input[[resetIdThisDTC]],
                      handlerExpr =  {
                        isolate({
                          rValues$DLdata[[mapAnalysisToDTCnumber[analysisName] ]] =
                            rValues$DLdataLastUsed =
                            DLdataOriginal
                        })
                      })
  )

  #### myChoiceIdThisDTC --  restore MyChoice data ####
  myName =
    paste0('observeEvent_myChoiceIdThisDTC_', thisDTCNumber)
  assign(myName, pos=1,
         observeEvent(
           label = myName,
           eventExpr = input[[myChoiceIdThisDTC]],
           handlerExpr =  {
             isolate({
               rValues$DLdata[[thisDTCNumber]] =
                 rValues$DLdataLastUsed =
                 rValues$DLdataMyChoice[[thisDTCNumber]]
             })
           })
  )

  #### Output of dataTableComponent ####
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
             actionButton(inputId = resetIdThisDTC, label = "Reset data to original"),
             actionButton(inputId = myChoiceIdThisDTC,
                          label = "Reset data to my choice"),
             br(),
             jumpBackWithPanel(analysisNumber, thisDTCNumber)
             ##  can't find nextNumber() from jumpBackWithPanel()... needed local=T
             #inclRmd('jumpBackWithPanel.Rmd')
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

