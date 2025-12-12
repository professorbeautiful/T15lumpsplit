WhoPriorProbNumericInput = function() {
  thisWhoPriorProbNumber = nextNumber(sequenceType = "WhoPriorProb")
  outputIdThisWhoPriorProb = paste0('WhoPriorProb', thisWhoPriorProbNumber)
  numInputIdThisWhoPriorProb = paste0('id', outputIdThisWhoPriorProb)
  output[[outputIdThisWhoPriorProb]] = renderUI({
    numericInput(inputId = numInputIdThisWhoPriorProb,
                 label = HTML('<font color=green>Dr.Who\'s </font>prior probability that
                 <br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<font color=blue>
                  Dr.Split\'</font> prior is correct'),
                 value = 1/2, min = 0, max=1, step = 0.1)
  })
  observeEvent(input[[numInputIdThisWhoPriorProb]],
               {
                 rValues$WhoPriorProb = input[[numInputIdThisWhoPriorProb]]
                 rValues$WhoPriorOdds = rValues$WhoPriorProb/(1-rValues$WhoPriorProb)
               })
  uiOutput(outputId = outputIdThisWhoPriorProb)
}
