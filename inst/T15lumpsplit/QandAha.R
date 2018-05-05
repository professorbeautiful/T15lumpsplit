QandAha = function(buttonLabel= "save") {
  thisQANumber = nextNumber(sequenceType = "QA")
  outputIdThisQA = paste0('QA', thisQANumber)
  textareaIdThisQA = paste0('id', outputIdThisQA)

  output[[outputIdThisQA]] = renderUI({
    textAreaInput(inputId = textareaIdThisQA, label = NULL,
                  value="Ask question here.")
  })

  span(paste("(", thisQANumber, ") "),
       # paste("thisQANumber", thisQANumber),
       # paste("outputIdThisQA", outputIdThisQA),
       # paste("buttonIdThisQA", buttonIdThisQA),
       uiOutput(outputId = outputIdThisQA)
  )
}
