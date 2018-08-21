QandAha = function(buttonLabel= "save") {
  thisQANumber = nextNumber(sequenceType = "QA")
  outputIdThisQA = paste0('QA', thisQANumber)
  textareaIdThisQA = paste0('id', outputIdThisQA)
  textAreaInput(inputId = textareaIdThisQA, width='200%',
                   label =
                         span(style='display:block; width: 200%',
                              paste("(QA", thisQANumber, ")",
                               "Ask a question and/or describe an 'aha' here.")
    ) )
}
