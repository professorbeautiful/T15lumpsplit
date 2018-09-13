QandAha = function(context='QA', buttonLabel= "save") {
  thisQANumber = nextNumber(sequenceType = "QA")
  if(!exists('QA_contexts'))
    QA_contexts <<- list()
  QA_contexts[[thisQANumber]] <<- context

  outputIdThisQA = paste0('QA', thisQANumber)
  textareaIdThisQA = paste0('id', outputIdThisQA)
  textAreaInput(inputId = textareaIdThisQA, width='200%',
                   label =
                         span(style='display:block; width: 200%',
                              paste("(QA", thisQANumber, ")",
                               "Ask a question and/or describe an 'aha' here.")
    ) )

}
