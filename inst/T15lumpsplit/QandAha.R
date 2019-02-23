QandAha = function(context='QA', linkLabel= "save") {
  thisQANumber = nextNumber(sequenceType = "QA")
  if(!exists('QA_contexts'))
    QA_contexts <<- list()
  QA_contexts[[thisQANumber]] <<- context

  outputIdThisQA = paste0('QA', thisQANumber)
  textareaIdThisQA = paste0('id', outputIdThisQA)
  linkIdThisQA = paste0('id', outputIdThisQA, "_link")
  observeEvent(eventExpr = input[[linkIdThisQA]], {
    cat('Calling saveEntriesJS (QA)', date(), '\n')
    js$saveEntriesJS()
  })
  splitLayout(cellWidths = c("75%", "25%"),
              textAreaInput(inputId = textareaIdThisQA, width='200%',
                   label =
                         span(style='display:block; width: 200%',
                              paste("(QA", thisQANumber, ")",
                               "Ask a question and/or describe an 'aha' here.")
    ) )
    , actionLink(inputId = linkIdThisQA,
                 label = '(click to save all responses)' )
    #(click or shift-cmd S saves all responses)')
  )
}
