TextQuestion = function(question="What do you think?") {
  thisTQNumber = nextNumber(sequenceType = "TQ")
  if(!exists('TQ_contexts'))
    TQ_contexts <<- list()
  TQ_contexts[[thisTQNumber]] <<- paste0('TQ-', question)

  outputIdThisTQ = paste0('TQ', thisTQNumber)
  textareaIdThisTQ = paste0('id', outputIdThisTQ)
  linkIdThisTQ = paste0('id', outputIdThisTQ, "_link")
  observeEvent(eventExpr = input[[linkIdThisTQ]], {
    cat('Calling saveEntriesJS (TQ)', date(), '\n')
    shinyjs::js$saveEntriesJS()
  })
  output[[outputIdThisTQ]] = renderUI({
    div(HTML(paste0(strong("A question for you: "), em(question))),
        splitLayout(cellWidths = c("75%", "25%"),
          #          style='display:block; width: 200%',
          textAreaInput(inputId = textareaIdThisTQ, width='200%',
                        label = paste("(TQ", thisTQNumber, ")",
                                      "Your answer:   ")
          )
          , actionLink(inputId = linkIdThisTQ,
                         label = '(click to save all responses)' )
          #(click or shift-cmd S saves all responses)')
        )
    )
  })
  uiOutput(outputId = outputIdThisTQ)
}
