TextQuestion = function(question="What do you think?") {
  thisTQNumber = nextNumber(sequenceType = "TQ")
  outputIdThisTQ = paste0('TQ', thisTQNumber)
  textareaIdThisTQ = paste0('id', outputIdThisTQ)
  output[[outputIdThisTQ]] = renderUI({
    div(em(question),
        textAreaInput(inputId = textareaIdThisTQ,
                      label = paste("(", thisTQNumber, ")",
                                    "Your answer:")
        )
    )
  })
  uiOutput(outputId = outputIdThisTQ)
}
