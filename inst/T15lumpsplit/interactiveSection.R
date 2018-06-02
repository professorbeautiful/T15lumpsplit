interactiveSection = function(codeToRun = "date()",
                              buttonLabel= "run code",
                              showCode = TRUE,
                              showResult = TRUE) {
  ### You need to add input$idRunCode1 to make it reactive!!
  thisSectionNumber = nextNumber()
  outputIdThisSection = paste0('RunCode', thisSectionNumber)
  buttonIdThisSection = paste0('id', outputIdThisSection)

  output[[outputIdThisSection]] = renderText({
    if(input[[buttonIdThisSection]] > 0)
       result = eval(parse(text=codeToRun))
    if(showResult == FALSE)
      ""
    else if(input[[buttonIdThisSection]] == 0)
      "<result will be here>"
    else
      #paste(
      capture.output(result) #)
  })
  outputRunCode = textOutput(outputId = outputIdThisSection)
  span(paste("(", thisSectionNumber, ") "),
       actionButton(buttonIdThisSection, buttonLabel),
       ifelse(showCode, pre(codeToRun), ""),  outputRunCode
  )
}
