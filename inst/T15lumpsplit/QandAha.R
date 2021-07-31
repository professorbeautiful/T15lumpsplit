QandAha = function(context='QA', linkLabel= "save") {
  thisQANumber = nextNumber(sequenceType = "QA")
  if(!exists('QA_contexts'))
    QA_contexts <<- list()
  QA_contexts[[thisQANumber]] <<- context

  outputIdThisQA = paste0('QA', thisQANumber)
  textareaIdThisQA = paste0('id', outputIdThisQA)
  linkIdThisQA = paste0('id', outputIdThisQA, "_link")
  observeEvent(eventExpr = input[[textareaIdThisQA]], {
    if(writingCookiesIsOK) {
      cookieText = paste0(
        'shinycookie::updateCookie(session, ',
        outputIdThisQA,
        '=input[["', textareaIdThisQA, '"]] )'
      )
      print(cookieText)
      eval(parse(text= cookieText ) )
    }
  })
  observeEvent(eventExpr = input[[linkIdThisQA]], {
    cat('Calling saveEntriesJS (QA)', date(), '\n')
    # shinyjs::js$saveEntriesJS()
    clickString =
      "document.getElementById('downloadAllUserEntries').click();"
    ### this fails
    tags$script(paste0('eval("', clickString, '")') )
    ### this works fine
    tags$script('eval(4123413)')
  })
  splitLayout(cellWidths = c("75%", "25%"),
              textAreaInput(inputId = textareaIdThisQA, width='200%',
                   label =
                         span(style='display:block; width: 200%',
                              paste("(QA", thisQANumber, ")",
                               "Ask a question and/or describe an 'aha' here.")
    ) )
    , actionLink(inputId = linkIdThisQA,
                 label = '(click to download all entries now)' )
    #(click or shift-cmd S saves all responses)')
  )
}
