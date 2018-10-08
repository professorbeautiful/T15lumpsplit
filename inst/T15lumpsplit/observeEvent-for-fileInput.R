observeEventForFileInput = function(contentType = 'comments')
  observeEvent(
  input[[paste0(contentType, 'FileInput')]], {
    print(paste0("Entering ", 'observeEvent_for_',contentType))
    inFile <- input[[paste0(contentType, 'FileInput')]]
    if (is.null(inFile))
      return(NULL)
    savedContents = read.csv(
      file = inFile$datapath,
      header = TRUE,
      stringsAsFactors = FALSE)
    rValues[[paste0('saved', contentType)]] = savedContents
    if(contentType == 'comments'){
      the_contexts = QA_contexts
      the_prefix = 'idQA'
    }
    if(contentType == 'answers'){
      the_contexts = TQ_contexts
      the_prefix = 'idTQ'
    }
    for(contextNumber in seq(along=the_contexts) ) {
      contextID = paste0(the_prefix, contextNumber)
      context = the_contexts[[contextNumber]]
      cat("input ", contextID, "\n")
      print(paste(contextID, " in place", context, "===>",
                  input[[contextID]], '\n'))
      retrievedcontext = savedContents[contextNumber, 'contexts']
      retrievedcontent = gsub('^ *', '',
                              savedContents[contextNumber, 'contents'])
      retrievedcontent = gsub('[1] *', '', fixed = TRUE, retrievedcontent)
      print(paste(contextID, 'retrieved: ',
                  retrievedcontext, "===>", retrievedcontent, '/'))
      retrievedcontent = as.vector(retrievedcontent)
      retrievedcontent = strsplit(split='\n', retrievedcontent)[[1]]
      if( ! identical(retrievedcontent, "")) {
        cat(" Placing ", retrievedcontent, ' into ',
            retrievedcontext,  '\n')
        updateTextAreaInput(session = session,
                            inputId = contextID,
                            value = retrievedcontent)
      }
    }
  }
)
