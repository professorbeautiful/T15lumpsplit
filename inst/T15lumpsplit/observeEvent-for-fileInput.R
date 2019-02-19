observeEventForFileInput = function(contentType = 'all_entries_')
  observeEvent(
    input[[paste0(contentType, 'FileInput')]], {
      print(paste0("Entering ", 'observeEvent_for_',contentType))
      fun_observeEventForFileInput(contentType)
    }
  )

fun_observeEventForFileInput = function(contentType) {
  inFile <- input[[paste0(contentType, 'FileInput')]]
  if (is.null(inFile))
    return(NULL)
  savedContents = read.csv(
    file = inFile$datapath,
    header = TRUE,
    stringsAsFactors = FALSE)
  rValues[[paste0('saved', contentType)]] = savedContents
  for(contentType in c('QA', 'TQ')) {
      the_contexts = get(paste0(contentType, '_contexts'))
      the_prefix = paste0('id', contentType)
      savedContents_this_contentType =
        savedContents[grep(paste0('^', contentType),
                           savedContents$contexts),
                      ]
    for(contextNumber in seq(along=the_contexts) ) {
      contextID = paste0(the_prefix, contextNumber)
      context = the_contexts[[contextNumber]]
      cat("input ", contextID, "\n")
      print(paste(contextID, " in place", context, "===>",
                  input[[contextID]], '\n'))
      retrievedcontext = savedContents_this_contentType[contextNumber, 'contexts']
      retrievedcontent = gsub('^ *', '',
                              savedContents_this_contentType[contextNumber, 'contents'])
      retrievedcontent = gsub('[1] *', '', fixed = TRUE, retrievedcontent)
      print(paste(contextID, 'retrieved: ',
                  retrievedcontext, "===>", retrievedcontent, '/'))
      retrievedcontent = as.vector(retrievedcontent)
      retrievedcontent =
        gsub('\\n', '\n', fixed=TRUE, retrievedcontent)
      if( ! identical(retrievedcontent, "")) {
        cat(" Placing ", retrievedcontent, ' into ',
            retrievedcontext,  '\n')
        updateTextAreaInput(session = session,
                            inputId = contextID,
                            value = retrievedcontent)
      }
    }
  }
}

# debug(fun_observeEventForFileInput)
