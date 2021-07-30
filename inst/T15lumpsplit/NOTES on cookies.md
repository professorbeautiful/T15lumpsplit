## Solving COMMENT and ANSWER problems with Bias,variance, ...


Put this in the JS code panel:
  Shiny.onInputChange("cookieinput", document.cookie);
then put this in the R code panel:
  input$cookieinput
Good, that works.
TODO: name the cookies after the contexts, i.e.
  Instead of QA and a number, QA-empBayeslogitRatios
  More robust to future edits.
TODO:  Parsing the cookies relies on the semi-colon separator.
So, we must substitute in case any joker uses a semi-colon.


Check this out: 
https://shiny.rstudio.com/articles/communicating-with-js.html
https://shiny.rstudio.com/articles/packaging-javascript.html


2021-07-29
QandAha.R and TextQuestion.R now write cookies for all the answers.
They SHOULD persist.
TODO: reload from cookies to textboxes.
(Be sure to disable the cookie-writing!)

Loading previous entries from disk is working (locally and at shinyapps).
Saving to disk using the button at the bottom is working (locally and at shinyapps).
Also control-shift-S does the file download.
	document.getElementById('downloadAllUserEntries').click();


### TODO:   TELL the user the file name.

Aha.   control-shift-S   does it directly.     In KeyHandler.js
		document.getElementById('downloadAllUserEntries').click();
but  "click to save all responses"  uses this indirect method.
	shinyjs::extendShinyjs(text = saveEntriesJScode,

I learned a bunch about extendShinyjs
The example in help for shinyjs works.
I can get a function to register , and they always look like the code in this file:
	 created-by-extendShinyJS.R
so the info is in the parent frame.
Also, names(js)

The method in 

In  R:
shinyjs::extendShinyjs(text = 'console.log(1345)', functions='shinytest')
js$shinytest      ok, but...
js$shinytest()   doesn't work.  It calls jsFuncHelper, which calls session$sendCustomMessage,
which calls private$sendMessage(custom = data)
which I can't see in the debugging panel.

Putting in print statements into js$shinytest, the fun values are
[1] "js$shinytest"
[1] "shinytest"


# Ah, problem solved!  
Looking at how shinyDebuggingPanel does it:
tags$script(
            paste0(
              '(eval("',   'console.log(9999)' , '"))'       # THIS WORKS!
            )
          )
AND it works with strings:
paste0(
              '(eval("',   'console.log(\\"STRING\\")' , '"))'       # THIS WORKS!
            )
  

=-=-=-=-=-=-=-=

Using cookies seems easy.
In R:
	shinycookie::updateCookie(
		  shiny::getDefaultReactiveDomain(), TQ1='I am TQ1')
This works.  Then  in JS,  document.cookie gives you:
	"TQ1=I%20am%20TQ1; TQ22222222=I%20am%20TQ2"
Parse it (at semicolons) with the code in cookie-example-2.R, saved now into readCookie.js
This works!   E.g.  readCookie('TQ1')

You can update the cookie from   R:  updateCookie(session, ...)

Persistence across sessions?  
In chrome://settings/cookies/detail?site=trials.shinyapps.io
"Expires
When the browsing session ends"
But: 
  time() + (10 * 365 * 24 * 60 * 60)

document.cookie = "username=Max Brown; expires=Wed, 05 Aug 2020 23:00:00 UTC";


You can run this in shinyDebugginPanel JS
	temp = 2452345;
temp = 'kjhgjk';    single or double quotes both ok.
Semicolon not needed.
Good news; after it disconnected at shinyapps, on reconnecting it remembered the cookie.
