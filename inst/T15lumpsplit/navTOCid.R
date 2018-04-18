navTOCid = paste0('navTOC',TOCnum); TOCnum = TOCnum+1
output[[navTOCid]] <- renderUI({ tagList(inclRmd("navTOC.Rmd") )})
div(uiOutput(navTOCid))
