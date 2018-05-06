TOCnum = nextNumber(sequenceType="TOC")
navTOCid = paste0('navTOC',TOCnum);
#cat('navTOCid', navTOCid, '\n')
cbInputId = paste0('cbInputId', navTOCid)
cbInputJS = paste0('input.',cbInputId)
output[[navTOCid]] <- renderUI({
  list(checkboxInput(cbInputId, "show/hide outline", value=FALSE),
       conditionalPanel(condition = cbInputJS,
                        tagList(inclRmd("navTOC.Rmd")) )
  )
})
uiOutput(navTOCid)
