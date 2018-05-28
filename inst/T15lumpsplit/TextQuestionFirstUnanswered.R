TextQuestionFirstUnanswered = reactive({
  TextQuestionLastAnswered() + 1
})

#### TextQuestionLastAnswered ####
# To allow user to return quickly to the location
# of the last question that he/she answered.
TextQuestionLastAnswered = reactive( {
  blankAnswers = sapply (
    1:getSequenceLength('TQ'),
    function(thisTQNumber)
      input[[paste0(paste0('TQ', thisTQNumber))]] != "")
  rev(which(blankAnswers) ) [1]
}
)


## To do:  implement shortcut key ##
