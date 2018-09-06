//Used by zoomAdvice.Rmd

//tags$head(tags$script('

// https://shiny.rstudio.com/articles/js-events.html

copyDims = function(e) {
    Shiny.onInputChange("innerHeight", window.innerHeight);
    Shiny.onInputChange("innerWidth", window.innerWidth);
    //alert('copyDims');
};

$(document).on('shiny:sessioninitialized', function(event) {
  alert('shiny:sessioninitialized');
});

$(document).on("shiny:sessioninitialized", copyDims);
$(window).resize(copyDims);

//                      '))
