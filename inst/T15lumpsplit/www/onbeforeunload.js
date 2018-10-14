//User must have interacted with the page, or elsd this is ignored.

// This much is needed... but returnValue is ignored except in IE.
window.addEventListener("beforeunload", function (event) {
  // Cancel the event as stated by the standard.
  //event.preventDefault();   // not needed in safari.
  // Chrome requires returnValue to be set.
  //event.returnValue = 'Hey there';
  //return 'Hey there';
});
/*


/*
//Not needed.
window.onbeforeunload = function (e) {
  //document.getElementById('downloadQandA').click();
  window.setTimeout(function() {
    document.getElementById('downloadQandA').click();
  }, 1000);
  window.setTimeout(function() {
    document.getElementById('downloadAnswers').click();
  }, 1000);
  // ('ALL DONE!'); No alerts allowed.
  e.returnValue = "You have attempted to leave this page.  If you have made any changes to the fields without clicking the Save button, your changes will be lost.  Are you sure you want to exit this page?";

};

/*

window.onbeforeunload = confirmExit;
  function confirmExit()
  {
    return "You have attempted to leave this page.  If you have made any changes to the fields without clicking the Save button, your changes will be lost.  Are you sure you want to exit this page?";
  }
  */
