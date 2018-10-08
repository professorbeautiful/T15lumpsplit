// WE MUST PUT ALL THE KEY HANDLING IN ONE PLACE

var ctrlDpressed = false;
var ctrlShiftSpressed = false;
var keyVar = 0;
var eventSaved = null;


$(document).on("keyup", function (event) {
    eventSaved = event;
    x = event.which || event.keyCode;
    keyVar = x;
    eventSaved = event;
    if(x==(68+15) && event.ctrlKey && event.shiftKey) { // control shift s
            ctrlShiftSpressed = ! ctrlShiftSpressed;
            Shiny.onInputChange("ctrlShiftSpressed", ctrlShiftSpressed);
            document.getElementById('downloadQandA').click();
            document.getElementById('downloadAnswers').click();
    }
    if(x==68 && event.ctrlKey) { // control d
      ctrlDpressed = ! ctrlDpressed;  // toggle
      Shiny.onInputChange("ctrlDpressed", ctrlDpressed);
      if(ctrlDpressed === true)
        navigateToY(0);
      //navigateToId(currentLocationId);
      //alert('control d ' + e.ctrlKey + ' ' + event.ctrlKey);
      // The alert works.
      // The panel appears only after something else JS happens.  WAIT, THAT'S NOT TRUE! IT WORKS!
      //
    }
    Shiny.onInputChange("Latestkeypressedx", x);
    Shiny.onInputChange("Latestkeypressede", event.code);
    if(x==27) { // escape key
      //navigateToId(currentLocationId);
      Key_pressed_for_navigateToY = true;
      Shiny.onInputChange("KeypressedfornavigateToY",
        Key_pressed_for_navigateToY);
      navigateToY(savedYposition);
  }
});
