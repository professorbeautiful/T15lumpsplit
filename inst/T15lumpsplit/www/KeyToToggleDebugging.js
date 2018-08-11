var ctrlDpressed = false;

$(document).on("keyup", function (e) {
    var x = event.which || event.keyCode;
    if(x==68 && e.ctrlKey) { // control d
      ctrlDpressed = true;
      //navigateToId(currentLocationId);
      alert('control d ' + e.ctrlKey + ' ' + event.ctrlKey);
    }
});
