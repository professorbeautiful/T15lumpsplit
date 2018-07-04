function navigateToId(id)  {
  location.hash = "#" + id;
}

$(document).on("keypress", function (e) {
    var x = event.which || event.keyCode;
    if(x==27) // escape key
      navigateToId(currentLocationId);
});
