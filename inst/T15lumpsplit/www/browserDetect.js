// Safari 3.0+ "[object HTMLElementConstructor]"
/*
var isSafari = /constructor/i.test(window.HTMLElement) || (function (p) { return p.toString() === "[object SafariRemoteNotification]"; })(!window['safari'] || (typeof safari !== 'undefined' && safari.pushNotification));

var oprAddons = (typeof opr === 'object')? opr.addons : false;
*/

var isSafari = navigator.vendor &&
  navigator.vendor.indexOf('Apple') > -1 &&
  navigator.userAgent &&
  navigator.userAgent.indexOf('CriOS') == -1 &&
  navigator.userAgent.indexOf('FxiOS') == -1;

Shiny.onInputChange("isSafari", isSafari);

var isFirefox =
  navigator.userAgent &&
  navigator.userAgent.indexOf('Mozilaa') > -1 ;

Shiny.onInputChange("isSafari", isSafari);
var isSafari = navigator.vendor &&
  navigator.vendor.indexOf('Apple') > -1 &&
  navigator.userAgent &&
  navigator.userAgent.indexOf('CriOS') == -1 &&
  navigator.userAgent.indexOf('FxiOS') == -1;

Shiny.onInputChange("isSafari", isSafari);
Shiny.onInputChange("isFirefox", isFirefox);

/*
if( ! isSafari)
 alert("Please use Safari as your browser for full features.");
*/
