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
if(isSafari === "") isSafari = false;
Shiny.onInputChange("isSafari", isSafari);

var isFirefox =
  navigator.userAgent &&
  navigator.userAgent.indexOf('Mozilla') > -1 ;
if(isFirefox === "") isFirefox = false;
// Actually, Safari also has navigator.userAgent
// equal to Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/11.1.2 Safari/605.1.15.
// So wrong, but it works ok for us.
Shiny.onInputChange("isFirefox", isFirefox);

/*
if( ! isSafari)
 alert("Please use Safari as your browser for full features.");
*/
