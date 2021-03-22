function newXhrAuthReq(method, url) {
    var sessionId = getCookieValue('sessionId');
    var xhr = new XMLHttpRequest();
    xhr.open(method, url);
    xhr.setRequestHeader("Authorization", "Basic " + sessionId);
    return xhr;
}

function authFn(xhr, f) {
    return function() {
	if (xhr.status == 403) {
	    window.location.href = '/carm/static/login.html';
	}
	f();
    }
}

function getCookieValue(a) {
    var b = document.cookie.match('(^|;)\\s*' + a + '\\s*=\\s*([^;]+)');
    return b ? b.pop() : '';
}

function parseTimestamp(t) {
    return new Date((t-2208988800)*1000);
}

function toUniversalTime(date) {
    return (date.getTime()/1000)+2208988800
}

function getParameters() {
    var prmstr = window.location.search.substr(1);
    return prmstr != null && prmstr != "" ? transformToAssocArray(prmstr) : {};
}

function transformToAssocArray( prmstr ) {
    var params = {};
    var prmarr = prmstr.split("&");
    for ( var i = 0; i < prmarr.length; i++) {
        var tmparr = prmarr[i].split("=");
        params[tmparr[0]] = tmparr[1];
    }
    return params;
}

function capitalizeFirstLetter(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}
