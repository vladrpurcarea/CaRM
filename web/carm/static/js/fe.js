function newXhrAuthReq(method, url) {
    var sessionId = getCookieValue('sessionId');
    var xhr = new XMLHttpRequest();
    xhr.open(method, url);
    xhr.setRequestHeader("Authorization", "Basic " + sessionId);
    return xhr;
}

function getCookieValue(a) {
    var b = document.cookie.match('(^|;)\\s*' + a + '\\s*=\\s*([^;]+)');
    return b ? b.pop() : '';
}

function parseTimestamp(t) {
    return new Date((t-2208988800)*1000);
}
