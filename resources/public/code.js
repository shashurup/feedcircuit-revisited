
function postTo(resource, body) {
    req = new XMLHttpRequest();
    req.open("POST", resource);
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    req.send(body);
}

function feedItemChanged(item, url) {
    if (item.checked)
        postTo("/selected-add", "id=" + item.value)
    else
        postTo("/selected-remove", "url=" + url)
}

function selectedItemChanged(item, url) {
    if (item.checked)
        postTo("/selected-remove", "url=" + url)
    else
        postTo("/selected-add", "id=" + item.value)
}

function removeFromSelected(url) {
    req = new XMLHttpRequest();
    req.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            window.opener.location.reload();
            window.close();
        }
    };
    req.open("POST", "/selected-remove");
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    req.send("url=" + url);
}
