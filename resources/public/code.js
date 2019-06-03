
function archiveItem(id) {
    req = new XMLHttpRequest();
    req.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            window.opener.location.reload();
            window.close();
        }
    };
    req.open("POST", "/archive");
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    req.send("selected-item=" + id);
}
