
function addToSelected(id) {
    req = new XMLHttpRequest();
    req.open("POST", "/selected");
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    req.send("id=" + id);
}

function removeFromSelected(id) {
    req = new XMLHttpRequest();
    req.open("DELETE", "/selected?id="+id);
    req.send();
}

function toggleItem(item) {
    var add = item.checked;
    if (item.className.search(/selected-item/) >= 0)
        add = !add;
    if (add)
        addToSelected(item.value);
    else
        removeFromSelected(item.value);
}

function UnselectAndClose(id) {
    req = new XMLHttpRequest();
    req.onreadystatechange = function() {
        if (this.readyState == 4 && this.status == 200) {
            try {
                window.opener.location.reload();
            } finally {
                window.close();
            }
        }
    };
    req.open("DELETE", "/selected?id="+id);
    req.send();
}
