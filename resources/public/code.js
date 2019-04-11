
function setItemState(id, state) {
    req = new XMLHttpRequest();
    req.open("POST", "/mark-item");
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    req.send("item-id=" + id + "&read=" + state);
}
