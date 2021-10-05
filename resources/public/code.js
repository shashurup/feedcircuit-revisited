
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

function initAppearance() {
    setAppearanceVisibility(
        document.getElementById("styles").value ||
        document.getElementsByName("extra-style")[0].value);
}

function toggleAppearance() {
    setAppearanceVisibility(
        document.getElementById('appearance').style.display == "none");
}


function setAppearanceVisibility(visible) {
    let appearanceDiv = document.getElementById('appearance');
    let appearanceHeader = document.getElementById('appearance-header');
    if (visible) {
        appearanceDiv.style.display = "initial";
        appearanceHeader.innerText = appearanceHeader.innerText.replace(" ...", "");
    }
    else {
        appearanceDiv.style.display = "none";
        appearanceHeader.innerText += " ...";
    }
}

function renameForm() {
    var form = document.getElementById("main");
    form.id = "main-js-enabled";
}
