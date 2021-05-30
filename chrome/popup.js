
chrome.tabs.query({ active: true, currentWindow: true }, function(tabs) {
    let url = tabs[0].url;
    document.write("Adding<br>" + url + "<br>to Feedcircuit<br>...<br>")
    req = new XMLHttpRequest();
    req.open("POST", "https://feedcircuit.kibardin.name/selected");
    req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    req.onreadystatechange = function() {
        if (req.readyState === XMLHttpRequest.DONE) {
            if (req.status === 0 || (req.status >= 200 && req.status < 400)) {
                document.write("done");
            }
        }
    }
    req.send("id=" + url);
});
