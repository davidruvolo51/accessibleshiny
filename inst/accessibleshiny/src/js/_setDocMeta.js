////////////////////////////////////////////////////////////////////////////////
// FILE: _setDocMeta.js
// AUTHOR: David Ruvolo
// CREATED: 2020-07-09
// MODIFIED: 2020-09-09
// PURPOSE: set document meta content that cannot be done using shiny tags
// DEPENDENCIES: NA
// STATUS: working
// COMMENTS: when imported and attached to DOMContentLoaded, make sure <span>
// is rendered.
////////////////////////////////////////////////////////////////////////////////

// define function
function setDocMeta() {
    const targetElem = document.getElementsByTagName("html")[0];
    const refElem = document.getElementById("accessible-shiny-meta");
    targetElem.lang = refElem.getAttribute("data-html-lang");
    targetElem.dir = refElem.getAttribute("data-html-dir");
}

// export
export default setDocMeta

