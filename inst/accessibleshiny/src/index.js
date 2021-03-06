////////////////////////////////////////////////////////////////////////////////
// FILE: index.js
// AUTHOR: David Ruvolo
// CREATED: 2020-07-08
// MODIFIED: 2020-09-09
// PURPOSE: parcel entry
// DEPENDENCIES: NA
// STATUS: in.progress
// COMMENTS: load scss, modules, etc.
////////////////////////////////////////////////////////////////////////////////

// import scss
import "./scss/index.scss"


// import modules
import setDocMeta from "./js/_setDocMeta"
import Accordion from "./js/_accordion"

////////////////////////////////////////

// functions to run on DOMLOADED
window.addEventListener("DOMContentLoaded", function(e) {
    const refElem = document.getElementById("accessible__shiny__meta");
    if (refElem) {
        setDocMeta();
    }
    if (!refElem) {
        console.error("accessibleshiny: meta element does not exist")
    }
}, { once: true })


// register bindings
Shiny.inputBindings.register(Accordion);
