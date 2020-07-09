////////////////////////////////////////////////////////////////////////////////
// FILE: index.js
// AUTHOR: David Ruvolo
// CREATED: 2020-07-08
// MODIFIED: 2020-07-08
// PURPOSE: parcel entry
// DEPENDENCIES: NA
// STATUS: in.progress
// COMMENTS: load scss, modules, etc.
////////////////////////////////////////////////////////////////////////////////

// import scss
import "./scss/index.scss"


// import modules
import _setDocMeta from "./js/_setDocMeta"
import Accordion from "./js/_accordion"

////////////////////////////////////////

// functions to run on DOMLOADED
window.addEventListener("DOMContentLoaded", function(e) {
    _setDocMeta();
}, { once: true })


// register bindings
Shiny.inputBindings.register(Accordion);
