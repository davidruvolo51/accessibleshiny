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


// import bindings
import Accordion from "./js/_accordion"


// register bindings
Shiny.inputBindings.register(Accordion);
