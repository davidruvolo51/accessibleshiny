////////////////////////////////////////////////////////////////////////////////
// FILE: _accordion.js
// AUTHOR: David Ruvolo
// CREATED: 2020-07-08
// MODIFIED: 2020-07-08
// PURPOSE: accordion button input binding
// DEPENDENCIES: Shiny assets
// STATUS: in.progress
// COMMENTS: This input binding toggles all instances of the accordion
// component. This script will create a new constructor that extends the
// Shiny.InputBinding() and adjust the state of the component when the
// toggle is clicked. ARIA attributes will also be adjusted. Return current
// state for use in the server (if needed by the user).
////////////////////////////////////////////////////////////////////////////////
// BEGIN

// create new binding
var Accordion = new Shiny.InputBinding();
$.extend(Accordion, {
    
    // find: locate component within scope
    find: function(scope) {
        return $(scope).find(".accordion");
    },

    // initialize: init component
    initialize: function(el) {
        return $(el).find(".accordion-button").attr("aria-expanded").toUpperCase();
    },

    // getValue: return component's current state
    getValue: function(el) {
        return $(el).find(".accordion-button").attr("aria-expanded").toUpperCase();
    },

    // subscribe: create an register DOM events
    subscribe: function(el, callback) {

        // pull elements
        var btn = $(el).find(".accordion-button");
        var icon = btn.find(".accordion-button-icon");
        var section = $(el).find(".accordion-section");
        
        // onClick event
        btn.on("click", function(e) {

            // toggle: component state + ARIA attributes
            if (btn.attr("aria-expanded") === "false") {
                btn.attr("aria-expanded", "true");
                section.removeAttr("hidden");
                icon.addClass("rotated");
            } else {
                btn.attr("aria-expanded", "false");
                section.attr("hidden", "");
                icon.removeClass("rotated");
            }

            // return state (if needed in the server)
            callback();
        });
    },

    // unsubscribe: clean up
    unsubscribe: function(el) {
        $(el).off(".Accorion");
        $(el).off("click");
    }
})

// export
export default Accordion