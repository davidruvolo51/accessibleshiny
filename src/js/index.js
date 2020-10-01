////////////////////////////////////////////////////////////////////////////////
// FILE: index.js
// AUTHOR: David Ruvolo
// CREATED: 2020-03-20
// MODIFIED: 2020-05-15
// PURPOSE: index.js for packge
// DEPENDENCIES: NA
// STATUS: working
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////
// BEGIN

// ~ 1 ~
// progress bar handlers
// define a few functions that updates the progress bar. These functions should
// calculate the width needed to update based on the relative screen size

// ~ 1a ~
// cacluate data for progress bar
function progress_data(el, now, max) {
    const elem = document.getElementById(el);
    const container = elem.parentElement;
    const width = container.getBoundingClientRect().width;
    const bins = width / max;
    const rate = bins / width
    const transform_value = rate * now;
    return {
        width: width,
        rate: rate,
        transform_value: transform_value
    }
}

// ~ 1b ~
// init_parent_element
function init_parent_element(el) {
    const elem = document.getElementById(el);
    const parent = elem.parentElement.parentElement;
    parent.classList.add("progress-bar-parent");
}

// ~ 1c ~ 
// update function
function update_progress_bar(el, now, max) {
    const elem = document.getElementById(el);
    const d = progress_data(el, now, max);
    elem.style.transform = `scaleX(${d.transform_value})`;
}

// ~ 1d ~
// Register input functions
// bind update function
Shiny.addCustomMessageHandler("update_progress_bar", function (value) {
    update_progress_bar(value[0], value[1], value[2]);
});

Shiny.addCustomMessageHandler("init_parent_element", function (value) {
    init_parent_element(value);
});

////////////////////////////////////////////////////////////////////////////////

// ~ 2 ~
// Accordion Functions
// The following functions are used to process the open and closing of the
// accordion components.

// ~ 2a ~
// function to open an close accordion toggle
// function setAccordionToggle(id) {

//     // select accordion elements
//     var acc_btn = document.querySelector(`button[data-group='${id}']`);
//     var acc_svg = document.querySelector(`svg[data-group='${id}']`);
//     var acc_html = document.querySelector(`section[data-group='${id}']`);

//     // update classes
//     acc_html.classList.toggle('accordion-hidden');
//     acc_svg.classList.toggle('rotated');
//     if (acc_btn.getAttribute('aria-expanded', 'value') === 'false') {
//         acc_btn.setAttribute('aria-expanded', true);
//         acc_html.removeAttribute('hidden');
//     } else {
//         acc_btn.setAttribute('aria-expanded', 'false');
//         acc_html.setAttribute('hidden', 'true');
//     }
// }

// // ~ 2b ~
// // function that binds toggle function to all accordion elements
// function useAccordionToggles() {
//     const btns = document.querySelectorAll("button[data-controls='accordion-toggle']");
//     btns.forEach((btn) => {
//         btn.addEventListener("click", () => { 
//             let id = btn.getAttribute("data-group");
//             setAccordionToggle(id);
//         });
//     });
// }

// // ~ 2c ~
// // Run when DOMContentLoaded
// window.addEventListener("DOMContentLoaded", () => { useAccordionToggles() });