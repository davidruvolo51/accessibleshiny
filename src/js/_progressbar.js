////////////////////////////////////////////////////////////////////////////////
// FILE: _progressbar.js
// AUTHOR: David Ruvolo
// CREATED: 2020-10-03
// MODIFIED: 2020-10-03
// PURPOSE: progressbar input binding
// DEPENDENCIES: NA
// STATUS: in.progress
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////

// define
var progress = new Shiny.InputBinding();

// round
function __round(x, precision){
    const p = Math.pow(10, precision);
    return Math.round(x * p) / p;
}

// gather data
// @param el an element to select
// @param method a string indicating which value to update (min, max, current) 
function progressbar__data(el) {
    const container = $(el).parent();
    const width = container.width();
    const max = $(el).attr("aria-valuemax");
    const value = $(el).attr("aria-valuecurrent");
    const bins = width / max;
    const rate = bins / width
    const transform_value = rate * value;
    return {
        value: value,
        max: max,
        width: width,
        rate: rate,
        transform_value: __round(transform_value, 3)
    }
}


// create binding
$.extend(progress,{

    // locate all instances of the progressbar element
    find: function(scope) {
        return $(scope).find(".progressbar");
    },

    // onRender
    initialize: function(el) {
        $(el).parent().addClass("progressbar__parent");
        var d = progressbar__data(el);
        $(el).find(".bar").css("transform",  `scaleX(${d.transform_value})`);
    },

    // get value
    getValue: function(el) {
        return false;
    },

    // receive messages from shiny
    receiveMessage: function(el, message) {
        $(el).attr("aria-valuecurrent", message.current);
        var d = progressbar__data(el);
        $(el).find(".bar").css("transform",  `scaleX(${d.transform_value})`);
        $(el).attr("aria-valuetext", message.text);

    },
    unsubcribe: function(el) {
        $(el).off("progress");
    }
});

// register
export default progress;