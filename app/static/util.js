"use strict";

// init tsview form

function init_tsviewform() {
    // setup click handler and do initial click
    // to fetch the plot
    $('#tsviewform').submit(event => {
        event.preventDefault()
        let plotargs = $('#tsviewform').serialize()
        fetch('tsplot?' + plotargs)
            .then(response => response.text())
            .then(response => {
                let $target = $('#output')
                $target.html(response)
                $('.dataframe').DataTable()
            })
    })
    // initial call
    $('#tsviewform').submit()
}

// the form contains select2 fields, to be initialised at load time

function init_select2_fields(fields) {
    fields.forEach(domid => $('#' + domid).select2())
}
