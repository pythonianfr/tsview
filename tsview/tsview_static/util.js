"use strict";

// init tsview form

function init_form(formid, actionuri) {
    // setup click handler and do initial click
    // to fetch the plot
    const $formid = $('#' + formid)
    $formid.submit(event => {
        event.preventDefault()
        let plotargs = $formid.serialize()
        fetch(actionuri + '?' + plotargs)
            .then(response => response.text())
            .then(response => {
                let $target = $('#output')
                $target.html(response)
                $target.append(`<a href="tsview?${plotargs}">Permalink</a>`)
                for (let item of new FormData(document.getElementById(formid)).entries()) {
                    let name = item[1]
                    $target.append(`<br/> <a href="tshistory/${name}" target=_blank>View ${name} history</a>`)
                }
                const table = $('.dataframe').DataTable()
                table.page.len(20);
                table.draw();
            })
    })
    // initial call
    $formid.submit()
}

// the form contains select2 fields, to be initialised at load time

function init_select2_fields(fields) {
    fields.forEach(domid => $('#' + domid).select2())
}
