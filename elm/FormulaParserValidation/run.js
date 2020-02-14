const fs = require('fs');
const parse = require('csv-parse/lib/sync');

const elm = require('validation.js');

const records = parse(
    fs.readFileSync('formula.csv', 'utf8'),
    {columns: true, quote: "'"}
);

var app = elm.Elm.FormulaParserValidation.Main.init({
    flags: [
        JSON.parse(fs.readFileSync('spec.json')),
        records
    ]
});

console.log('DONE');
