(cd .. && make tsformula-cmd)
#tsformula show --dumpjs /tmp/res.js -s ../elm/FormulaParserValidation/spec.json 

echo "" > /tmp/parse_res.txt
echo "" > /tmp/edit_res.txt

for app in eflower greenalp kallista meteo totalbe
do
    echo "Processing ${app}"
    echo "Parsing ${app}" >> /tmp/parse_res.txt
    tsformula -s spec/${app}_spec.json \
        parse formula/${app}_formula.csv >> /tmp/parse_res.txt

    echo "Editing ${app}" >> /tmp/edit_res.txt
#    tsformula -s spec/${app}_spec.json \
#        edit formula/${app}_formula.csv >> /tmp/edit_res.txt
done

grep KO /tmp/parse_res.txt || echo OK
