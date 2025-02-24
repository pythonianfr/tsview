(cd .. && make tsformula-cmd)
#tsformula show --dumpjs /tmp/res.js -s ../elm/FormulaParserValidation/spec.json 

echo "" > /tmp/parse_res.txt
echo "" > /tmp/edit_res.txt

for app in $(ls formula/)
do
    csv_file="formula/${app}"

    echo "Processing ${app}"
    echo "Parsing ${app}" >> /tmp/parse_res.txt
    tsformula -s main_spec.json parse ${csv_file} >> /tmp/parse_res.txt

    echo "Editing ${app}" >> /tmp/edit_res.txt
#    tsformula -s main_spec.json edit ${csv_file} >> /tmp/edit_res.txt
done

grep KO /tmp/parse_res.txt || echo OK
