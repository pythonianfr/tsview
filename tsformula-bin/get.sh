for site in eflower totalbe kallista
do
    curl -n https://refinery.${site}.pythonian.fr/downloadformulas \
        > formula/${site}_formula.csv
    curl -n https://refinery.${site}.pythonian.fr/spec | jq . \
        > spec/${site}_spec.json
done
