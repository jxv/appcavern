#! /bin/sh

yaml2json () {
  ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load($stdin.read))'
}


for i in api/*.yaml; do
    [ -f "$i" ] || break
    echo $i
    j=`basename $i .yaml`
    yaml2json < $i > "api_readonly/$j.json"
done

fluid -l haskell -s ./api_readonly -m AppCavern.Api -n Api -d ./src/AppCavern -e server
