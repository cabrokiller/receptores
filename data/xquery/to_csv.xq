let $qt := "&#34;"
let $sep:= "&#59;"
for $drug in db:open('fulldatabase')/drugbank/drug
for $target in $drug/targets/target
for $action in $target/actions
for $gene in $target/polypeptide
for $atrs in $drug/atc-codes/atc-code
for $smile in $drug/calculated-properties/property
where $inchi[kind="InChIKey"]
  return
    concat(
          $qt, $drug/name/text(), $qt, $sep,
          $qt, $inchi/value, $qt, $sep,
          $qt, $drug/half-life/text(), $qt, $sep,
          $qt, $atrs/@code/string(), $qt, $sep,
          $qt, $target/name, $qt, $sep,
          $qt, $gene/gene-name, $qt, $sep,
          $qt, $target/known-action, $qt, $sep,
          $qt, $action, $qt
          )