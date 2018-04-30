for $drug in db:open('fulldatabase')/drugbank/drug
for $target in $drug/targets/target
  return
    concat(
      escape-html-uri(
        string-join(
          (
             $drug/name,
             $target/name,
             $target/polypeptide/gene-name,
             $target/actions/action,
             $target/known-action
          )
        /normalize-space(),
        ",")
      ),
    codepoints-to-string(10))