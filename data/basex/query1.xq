<drugs>
{for $drug in db:open('fulldatabase')/drugbank/drug
for $target in $drug/targets/target
return 
  <drug name="{data($drug/name)}">
    <name>{ $drug/name/text() }</name>
    <half_life>{ $drug/half-life/text() }</half_life>
    <receptor> { $target/name/text()}</receptor>
    <gene> { $target/polypeptide/gene-name/text()}</gene>
    <action> { $target/actions/action/text()} </action>
    <knownaction>{ $target/known-action/text()}</knownaction>
  </drug>}
</drugs>