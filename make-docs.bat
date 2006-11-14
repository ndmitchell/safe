mkdir docs
copy Safe.hs docs\Safe.hs
haddock -h docs\Safe.hs --odir=docs --source-module=Safe.hs.html --source-entity=Safe.hs.html#%%{NAME}

copy d:\bin\hscolour.css docs\hscolour.css
hscolour -anchorCSS docs\Safe.hs > docs\Safe.hs.html

if "%1" == "deploy" copy "docs\*.*" "W:\projects\safe\*.*"

