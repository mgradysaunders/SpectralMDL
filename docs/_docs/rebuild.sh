# TODO minify stylesheet
asciidoctor -r ./smdl.rb *.adoc
mv *.html ../docs
