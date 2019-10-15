all:
	vi sdsc19.Rmd
	Rscript -e 'rmarkdown::render("sdsc19.Rmd")'
	Rscript -e 'knitr::purl("sdsc19.Rmd")'
	mv sdsc19.html docs/index.html

purl:
	Rscript -e 'knitr::purl("sdsc19.Rmd")'

view:
	google-chrome docs/index.html

push:
	git commit -a -m 'update'
	git push
