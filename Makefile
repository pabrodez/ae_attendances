HTML_FILES=index.html

index.html : ae_attendances.Rmd style.css
	Rscript -e 'Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio/bin/pandoc"); rmarkdown::render(input = "ae_attendances.Rmd", output_file = "index.html")'

clean : 
	rm -f $(HTML_FILES)
