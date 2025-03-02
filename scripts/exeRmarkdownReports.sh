#!/bin/bash

export createPdfReport=1

# generate rmarkdown reports
Rscript -e "rmarkdown::render(input='01_derive_voting_blocs.Rmd', output_format='pdf_document', output_file='01_derive_voting_blocs.pdf')"
Rscript -e "rmarkdown::render(input='02_exploratory_data_analysis.Rmd', output_format='pdf_document', output_file='02_exploratory_data_analysis.pdf')"
Rscript -e "rmarkdown::render(input='03_data_processing.Rmd', output_format='pdf_document', output_file='03_data_processing.pdf')"
Rscript -e "rmarkdown::render(input='04_data_modelling.Rmd', output_format='pdf_document', output_file='04_data_modelling.pdf')"
Rscript -e "rmarkdown::render(input='05_model_evaluation.Rmd', output_format='pdf_document', output_file='05_model_evaluation.pdf')"
Rscript -e "rmarkdown::render(input='06_conclusion.Rmd', output_format='pdf_document', output_file='06_conclusion.pdf')"

if [[ "$createPdfReport" == 1 ]]; then
	# move .pdf files to reports
	mv 01_derive_voting_blocs.pdf ../report/01_derive_voting_blocs.pdf
	mv 02_exploratory_data_analysis.pdf ../report/02_exploratory_data_analysis.pdf
	mv 03_data_processing.pdf ../report/03_data_processing.pdf
	mv 04_data_modelling.pdf ../report/04_data_modelling.pdf
	mv 05_model_evaluation.pdf ../report/05_model_evaluation.pdf
	mv 06_conclusion.pdf ../report/06_conclusion.pdf
else
	# delete .pdf files to reports
	rm 01_derive_voting_blocs.pdf
	rm 02_exploratory_data_analysis.pdf
	rm 03_data_processing.pdf
	rm 04_data_modelling.pdf
	rm 05_model_evaluation.pdf
	rm 06_conclusion.pdf
fi