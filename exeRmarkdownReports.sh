#!/bin/bash
export createPdfReport=0

# generate rmarkdown reports
LC_ALL=C.UTF-8 Rscript -e "renv::load(); rmarkdown::render(input='scripts/01_derive_voting_blocs.Rmd', output_format='pdf_document', output_file='01_derive_voting_blocs.pdf');"
LC_ALL=C.UTF-8 Rscript -e "renv::load(); rmarkdown::render(input='scripts/02_exploratory_data_analysis.Rmd', output_format='pdf_document', output_file='02_exploratory_data_analysis.pdf');"
LC_ALL=C.UTF-8 Rscript -e "renv::load(); rmarkdown::render(input='scripts/03_data_processing.Rmd', output_format='pdf_document', output_file='03_data_processing.pdf');"
LC_ALL=C.UTF-8 Rscript -e "renv::load(); rmarkdown::render(input='scripts/04_data_modelling.Rmd', output_format='pdf_document', output_file='04_data_modelling.pdf');"
LC_ALL=C.UTF-8 Rscript -e "renv::load(); rmarkdown::render(input='scripts/05_model_evaluation.Rmd', output_format='pdf_document', output_file='05_model_evaluation.pdf');"
LC_ALL=C.UTF-8 Rscript -e "renv::load(); rmarkdown::render(input='scripts/06_conclusion.Rmd', output_format='pdf_document', output_file='06_conclusion.pdf');"

if [[ "$createPdfReport" == 1 ]]; then
	# move .pdf files to reports
	mv scripts/01_derive_voting_blocs.pdf report/01_derive_voting_blocs.pdf
	mv scripts/02_exploratory_data_analysis.pdf report/02_exploratory_data_analysis.pdf
	mv scripts/03_data_processing.pdf report/03_data_processing.pdf
	mv scripts/04_data_modelling.pdf report/04_data_modelling.pdf
	mv scripts/05_model_evaluation.pdf report/05_model_evaluation.pdf
	mv scripts/06_conclusion.pdf report/06_conclusion.pdf
else
	# delete .pdf files to reports
	rm scripts/01_derive_voting_blocs.pdf
	rm scripts/02_exploratory_data_analysis.pdf
	rm scripts/03_data_processing.pdf
	rm scripts/04_data_modelling.pdf
	rm scripts/05_model_evaluation.pdf
	rm scripts/06_conclusion.pdf
fi