set createPdfReport=0

:: generate rmarkdown reports
call Rscript -e "rmarkdown::render(input='scripts/01_derive_voting_blocs.Rmd', output_format='pdf_document', output_file='01_derive_voting_blocs.pdf');"
call Rscript -e "rmarkdown::render(input='scripts/02_exploratory_data_analysis.Rmd', output_format='pdf_document', output_file='02_exploratory_data_analysis.pdf');"
call Rscript -e "rmarkdown::render(input='scripts/03_data_processing.Rmd', output_format='pdf_document', output_file='03_data_processing.pdf');"
call Rscript -e "rmarkdown::render(input='scripts/04_data_modelling.Rmd', output_format='pdf_document', output_file='04_data_modelling.pdf');"
call Rscript -e "rmarkdown::render(input='scripts/05_model_evaluation.Rmd', output_format='pdf_document', output_file='05_model_evaluation.pdf');"
call Rscript -e "rmarkdown::render(input='scripts/06_conclusion.Rmd', output_format='pdf_document', output_file='06_conclusion.pdf');"

if %createPdfReport%==1 (
	:: move .pdf files to reports
	call move scripts\01_derive_voting_blocs.pdf report\01_derive_voting_blocs.pdf
	call move scripts\02_exploratory_data_analysis.pdf report\02_exploratory_data_analysis.pdf
	call move scripts\03_data_processing.pdf report\03_data_processing.pdf
	call move scripts\04_data_modelling.pdf report\04_data_modelling.pdf
	call move scripts\05_model_evaluation.pdf report\05_model_evaluation.pdf
	call move scripts\06_conclusion.pdf report\06_conclusion.pdf
) else (
	:: delete .pdf files to reports
	call del scripts\01_derive_voting_blocs.pdf
	call del scripts\02_exploratory_data_analysis.pdf
	call del scripts\03_data_processing.pdf
	call del scripts\04_data_modelling.pdf
	call del scripts\05_model_evaluation.pdf
	call del scripts\06_conclusion.pdf
)