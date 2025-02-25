set createPdfReport=1==0

:: generate rmarkdown reports
call Rscript -e "rmarkdown::render(input='01_derive_voting_blocs.Rmd', output_format='pdf_document', output_file='01_derive_voting_blocs.pdf')"
call Rscript -e "rmarkdown::render(input='02_exploratory_data_analysis.Rmd', output_format='pdf_document', output_file='02_exploratory_data_analysis.pdf')"
call Rscript -e "rmarkdown::render(input='03_data_processing.Rmd', output_format='pdf_document', output_file='03_data_processing.pdf')"
call Rscript -e "rmarkdown::render(input='04_data_modelling.Rmd', output_format='pdf_document', output_file='04_data_modelling.pdf')"
call Rscript -e "rmarkdown::render(input='05_model_evaluation.Rmd', output_format='pdf_document', output_file='05_model_evaluation.pdf')"
call Rscript -e "rmarkdown::render(input='06_conclusion.Rmd', output_format='pdf_document', output_file='06_conclusion.pdf')"

if %createPdfReport% (
	:: move .pdf files to reports
	call move 01_derive_voting_blocs.pdf ..\report\01_derive_voting_blocs.pdf
	call move 02_exploratory_data_analysis.pdf ..\report\02_exploratory_data_analysis.pdf
	call move 03_data_processing.pdf ..\report\03_data_processing.pdf
	call move 04_data_modelling.pdf ..\report\04_data_modelling.pdf
	call move 05_model_evaluation.pdf ..\report\05_model_evaluation.pdf
	call move 06_conclusion.pdf ..\report\06_conclusion.pdf
) else (
	:: delete .pdf files to reports
	call del 01_derive_voting_blocs.pdf
	call del 02_exploratory_data_analysis.pdf
	call del 03_data_processing.pdf
	call del 04_data_modelling.pdf
	call del 05_model_evaluation.pdf
	call del 06_conclusion.pdf
)