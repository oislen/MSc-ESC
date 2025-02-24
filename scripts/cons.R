gen_output_report_files <- FALSE
gen_output_model_files <- FALSE
gen_output_data_files <-FALSE

# stats
fpath_stats_cor_predictors <- NA
fpath_stats_cor_response <- NA
fpath_stats_chisq_response <- NA
fpath_stats_descriptive_numeric <- NA
fpath_stats_descriptive_categorical <- NA
fpath_stats_na_prop_per_columns <- NA
fpath_stats_missing_oberations <- NA
# missing values directory
fdir_plots_missings <- NA
# box-cox plots
fpath_plots_boxcox_overall_model <- NA
fpath_plots_boxcox_tele_model <- NA
# residual vs fits
fpath_plots_residuals_fits_overall_model <- NA
fpath_plots_residuals_fits_tele_model <- NA
fpath_plots_residuals_fits_jury_model <- NA
# cooks distance
fpath_plots_residuals_cooks_overall_model <- NA
fpath_plots_residuals_cooks_tele_model <- NA
fpath_plots_residuals_cooks_jury_model <- NA
# residual influence 
fpath_plots_residuals_influence_overall_model <- NA
fpath_plots_residuals_influence_tele_model <- NA
fpath_plots_residuals_influence_jury_model <- NA
# residual distribution
fpath_plots_residuals_distribution_overall_model <- NA
fpath_plots_residuals_distribution_tele_model <- NA
fpath_plots_residuals_distribution_jury_model <- NA
# residual qq
fpath_plots_residuals_qq_overall_model <- NA
fpath_plots_residuals_qq_tele_model <- NA
fpath_plots_residuals_qq_jury_model <- NA
# residual spread level
fpath_plots_residuals_spreadlevel_overall_model <- NA
fpath_plots_residuals_spreadlevel_tele_model <- NA
fpath_plots_residuals_spreadlevel_jury_model <- NA

# if generating output report file paths
if (gen_output_report_files) {
  # stats
  fpath_stats_cor_predictors <- "../report/Stats/cor_tests_predictors.csv"
  fpath_stats_cor_response <- "../report/stats/cor_tests_response.csv"
  fpath_stats_chisq_response <-"../report/stats/chi_sq_tests_response.csv"
  fpath_stats_descriptive_numeric <- "../report/stats/numeric_descriptive_statistics.csv"
  fpath_stats_descriptive_categorical <- "../report/stats/categorical_descriptive_statistics.csv"
  fpath_stats_na_prop_per_columns <- '../report/stats/NA_prop_per_columns.csv'
  fpath_stats_missing_oberations <- "../report/stats/TC_FC_missing_obseration_df.csv"
  # missing values directory
  fdir_plots_missings <- '../report/plots/missings/'
  # box-cox plots
  fpath_plots_boxcox_overall_model <- '../report/plots/boxcox/model_overall_boxcox.jpg'
  fpath_plots_boxcox_tele_model <- '../report/plots/boxcox/model_tele_boxcox.jpg'
  # residual vs fits
  fpath_plots_residuals_fits_overall_model <- '../report/plots/residual/model_overall_residuals_vs_fits.jpg'
  fpath_plots_residuals_fits_tele_model <- '../report/plots/residual/model_tele_residuals_vs_fits.jpg'
  fpath_plots_residuals_fits_jury_model <- '../report/plots/residual/model_jury_residuals_vs_fits.jpg'
  # cooks distance
  fpath_plots_residuals_cooks_overall_model <- '../report/plots/residual/model_overall_cooks.jpg'
  fpath_plots_residuals_cooks_tele_model <- '../report/plots/residual/model_tele_cooks.jpg'
  fpath_plots_residuals_cooks_jury_model <- '../report/plots/residual/model_jury_cooks.jpg'
  # residual influence 
  fpath_plots_residuals_influence_overall_model <- '../report/plots/residual/model_overall_influence.jpg'
  fpath_plots_residuals_influence_tele_model <- '../report/plots/residual/model_televote_influence.jpg'
  fpath_plots_residuals_influence_jury_model <- '../report/plots/residual/model_jury_influence.jpg'
  # residual distribution
  fpath_plots_residuals_distribution_overall_model <- '../report/plots/residual/model_overall_residual_distribution.jpg'
  fpath_plots_residuals_distribution_tele_model <- '../report/plots/residual/model_televote_residual_distribution.jpg'
  fpath_plots_residuals_distribution_jury_model <- '../report/plots/residual/model_jury_residual_distribution.jpg'
  # residual qq
  fpath_plots_residuals_qq_overall_model <- '../report/plots/residual/model_overall_qq.jpg'
  fpath_plots_residuals_qq_tele_model <- '../report/plots/residual/model_televote_qq.jpg'
  fpath_plots_residuals_qq_jury_model <- '../report/plots/residual/model_jury_qq.jpg'
  # residual spread level
  fpath_plots_residuals_spreadlevel_overall_model <- '../report/plots/residual/model_overall_spreadlevel.jpg'
  fpath_plots_residuals_spreadlevel_tele_model <- '../report/plots/residual/model_televote_spreadlevel.jpg'
  fpath_plots_residuals_spreadlevel_jury_model <- '../report/plots/residual/model_jury_spreadlevel.jpg'
}

# model file paths
fpath_model_overall_final <- NA
fpath_model_televote_final <- NA
fpath_model_jury_final <- NA
# archived models file paths
fpath_model_overall_final <- NA
fpath_model_televote_final <- NA
fpath_model_jury_final <- NA

# if generating output model file paths
if (gen_output_model_files) {
  # model file paths
  fpath_model_overall_final <- '../models/overall_final_model.RDS'
  fpath_model_televote_final <- '../models/televote_final_model.RDS'
  fpath_model_jury_final <- '../models/jury_final_model.RDS'
  # archived models file paths
  fpath_model_arch_overall_final <- '../models/arch/overall_final_model.RDS'
  fpath_model_arch_televote_final <- '../models/arch/televote_final_model.RDS'
  fpath_model_arch_jury_final <- '../models/arch/jury_final_model.RDS'
}

# set file paths for data
fpath_data_processed <- NA
if (gen_output_data_files) {
  fpath_data_processed <- '../data/processed_data.csv'
  
}