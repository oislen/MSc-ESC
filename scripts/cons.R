gen_output_report_stat_files <- FALSE
gen_output_report_plot_files <- FALSE
gen_output_model_files <- FALSE
gen_output_model_arch_files <- FALSE
gen_output_data_files <-FALSE

#-- Data --#

# set file paths for data
fpath_data_voting_bloc <- "../data/voting_bloc_data.csv"
fpath_data_average_points <- "../data/average_points.csv"
fpath_data_processed <- '../data/processed_data.csv'

#-- Stats --#

fpath_stats_cor_predictors <- "../report/Stats/cor_tests_predictors.csv"
fpath_stats_cor_response <- "../report/stats/cor_tests_response.csv"
fpath_stats_chisq_response <-"../report/stats/chi_sq_tests_response.csv"
fpath_stats_descriptive_numeric <- "../report/stats/numeric_descriptive_statistics.csv"
fpath_stats_descriptive_categorical <- "../report/stats/categorical_descriptive_statistics.csv"
fpath_stats_na_prop_per_columns <- '../report/stats/NA_prop_per_columns.csv'
fpath_stats_missing_oberations <- "../report/stats/TC_FC_missing_obseration_df.csv"

#-- Models --#

# model file paths
fpath_model_overall_final <- '../models/overall_final_model.RDS'
fpath_model_televote_final <- '../models/televote_final_model.RDS'
fpath_model_jury_final <- '../models/jury_final_model.RDS'

# archived models file paths
fpath_model_arch_overall_final <- '../models/arch/overall_final_model.RDS'
fpath_model_arch_televote_final <- '../models/arch/televote_final_model.RDS'
fpath_model_arch_jury_final <- '../models/arch/jury_final_model.RDS'

#-- Plots --#

# missing values directory
fdir_plots_missings <- NA
fdir_plots_bar_charts <- NA
fdir_plots_histograns <- NA
fdir_plots_scatterplots <- NA
# dendrograms
fpath_plots_dendrogram_edge_betweenness <- NA
fpath_plots_dendrogram_short_random_walks <- NA
# networks
fpath_plots_networks_cob_metric_televote <- NA
fpath_plots_networks_cob_metric_jury <- NA
fpath_plots_networks_citizens_metric_televote <- NA
fpath_plots_networks_citizens_metric_jury <- NA
fpath_plots_networks_cobcit_metric_televote <- NA
fpath_plots_networks_cobcit_metric_jury <- NA
fpath_plots_networks_cobcit_sf1_metric_televote <- NA
fpath_plots_networks_cobcit_sf1_metric_jury <- NA
fpath_plots_networks_cap_distance_televote <- NA
fpath_plots_networks_cap_distance_jury <- NA
fpath_plots_networks_average_points_televote <- NA
fpath_plots_networks_average_points_jury <- NA
fpath_plots_networks_average_points_graph <- NA
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
if (gen_output_report_plot_files) {
  # missing values directory
  fdir_plots_missings <- '../report/plots/missings/'
  fdir_plots_bar_charts <- '../report/plots/bar_charts'
  fdir_plots_histograns <- '../report/plots/histograms'
  fdir_plots_scatterplots <- '../report/plots/scatterplots'
  # dendrograms
  fpath_plots_dendrogram_edge_betweenness <- '../report/plots/dendrograms/edge-betweenness-dendrogram.jpg'
  fpath_plots_dendrogram_short_random_walks <- '../report/plots/dendrograms/short-random-walks-dendrogram.jpg'
  # networks
  fpath_plots_networks_cob_metric_televote <- '../report/plots/networks/televote-metric-cob.jpg'
  fpath_plots_networks_cob_metric_jury <- '../report/plots/networks/jury-metric-cob.jpg'
  fpath_plots_networks_citizens_metric_televote <- '../report/plots/networks/televote-metric-citizens.jpg'
  fpath_plots_networks_citizens_metric_jury <- '../report/plots/networks/jury-metric-citizens.jpg'
  fpath_plots_networks_cobcit_metric_televote <- '../report/plots/networks/televote-metric-cobcit.jpg'
  fpath_plots_networks_cobcit_metric_jury <- '../report/plots/networks/jury-metric-cobcit.jpg'
  fpath_plots_networks_cobcit_sf1_metric_televote <- '../report/plots/networks/televote-metric-cobcit-sf1.jpg'
  fpath_plots_networks_cobcit_sf1_metric_jury <- '../report/plots/networks/jury-metric-cobcit-sf1.jpg'
  fpath_plots_networks_cap_distance_televote <- '../report/plots/networks/televote-cap-dist.jpg'
  fpath_plots_networks_cap_distance_jury <- '../report/plots/networks/jury-cap-dist.jpg'
  fpath_plots_networks_average_points_televote <- '../report/plots/networks/televote-average-points.jpg'
  fpath_plots_networks_average_points_jury <- '../report/plots/networks/jury-average-points.jpg'
  fpath_plots_networks_average_points_graph <- '../report/plots/networks/average-points-graph.jpg'
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

