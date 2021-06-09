TC_FC_missing_observations <- function(from_countries, orig_data, comp_data, digits = 2){
  # NOTE: Observe how these missing observations effect the To_country and From_country
  # Remember all the migration factors are intrinsically linked to to both
  # From_country and two_country
  # define the output results
  TC_FC_results_cols <- c("Country", "FC - Inc. Miss Obs.", "FC - Exc. Miss Obs.", "FC - Miss Obs. %", "TC - Inc. Miss Obs.", "TC - Exc. Miss Obs.", "TC - Miss Obsv. %")
  # create an empty data frame to hold the results
  TC_FC_df <- as.data.frame(matrix(nrow = length(from_countries), ncol = length(TC_FC_results_cols), dimnames = list(from_countries, TC_FC_results_cols)))
  
  # Use a for loop to fill in the data frame
  for(col in from_countries){
    # Fill in the Country Column
    TC_FC_df[col, "Country"] <- col
    # Fill in the FC-Inc. Column
    TC_FC_df[col, "FC - Inc. Miss Obs."] <- summary(as.factor(orig_data$From_country))[col]
    # Fill in the FC-Exc. Column
    TC_FC_df[col, "FC - Exc. Miss Obs."] <- summary(as.factor(comp_data$From_country))[col]
    # Fill in the FC-Missing Prct
    miss_prop <- TC_FC_df[col, "FC - Inc. Miss Obs."] / TC_FC_df[col, "FC - Exc. Miss Obs."]
    TC_FC_df[col, "FC - Miss Obs. %"] <- round((1 - miss_prop) * 100, digits = digits)
    # Fill in the TC-Inc. Column
    TC_FC_df[col, "TC - Inc. Miss Obs."] <- summary(as.factor(orig_data$To_country))[col]
    # Fill in the TC-Exc. Column
    TC_FC_df[col, "TC - Exc. Miss Obs."] <- summary(as.factor(comp_data$To_country))[col]
    # Fill in the TC-Missing Prct
    miss_prop <- TC_FC_df[col, "TC - Inc. Miss Obs."] / TC_FC_df[col, "TC - Exc. Miss Obs."] 
    TC_FC_df[col, "TC - Miss Obsv. %"] <- round((1 - miss_prop) * 100, digits = 2)
  }           
  return(TC_FC_df)
}