#' Extract Predictors by Category
#' 
#' @description Generates predictor columns for a give category
#' 
#' @param cat A category of predictors to generate; one of "competition", "performance" or "external"
#' 
#' @return Returns the predictor variables as a vector
#' 
extract_preds_by_cats <- function(cat = c("competition", "performance", "external")){
  if (cat == "competition"){
    cat_preds <- c( 
      "Round_f", "Round_sf1", "Voting_Method_J", "Host_Nation_y", "Average_Points", "OOA"
      )
  }
  if (cat == "performance"){
    cat_preds <- c(
      "TC_PerfType_Group", "TC_PerfType_Mixed", "TC_PerfType_Solo", "TC_SingerGender_Female", "TC_SingerGender_Male", 
      "FC_SONGLANG_English", "TC_SONGLANG_Bosnian", "ComSONGLAN", "key_0", "key_2", "key_6", "key_8", "mode_1", 
      "time_signature_4", "danceability", "energy", "loudiness", "speechiness", "acousticness", "instrumentalness", 
      "liveness", "valence", "tempo", "duration_ms"
      )
  }
  if (cat == "external"){
      cat_preds <- c(
        "VBlocs1_FC_1", "VBlocs2_FC_1", "VBlocs1_TC_1", "VBlocs1_TC_8", "VBlocs1_TC_9", "VBlocs1_TC_13", 
        "VBlocs2_TC_1", "ComVBlocs1_y", "ComVBlocs2_y", "FC_LANGFAM_Baltic", "TC_LANGFAM_Armenian",
        "TC_LANGFAM_Baltic", "ComLANGFAM_y","Neighbours_y", "TC_NumNeigh", "FC_NonCOB","FC_NonCitzens", 
        "METRIC_Citizens","CAP_DIST_km"
        )
  }
  return(cat_preds)
}