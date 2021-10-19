#' Read and adjust the logit structure
#'
#' @param input_folder folder hosting raw data
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return a data.table with the structure of the decision tree


lvl0_fullLogitStructure <- function(input_folder, GCAM_dir = "GCAM"){
  vehicle_type <- technology <- vehicle_type_GCAM <- NULL
  #load logit structure
  logit_category <- fread(file.path(input_folder, GCAM_dir, "logit_categories.csv"))[, vehicle_type := NULL]
  ## remove the Adv categories, Hybrid Liquids and LA_BEV
  logit_category = logit_category[!technology %in% c("Tech-Adv-Electric", "Adv-Electric", "Hybrid Liquids", "Tech-Adv-Liquid", "Adv-Liquid")]
  ## remove coal
  logit_category <- logit_category[technology != "Coal"]

  setnames(logit_category, "univocal_name", "vehicle_type_GCAM")
  logit_techmap <- fread(system.file("extdata", "GCAM_EDGET_vehiclemap.csv", package="edgeTransport"))
  #logit_techmap <- fread("~/git/edgeTransport/inst/extdata/GCAM_EDGET_vehiclemap.csv")
  logit_category <- logit_techmap[logit_category, on="vehicle_type_GCAM"]
  logit_category[is.na(vehicle_type), vehicle_type := vehicle_type_GCAM]
  logit_category <- logit_category[vehicle_type != "TODEL"]
  logit_category[, vehicle_type_GCAM := NULL]
  logit_category <- unique(logit_category)

  logit_category <- rbindlist(list(
    logit_category,
    logit_category[technology == "BEV"][, technology := "Hybrid Electric"],
    ## alternative busses
    logit_category[vehicle_type == "Bus" & technology == "Liquids"][, technology := "Electric"],
    logit_category[vehicle_type == "Bus" & technology == "Liquids"][, technology := "FCEV"],
    ## alternative trucks
    logit_category[grepl("^Truck", vehicle_type) & technology == "Liquids"][, technology := "Electric"],
    logit_category[grepl("^Truck", vehicle_type) & technology == "Liquids"][, technology := "FCEV"]))

  ## fix cycling and walking logit categories
  logit_category[
    vehicle_type %in% c("Walk", "Cycle"),
    `:=`(subsector_L3="trn_pass_road", subsector_L2="trn_pass_road_nonmot",
         subsector_L1=paste0("trn_pass_road_nonmot_tmp_subsector_L1"))]
  return(logit_category)
}
