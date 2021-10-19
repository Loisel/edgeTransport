#' Apply cleaning steps to GCAM data
#'
#' @param dt input table
#' @return table with
cleanGCAMdata <- function(dt, valcol="value"){
  ## remove underscore from region names
  if("region" %in% colnames(dt)){
    dt[, region := gsub("_", " ", region)]
  }
  if("stub.technology" %in% colnames(dt)){
    setnames(dt, "stub.technology", "technology")
  }
  if("supplysector" %in% colnames(dt)){
    dt[, supplysector := NULL]
  }
  if("tranSubsector" %in% colnames(dt)){
    setnames(dt, "tranSubsector", "vehicle_type_GCAM")
  }
  if("vehicle_type" %in% colnames(dt)){
    dt %>% setnames("vehicle_type", "vehicle_type_GCAM") %>%
      .[!vehicle_type_GCAM %in% c("Heavy Bus", "Light Bus", "Three-Wheeler_tmp_vehicletype", "Truck")]
  }

  if("technology" %in% colnames(dt)){
    ## remove unused techs
    dt <- dt[!technology %in%
             c("Tech-Adv-Electric", "Adv-Electric", "Hybrid Liquids",
               "Tech-Adv-Liquid", "Adv-Liquid", "Coal")
             ]
  }
  ## aggregate vehicle classes
  dt[, (valcol) := as.numeric(get(valcol))]
  logit_techmap <- fread(system.file("extdata", "GCAM_EDGET_vehiclemap.csv", package="edgeTransport"))
  #logit_techmap <- fread("~/git/edgeTransport/inst/extdata/GCAM_EDGET_vehiclemap.csv")
  dt <- logit_techmap[dt, on="vehicle_type_GCAM"][
    is.na(vehicle_type), vehicle_type := vehicle_type_GCAM][
    vehicle_type != "TODEL"]

  if("technology" %in% colnames(dt)){
    dt[, (valcol) := mean(get(valcol), na.rm=TRUE),
       by=c("region", "year", "vehicle_type", "technology")]
  }else{
    dt[, (valcol) := mean(get(valcol), na.rm=TRUE),
       by=c("region", "year", "vehicle_type")]
  }

  dt[, vehicle_type_GCAM := NULL]
  return(unique(dt))
}


#' Cross join the full logit with spatial and temporal dimensions from
#' a given dataset.
#'
#' @param dt input table
#' @return table with
fullDimensionality <- function(dt, logit_structure){
  logit_structure[, k := 1]
  spatio_temp <- unique(dt[, .(k=1, year, region)])
  ## full load factor table based on year and region
  dt_full <- spatio_temp[
    logit_structure, on="k", allow.cartesian=TRUE][, k := NULL]
  logit_structure[, k := NULL]
  return(dt_full)
}


#' Read and prepare GCAM load factor
#'
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING a mapping between ISO3 codes and GCAM region names
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return table with load factors, t/veh for freight, passengers/veh for passenger


lvl0_GCAMloadFactor <- function(input_folder, logit_structure, GCAM2ISO_MAPPING, GCAM_dir = "GCAM"){
  load_factor <- fread(
    file.path(input_folder, GCAM_dir, "L254.StubTranTechLoadFactor.csv"), skip=4, header = T) %>%
    cleanGCAMdata(valcol="loadFactor") %>%
    .[, technology := NULL]

  load_factor_full <- fullDimensionality(load_factor, logit_structure)

  ## we end up with a full table (all logit categories covered for all regions)
  load_factor_full[load_factor, loadFactor := i.loadFactor, on=colnames(load_factor)[1:3]]

  ## now fill up NAs, we use averages accross regions for missing values
  ## load_factor[, loadFactor := ifelse(is.na(loadFactor),
  ##                                    mean(loadFactor, na.rm=TRUE),
  ##                                    loadFactor),
  ##             by=c("year", "vehicle_type")]

  ## bikes and walk
  load_factor_full[vehicle_type %in% c("Cycle", "Walk"), loadFactor := 1]

  ## uniqueness checks
  stopifnot(!any(duplicated(load_factor_full)))

  return(
    disaggregate_dt(load_factor_full, GCAM2ISO_MAPPING) %>%
    setnames("loadFactor", "loadfactor|pt/v"))

}

#' Read and prepare GCAM vehicle intensity
#'
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING a mapping between ISO3 codes and GCAM region names
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return table with vehicle intensity, MJ/vkm


lvl0_GCAMvehIntensity <- function(input_folder, logit_structure, GCAM2ISO_MAPPING, GCAM_dir = "GCAM"){
  CONV_MJ_btu = 947.777
  intensity <- fread(
    file.path(input_folder, GCAM_dir, "L254.StubTranTechCoef.csv"), skip=4, header = T) %>%
    cleanGCAMdata(valcol="coefficient") %>%
    .[, .(region, year, vehicle_type, technology,
          `intensity|MJ/vkm`=coefficient/CONV_MJ_btu)]

  intensity_full <- fullDimensionality(intensity, logit_structure)

  ## we end up with a full table (all logit categories covered for all regions)
  intensity_full[intensity, `intensity|MJ/vkm` := `i.intensity|MJ/vkm`, on=colnames(intensity)[1:4]]

  ## uniqueness checks
  stopifnot(!any(duplicated(intensity_full)))

  return(disaggregate_dt(intensity_full, GCAM2ISO_MAPPING))
}


#' Read and prepare historic (until 2010) ES demand from GCAM
#'
#' @param input_folder folder hosting raw data
#' @param logit_structure full logit tree structure
#' @param GCAM2ISO_MAPPING a mapping between ISO3 codes and GCAM region names
#' @param GDP_country country-wise GDP PPP data
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return table with ES demand, unit: million tkm/pkm

lvl0_GCAMhistoricESdemand <- function(input_folder, logit_structure, GCAM2ISO_MAPPING,
                                      GDP_country, GCAM_dir = "GCAM"){
  demand <- fread(
    file.path(input_folder, GCAM_dir, "tech_output.csv"), skip = 1, sep=";", header = T) %>%
    melt(measure.vars=6:8, value.name="tech_output", variable.name = "year") %>%
    .[, .(region, year=as.numeric(as.character(year)), vehicle_type=subsector,
          technology, `demand|million tpkm/a`=tech_output)] %>%
    cleanGCAMdata(valcol="demand|million tpkm/a")

  demand_full <- fullDimensionality(demand, logit_structure)

  ## we end up with a full table (all logit categories covered for all regions)
  demand_full[demand, `demand|million tpkm/a` := `i.demand|million tpkm/a`, on=colnames(demand)[1:4]]

  ## uniqueness checks
  stopifnot(!any(duplicated(demand_full)))

  ## to ISO
  demand_iso <- disaggregate_dt(
    demand_full, GCAM2ISO_MAPPING,
    fewcol = "region", manycol = "iso", valuecol = "demand|million tpkm/a",
    datacols=c("sector", "subsector_L1", "subsector_L2",
               "subsector_L3", "vehicle_type", "technology"),
    weights = GDP_country, weightcol = "weight"
  )

  return(demand_iso)
}


#' Read and prepare GCAM speed index
#'
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING a mapping between ISO3 codes and GCAM region names
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return table with speed index, dimensionless

lvl0_GCAMspeed <- function(input_folder, logit_structure, GCAM2ISO_MAPPING, GCAM_dir = "GCAM"){
  speed_mot = fread(file.path(input_folder, GCAM_dir, "L254.tranSubsectorSpeed.csv"), skip=4) %>%
    cleanGCAMdata(valcol="speed")
  speed_full <- fullDimensionality(speed_mot, logit_structure)
  speed_full[speed_mot, speed := i.speed, on=colnames(speed_mot)[1:3]]

  ## speed non-motorized
  speed_not_mot = fread(file.path(input_folder, GCAM_dir, "A54.globaltech_nonmotor.csv"),
                        skip=1, header = T) %>%
    .[, .(vehicle_type=tranSubsector, speed)]

  speed_full[speed_not_mot, speed := i.speed, on="vehicle_type"]

  stopifnot(!any(duplicated(speed_full)))

  speed_full <- speed_full[sector == "trn_pass"]
  ## now fill up NAs, we use averages accross regions for missing values
  speed_full[, speed := ifelse(is.na(speed),
                               mean(speed, na.rm=TRUE),
                               speed),
             by=c("year", "vehicle_type")]

  ## Apply convergence in time to the fastest vehicle across regions
  speed_full[, maxspeed := max(speed[year == 2100]), by = "vehicle_type"]
  speed_full[year >= 2020,
             speed := speed[year == 2020]*(2100-year)/(2100-2020) + maxspeed*(year-2020)/(2100-2020),
             by =c("vehicle_type", "region")]


  return(disaggregate_dt(speed_full, GCAM2ISO_MAPPING))
}


#' Read and prepare GCAM value-of-time
#'
#' @param input_folder folder hosting raw data
#' @param GCAM2ISO_MAPPING a mapping between ISO3 codes and GCAM region names
#' @param GCAM_dir subdirectory within the data input with GCAM data
#' @return table with value-of-time, in relation to GDP MER

lvl0_GCAMvalueOfTime <- function(input_folder, logit_structure, GCAM2ISO_MAPPING,
                                 GDP_POP_MER_country, GCAM_dir = "GCAM"){
  vott <- fread(file.path(input_folder, GCAM_dir, "A54.tranSubsector_VOTT.csv"), skip = 1) %>%
    .[, .(vehicle_type_GCAM=tranSubsector, vot=time.value.multiplier)] %>%
    .[!vehicle_type_GCAM %in% c("Heavy Bus", "Light Bus", "Three-Wheeler_tmp_vehicletype", "Truck")]

  logit_techmap <- fread("~/git/edgeTransport/inst/extdata/GCAM_EDGET_vehiclemap.csv")
  vott <- logit_techmap[vott, on="vehicle_type_GCAM"][
    is.na(vehicle_type), vehicle_type := vehicle_type_GCAM][
    vehicle_type != "TODEL"]

  vott <- vott[!is.na(vot)] %>%
    .[, .(vot= mean(vot, na.rm=TRUE)),
      by=c("vehicle_type")]

  stopifnot(!any(duplicated(vott)))

  speed <- lvl0_GCAMspeed(input_folder, logit_structure, GCAM2ISO_MAPPING)

  vott <- vott[speed, on="vehicle_type"] %>%
    merge(GDP_POP_MER_country, by = c("iso", "year"))

  WEEKS_PER_YEAR = 50
  HOURS_PER_WEEK = 40
  vott[, `valueOfTime|2005$` := GDP_cap                             ## [2005$/person/year]
                       *vot                                ## [2005$/person/year]
                       /(HOURS_PER_WEEK* WEEKS_PER_YEAR)/  ## [2005$/h]
                       speed]                              ## [2005$/km]

  return(vott[,.(iso, year, sector, subsector_L3, subsector_L2, subsector_L1,
                     technology, `valueOfTime|2005$`)])

}


#' Load value-of-time and exponents
#'
#' load logit exponents for each level: they are based on GCAM assumptions.
#' They are on csv files that already follow the EDGE structure, created by hand.
#'
#' VOT values in (1990$/pkm)
#' @param GCAM_data GCAM based data
#' @param GDP_MER_country GDP iso level MER
#' @param POP_country population (ISO level)
#' @param input_folder folder hosting raw data
#' @param logitexp_dir directory with logit exponents for GCAM
#' @importFrom rmndt aggregate_dt
#'


lvl0_VOTandExponents <- function(GCAM_data, GDP_MER_country, POP_country, input_folder, logitexp_dir="GCAM_logit_exponents"){
  sector <- logit.exponent <- value <-  region <- ISO3 <- `.` <- time <- Year <- Value <- time_price <- GDP_cap <- time.value.multiplier <- tranSubsector <- supplysector <- univocal_name <- speed_conv <- year_at_yearconv <- yearconv <-weight <- GDP <- speed_trend <- POP_val <- NULL
  loadFactor <- loadFactor_conv <- loadFactor_trend <- subsector_L1 <- subsector_L2 <- speed <- iso <- NULL
  subsector_L3 <- technology <- vehicle_type <- NULL
  exp_folder = function(fname){
    file.path(input_folder, logitexp_dir, fname)
  }

  GDP_POP = merge(GDP_MER_country, POP_country, by = c("iso", "year"))

  GDP_POP_cap = GDP_POP[,GDP_cap := weight/value]

  logit_exponent_FV = fread(exp_folder("FV_logitexponent.csv"))
  logit_exponent_VS1 = fread(exp_folder("VS1_logitexponent.csv"))
  logit_exponent_S1S2 = fread(exp_folder("S1S2_logitexponent.csv"))
  logit_exponent_S2S3 = fread(exp_folder("S2S3_logitexponent.csv"))
  logit_exponent_S3S = fread(exp_folder("S3S_logitexponent.csv"))


  ## Ceate level specific VOT data tables

  ## FV

  value_of_time_LDV = value_of_time[supplysector %in% c("trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W"),
                                    .(vehicle_type = tranSubsector, subsector_L1 = supplysector, iso, year, time_price)]

  value_time_FV = value_of_time_LDV[!is.na(time_price),]

  ## VS1

  value_time_VS1 = data.table(subsector_L1 = character(), vehicle_type = character(), iso = character(), year = numeric(), time_price = numeric())

  ## S1S2

  value_time_S1S2 = data.table(subsector_L2 = character(), subsector_L1 = character(), iso = character(), year = numeric(), time_price = numeric())

  ## S2S3

  value_time_S2S3 = value_of_time[tranSubsector == "Bus",
                                 .(subsector_L3 = supplysector, subsector_L2 = tranSubsector, iso, year, time_price)]

  ## S3S

  value_time_S3S = value_of_time[tranSubsector %in% c("International Aviation", "Passenger Rail", "Domestic Aviation", "HSR"),
                                 .(sector=supplysector, subsector_L3 = tranSubsector, iso, year, time_price)]

  price_nonmot = value_of_time[tranSubsector %in% c("Cycle", "Walk"),
                              .(sector = supplysector, subsector_L3 = tranSubsector, iso, year, tot_price = time_price)]

  price_nonmot[, subsector_L2 := paste0(subsector_L3, "_tmp_subsector_L2")]
  price_nonmot[, subsector_L1 := paste0(subsector_L3, "_tmp_subsector_L1")]
  price_nonmot[, vehicle_type := paste0(subsector_L3, "_tmp_vehicletype")]
  price_nonmot[, technology := paste0(subsector_L3, "_tmp_technology")]

  VOT_output = list(value_time_FV = value_time_FV,
                    value_time_VS1 = value_time_VS1,
                    value_time_S1S2 = value_time_S1S2,
                    value_time_S2S3 = value_time_S2S3,
                    value_time_S3S = value_time_S3S)

  logit_output = list(logit_exponent_FV = logit_exponent_FV,
                      logit_exponent_VS1 = logit_exponent_VS1,
                      logit_exponent_S1S2 = logit_exponent_S1S2,
                      logit_exponent_S2S3 = logit_exponent_S2S3,
                      logit_exponent_S3S = logit_exponent_S3S)

  result = list(VOT_output = VOT_output,
                logit_output = logit_output,
                price_nonmot = price_nonmot)

  return(result)

}
