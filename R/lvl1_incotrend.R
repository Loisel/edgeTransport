#' Calculate a trend for share weights and inconvenience costs based on the EDGE scenario
#'
#' @param SWS preference factors
#' @param preftab mode and veh preferences table
#' @param calibdem calibration demand
#' @param incocost inconvenience costs for 4wheelers
#' @param years time steps
#' @param GDP_POP_MER GDP population on MER base
#' @param smartlifestyle switch activating sustainable lifestyles
#' @param tech_scen technology at the center of the policy packages
#' @param SSP_scen SSP or SDP scenario
#' @param mitab.path mitigation pathways table path
#'
#' @importFrom zoo na.approx na.spline
#' @return projected trend of preference factors
#' @author Alois Dirnaichner, Marianna Rottoli


lvl1_preftrend <- function(SWS, preftab, calibdem, incocost, years, GDP_POP_MER,
                           smartlifestyle, tech_scen, SSP_scen, mitab.path=NULL){
  subsector_L1 <- gdp_pop <- technology <- tot_price <- sw <- logit.exponent <- NULL
  logit_type <- `.` <- region <- vehicle_type <- subsector_L2 <- subsector_L3 <- NULL
  sector <- V1 <- tech_output <- V2 <- GDP_cap <- value <- convsymmBEVlongDist <- NULL
  SSP_scenario <- level <- i.sw <- approx <- tech_scenario <- techvar <- regioncat <- NULL
  vehvar <- target <- symmyr <- speed <- FV_techvar <- NULL


  ## function that extrapolate constant values
  filldt <- function(dt, proxy){
    yrs_toadd <- setdiff(years, unique(dt$year))
    for (yr in yrs_toadd) {
      tmp_dt <- dt[year == proxy]
      tmp_dt[, year := yr]
      tmp_dt[, sw := NA]
      dt <- rbind(dt, tmp_dt)
    }
    return(dt)
  }

  ## load pref table
  if(is.null(preftab)){
    preftab <- system.file("extdata", "sw_trends.csv", package = "edgeTransport")
  }
  ptab <- fread(preftab, header=T)[SSP_scenario == SSP_scen][, SSP_scenario := NULL]

  ptab <- melt(ptab, value.name = "sw", variable.name = "year", id.vars = colnames(ptab)[1:9])
  ptab[, year := as.numeric(as.character(year))]
  ## add missing years
  ptab <- filldt(ptab, 2020)

  ## merge tech prefs
  FVdt <- SWS$FV_final_SW
  ## random fixings for missing demand data
  FVdt <- rbindlist(list(
    FVdt,
    FVdt[region == "CHA" & year <= 2010 & vehicle_type == "Motorcycle (50-250cc)"][
      , vehicle_type := "Motorcycle (>250cc)"],
    FVdt[region == "IND" & year <= 2010 & vehicle_type == "Motorcycle (50-250cc)"][
      , vehicle_type := "Motorcycle (>250cc)"],
    FVdt[region == "IND" & year <= 2010 & vehicle_type == "Truck (18t)"][
      , vehicle_type := "Truck (26t)"],
    FVdt[region == "IND" & year <= 2010 & vehicle_type == "Truck (18t)"][
      , vehicle_type := "Truck (40t)"],
    FVdt[region == "JPN" & year <= 2010 & vehicle_type == "Truck (7.5t)"][
      , vehicle_type := "Truck (18t)"],
    FVdt[region == "JPN" & year <= 2010 & vehicle_type == "Truck (7.5t)"][
      , vehicle_type := "Truck (26t)"],
    FVdt[region == "JPN" & year <= 2010 & vehicle_type == "Truck (7.5t)"][
      , vehicle_type := "Truck (40t)"],
    FVdt[region == "MEA" & year <= 2010 & vehicle_type == "Truck (18t)"][
      , vehicle_type := "Truck (26t)"],
    FVdt[region == "MEA" & year <= 2010 & vehicle_type == "Truck (18t)"][
      , vehicle_type := "Truck (40t)"],
    FVdt[region == "MEA" & year <= 2010 & vehicle_type == "Motorcycle (50-250cc)"][
      , vehicle_type := "Motorcycle (>250cc)"],
    FVdt[region == "MEA" & year <= 2010 & vehicle_type == "Motorcycle (50-250cc)"][
      , vehicle_type := "Moped"],
    FVdt[region == "USA" & year <= 2010 & vehicle_type == "Motorcycle (>250cc)"][
      , vehicle_type := "Motorcycle (50-250cc)"],
    FVdt[region == "USA" & year <= 2010 & vehicle_type == "Motorcycle (>250cc)"][
      , vehicle_type := "Moped"]
  ))

  FVtarget <- ptab[level == "FV"]
  ## insert historical values
  FVtarget[FVdt, sw := i.sw, on=c("region", "year", "vehicle_type", "technology")]
  FVtarget[year <= 2010 & is.na(sw), sw := 0]
  FVtarget[subsector_L3 == "HSR", sw := 1]
  tmps <- filldt(FVdt[grepl("_tmp_", technology)], 2010)[
    , `:=`(sw=1, level="FV", approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  ## merge placeholder
  FVtarget <- rbind(FVtarget, tmps)

  FVtarget[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "vehicle_type", "technology")]
  FVtarget[sw < 0, sw := 0]

  ## introduces NA for sw == 0
  FVtarget[, sw := sw/max(sw),
           by = c("region", "year", "vehicle_type")]
  nas <- FVtarget[is.na(sw)]
  if(nrow(nas) > 0){
    print("Warning: NAs in SWs found.")
    browser()
  }

  setnames(FVtarget, "sw", "value")
  FVtarget[, logit_type := "sw"]
  FVtarget[, c("techscen", "level", "approx") := NULL]

  ## merge with incocost, this should be moved elsewhere in the future
  FV_inco = FVdt[subsector_L1 == "trn_pass_road_LDV_4W" & technology == "Liquids" & year <= 2020]
  FV_inco[, value := tot_price*(sw^(1/logit.exponent)-1)]
  FV_inco[, logit_type := "pinco_tot"]
  FV_inco = FV_inco[,.(region,year,technology,vehicle_type,subsector_L1,subsector_L2,subsector_L3,sector,logit_type, value)]
  ## add also values for 2015 and 2020 for Liquids, as the other technologies have them
  FV_inco = rbind(FV_inco, FV_inco[year == 2010][, year := 2015], FV_inco[year == 2010][, year := 2020])
  ## merge to the "normally calculated" pinco, and create the output at this level
  incocost = merge(incocost, unique(FV_inco[,c("region", "vehicle_type")]), all = FALSE)
  FVtarget = rbind(FVtarget, FV_inco, incocost)

  ## merge size prefs
  VSdt <- SWS$VS1_final_SW

  VStarget <- ptab[level == "VS1"]
  VStarget[, technology := NULL]
  ## insert historical values
  VStarget[VSdt, sw := i.sw, on=c("region", "year", "sector", "subsector_L1",
                                  "subsector_L2", "subsector_L3", "vehicle_type")]
  VStarget[year <= 2010 & is.na(sw), sw := 0]

  ## merge placeholder
  tmps <- filldt(VSdt[grepl("_tmp_", vehicle_type)], 2010)[
    , `:=`(sw=1, level="VS1", techscen=unique(VStarget$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  ## add missing placeholders (HSR and rail)
  tmps <- unique(
    rbind(
      tmps, rbindlist(
              lapply(c("IND", "MEA", "REF", "ECE"),
                     function(reg){
                       tmps[region == "JPN"][, region := reg]
                     }))))
  VStarget <- rbind(VStarget, tmps)

  VStarget[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector", "subsector_L1",
                "subsector_L2", "subsector_L3", "vehicle_type")]
  VStarget[sw < 0, sw := 0]
  VStarget[, c("techscen", "level", "approx") := NULL]

  ## merge L1 sws (4W vs 2W)
  S1dt <- SWS$S1S2_final_SW

  S1target <- ptab[level == "S1S2"]
  S1target[, c("technology", "vehicle_type") := NULL]
  ## insert historical values
  S1target[S1dt, sw := i.sw, on=c("region", "year", "sector", "subsector_L1",
                                  "subsector_L2", "subsector_L3")]
  S1target[year <= 2010 & is.na(sw), sw := 0]

  ## merge placeholder
  tmps <- filldt(S1dt[grepl("_tmp_", subsector_L1)], 2010)[
    , `:=`(sw=1, level="S1S2", techscen=unique(S1target$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  ## add missing placeholders (HSR and rail)
  tmps <- unique(
    rbind(
      tmps, rbindlist(
              lapply(c("IND", "MEA", "REF", "ECE"),
                     function(reg){
                       tmps[region == "JPN"][, region := reg]
                     }))))
  S1target <- rbind(S1target, tmps)

  S1target[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector", "subsector_L1",
                "subsector_L2", "subsector_L3")]
  S1target[sw < 0, sw := 0]
  S1target[, c("techscen", "level", "approx") := NULL]

  ## merge L2 sws
  S2dt <- SWS$S2S3_final_SW

  S2target <- ptab[level == "S2S3"]
  S2target[, c("technology", "vehicle_type", "subsector_L1") := NULL]
  ## insert historical values
  S2target[S2dt, sw := i.sw, on=c("region", "year", "sector",
                                  "subsector_L2", "subsector_L3")]
  S2target[year <= 2010 & is.na(sw), sw := 0]

  ## merge placeholder
  tmps <- filldt(S2dt[grepl("_tmp_", subsector_L2)], 2010)[
    , `:=`(sw=1, level="S2S3", techscen=unique(S2target$techscen), approx="linear")]
  tmps[, c("logit.exponent", "tot_price") := NULL]
  tmps <- unique(
    rbind(
      tmps, rbindlist(
              lapply(c("IND", "MEA", "REF", "ECE"),
                     function(reg){
                       tmps[region == "JPN"][, region := reg]
                     }))))
  S2target <- rbind(S2target, tmps)

  S2target[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector",
                "subsector_L2", "subsector_L3")]
  S2target[sw < 0, sw := 0]
  S2target[, c("techscen", "level", "approx") := NULL]

  ## merge L3 sws
  S3dt <- SWS$S3S_final_SW

  S3target <- ptab[level == "S3S"]
  S3target[, c("technology", "vehicle_type", "subsector_L1", "subsector_L2") := NULL]
  ## insert historical values
  S3target[S3dt, sw := i.sw, on=c("region", "year", "sector", "subsector_L3")]
  S3target[year <= 2010 & is.na(sw), sw := 0]

  ## no placeholder on S3
  S3target[, sw := ifelse(approx == "spline", na.spline(sw, x = year), na.approx(sw, x = year)),
           by=c("region", "sector", "subsector_L3")]
  S3target[sw < 0, sw := 0]
  S3target[, c("techscen", "level", "approx") := NULL]

  ## normalization
  S3target[, sw := sw/max(sw),
           by = c("region", "year", "sector")]
  nas <- S3target[is.na(sw)]
  if(nrow(nas) > 0){
    print("Warning: NAs in SWs found.")
    browser()
  }


  S2target[, sw := sw/max(sw),
           by = c("region", "year", "subsector_L3")]
  nas <- S2target[is.na(sw)]
  if(nrow(nas) > 0){
    print("Warning: NAs in SWs found.")
    browser()
  }

  S1target[, sw := sw/max(sw),
           by = c("region", "year", "subsector_L2")]
  nas <- S1target[is.na(sw)]
  if(nrow(nas) > 0){
    print("Warning: NAs in SWs found.")
    browser()
  }

  VStarget[, sw := sw/max(sw),
           by = c("region", "year", "subsector_L1")]
  nas <- VStarget[is.na(sw)]
  if(nrow(nas) > 0){
    print("Warning: NAs in SWs found.")
    browser()
  }


  ## Adjust tech mixes
  apply_logistic_trends <- function(yrs, final, ysymm, speed, initial = 1){
    fct <- exp((yrs - ysymm)/speed)/(exp((yrs - ysymm)/speed) + 1)
    initial + fct * (final - initial)
  }

  ## mitab <- CJ(SSP_scenario=c("SSP1", "SSP2", "SSP5", "SSP2EU", "SDP"),
  ##    tech_scenario=c("Mix2", "Mix3", "Mix4"),
  ##    regioncat=c("rich", "poor"),
  ##    vehvar=c("Truck|heavy", "Truck|light", "Bus", "Aviation", "Ship", "LDV|2W", "Rail"),
  ##    techvar=c("Liquids", "Electric", "Hydrogen"),
  ##    target=1, symmyr=2050, speed=10)
  ## fwrite(mitab, "edget-mitigation.csv")
  if(is.null(mitab.path)){
      mitab.path <- system.file("extdata", "edget-mitigation.csv", package="edgeTransport")
    }
  mitab <- fread(mitab.path, header = TRUE, check.names = TRUE)[
    SSP_scenario == SSP_scen & tech_scenario == tech_scen]
  if(nrow(mitab) > 0){
    ## treat a region as a rich region starting from:
    richcutoff <- 25000
    mimap <- system.file("extdata", "mitigation-techmap.csv", package="edgeTransport")
    techmap <- fread(text="technology,FV_techvar
FCEV,Hydrogen
BEV,Electric
NG,Liquids
Hybrid Electric,Liquids")
    mimap <- fread(system.file("extdata", "mitigation-techmap.csv", package="edgeTransport"))
    FVtarget <- mimap[FVtarget, on="vehicle_type"]
    FVtarget <- techmap[FVtarget, on="technology"]
    FVtarget[is.na(FV_techvar), FV_techvar := technology]

    richregions <- unique(unique(GDP_POP_MER[year == 2010 & GDP_cap > richcutoff, region]))
    FVtarget[, regioncat := ifelse(region %in% richregions, "rich", "poor")]

    ## remove L2 and L3 from mitab to avoid a join on these sectors
    FVtarget <- mitab[level == "FV"][, c("subsector_L2", "subsector_L3") := NULL][FVtarget, on=c("FV_vehvar", "FV_techvar", "regioncat")]

    FVtarget[, value := ifelse(
                 is.na(target), value, apply_logistic_trends(year, target, symmyr, speed) * value),
             by=c("region", "vehicle_type", "technology")]
    cname_to_remove <- colnames(mitab)[!grepl("subsector_", colnames(mitab))]
    FVtarget[, (cname_to_remove) := NULL]
    FVtarget[logit_type == "sw", value := value/max(value),
             by = c("region", "year", "vehicle_type")]
    nas <- FVtarget[logit_type != "pchar" & is.na(value)]
    if(nrow(nas) > 0){
      print(sprintf("NAs found in FV shareweight trends for %s scenario.", tech_scen))
      browser()
    }

    ## level S2: Bus vs LDV
    S2target[, regioncat := ifelse(region %in% richregions, "rich", "poor")]
    ## remove L3 from mitab to avoid a join on these sectors
    S2target <- mitab[level == "S2"][, subsector_L3 := NULL][
      S2target, on=c("subsector_L2", "regioncat")]
    S2target[, sw := ifelse(
                 is.na(target), sw, apply_logistic_trends(year, target, symmyr, speed) * sw),
             by=c("region", "subsector_L2")]
    S2target[, (cname_to_remove) := NULL]
    S2target[, sw := sw/max(sw),
             by = c("region", "year", "subsector_L3")]
    nas <- S2target[is.na(sw)]
    if(nrow(nas) > 0){
      print(sprintf("NAs found in S2 shareweight trends for %s scenario.", tech_scen))
      browser()
    }

    ## level S3: all other mode shares
    S3target[, regioncat := ifelse(region %in% richregions, "rich", "poor")]
    ## remove L3 from mitab to avoid a join on these sectors
    S3target <- mitab[level == "S3"][, subsector_L2 := NULL][
      S3target, on=c("subsector_L3", "regioncat")]
    S3target[, sw := ifelse(
                 is.na(target), sw, apply_logistic_trends(year, target, symmyr, speed) * sw),
             by=c("region", "subsector_L3")]
    S3target[, (cname_to_remove) := NULL]
    S3target[, sw := sw/max(sw),
             by = c("region", "year", "sector")]
    nas <- S3target[is.na(sw)]
    if(nrow(nas) > 0){
      print(sprintf("NAs found in S3 shareweight trends for %s scenario.", tech_scen))
      browser()
    }

  }else{
    if(tech_scen != "Mix1"){
      print(sprintf("Warning: No mitigation factors found for %s scenario.", tech_scen))
    }
  }


  ## The values of SWS have to be normalized again
  return(list(
    S3S_final_pref=S3target,
    S2S3_final_pref=S2target,
    S1S2_final_pref=S1target,
    VS1_final_pref=VStarget,
    FV_final_pref=FVtarget
  ))
}
