#' Provide EDGE-Transport input parameters
#' @author Alois Dirnaichner
#' @param subtype one of the parameters required for EDGE-T
#' @param adjustments adjust historical data (boolean, defaults to TRUE)

calcEDGETinputs <- function(subtype, adjustments = TRUE) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  switch(
    subtype,
    "annualMileage" = {
      am_unit <- "vkm/yr"
      ## in the prepare function we prepare the mileage for all vehicle_types that
      ## are found in the data and extend the data to the other technologies
      ## i.e., FCEVs, BEVs. We fill some gaps and map the vehicle_types.
      ## we do not yet: 1) extend data to other regions or 2) to other vehicle_types
      dt <- toolEDGETprepareUCD(readSource("UCD", subtype), subtype)
      dt[, c("model", "scenario", "variable") := NULL]
      dt[, unit := am_unit]

      ## use same periods in both sources
      dt <- rbind(dt[period == 2005], dt[period == 2005][, period := 2010])

      am_TRACCS <- toolEDGETprepareTRACCS(readSource("TRACCS", subtype), subtype)
      am_TRACCS[, c("model", "scenario", "variable") := NULL]
      am_TRACCS[, unit := am_unit]
      am_TRACCS <- am_TRACCS[period %in% c(2005, 2010)]

      ## add missing "other techs"
      lstruct <- lstruct[vehicle_type %in% unique(am_TRACCS$vehicle_type)]
      full_table <- CJ(region=am_TRACCS$region, period=am_TRACCS$period,
                       vehicle_type=am_TRACCS$vehicle_type,
                       technology=lstruct$technology, unit=am_TRACCS$unit, unique=T)
      am_TRACCS <- am_TRACCS[full_table, on=c("region", "period", "vehicle_type", "technology", "unit")]
      am_TRACCS[, value := ifelse(is.na(value), .SD[technology == "Liquids", value], value),
         by=c("region", "period", "vehicle_type")]
      am_TRACCS <- am_TRACCS[lstruct, on=c("vehicle_type", "technology")]

      ## trucks mileage and buses only from TRACCS, this has to be applied to UCD
      tm <- am_TRACCS[subsector_l2 %in% c("Bus" ,"trn_freight_road_tmp_subsector_L2")]
      ## use mean values for non-TRACCS countries
      tm <- tm[, .(value = mean(value)), by=c("period", "vehicle_type")]

      nonTRACCS_tm <- CJ(region=dt$region, period=tm$period, unique=TRUE)
      nonTRACCS_tm <- nonTRACCS_tm[tm, on="period", allow.cartesian=TRUE]
      nonTRACCS_tm <- lstruct[nonTRACCS_tm, on="vehicle_type", allow.cartesian=TRUE]
      nonTRACCS_tm[, unit := unique(dt$unit)]

      dt <- rbind(dt, nonTRACCS_tm, use.names=TRUE, fill=TRUE)

      ## update-join - prefer TRACCS values wherever we have them
      ## dt[, traccs_value := am_TRACCS[.SD, on=colnames(dt)[1:10], x.value]]
      dt[am_TRACCS, value := i.value, on=colnames(dt)[1:10]]

      plane_types <- unique(lstruct[grepl("Aviation", vehicle_type), vehicle_type])
      ## for planes we assume 3000 working hours per year at 750 km/h ~ 2e6 km/yr
      ## https://eu.usatoday.com/story/travel/columnist/cox/2012/11/19/ask-the-captain-how-far-does-a-jet-fly-during-its-lifetime/1712269/
      planes <- CJ(region=dt$region, period=dt$period, vehicle_type=plane_types, value=2e6,
                   unit=am_unit, unique=TRUE)
      planes <- lstruct[planes, on="vehicle_type", allow.cartesian=TRUE]

      ship_types <- unique(lstruct[grepl("Ship", vehicle_type), vehicle_type])
      ## for international ships we assume 300.000 km/yr
      ## https://www.ioscm.com/blog/industry-facts-101-the-shipping-industry-is-enormous/
      ships <- CJ(region=dt$region, period=dt$period, vehicle_type=ship_types, value=3e5,
                  unit=am_unit, unique=TRUE)
      ## domestic ships one third
      ships[vehicle_type == "Domestic Ship_tmp_vehicletype", value := 1e5]
      ships <- lstruct[ships, on="vehicle_type", allow.cartesian=TRUE]

      dt <- rbind(dt, planes, ships, use.names=TRUE)

      q <- as.quitte(dt)

      ## check for missing data by joining the logit structure and looking for NAs,
      ## the join has to happen on subtype level since it does not make sense
      ## slash we do not have data for all categories. E.g., mileage for non-motorized modes or
      ## trains does not make sense
      am_check <- lstruct[!subsector_l3 %in% c("Walk", "Cycle", "HSR", "Passenger Rail", "Freight Rail")]
      q <- q[am_check, on=colnames(am_check)]
      ## note that the actual check is done for all subtypes at the end of the function

      weight <- calcOutput("GDP", aggregate = F)[, unique(q$period), "gdp_SSP2"]
      unit <- am_unit
      description <- "Annual mileage data for LDV, trucks, trains and ships. Sources: TRACCS, UCD."

    },

    "esDemand" = {
      fr_unit <- "million tkm"
      pa_unit <- "million pkm"
      ## the GCAM data is more-or-less complete, we use this as a starting point
      dt <- toolEDGETprepareGCAM(readSource("GCAM", subtype), subtype)
      dt[sector == "trn_freight", unit := fr_unit]
      dt[sector == "trn_pass", unit := pa_unit]
      dt[, c("model", "scenario", "variable") := NULL]
      full <- rbind(
        CJ(region=dt$region, period=dt$period, vehicle_type="Walk_tmp_vehicletype",
           technology="Walk_tmp_technology", unit=pa_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period, vehicle_type="Cycle_tmp_vehicletype",
           technology="Cycle_tmp_technology", unit=pa_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period,
           vehicle_type=c("International Aviation_tmp_vehicletype", "Domestic Aviation_tmp_vehicletype"),
           technology="Hydrogen", unit=pa_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period,
           vehicle_type=unique(dt[subsector_l1 == "trn_pass_road_LDV_4W", vehicle_type]),
           technology="Hybrid Electric", unit=pa_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period,
           vehicle_type="Bus_tmp_vehicletype",
           technology="BEV", unit=pa_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period,
           vehicle_type="Bus_tmp_vehicletype",
           technology="FCEV", unit=pa_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period,
           vehicle_type=unique(dt[subsector_l3 == "trn_freight_road", vehicle_type]),
           technology="BEV", unit=fr_unit, unique=TRUE),
        CJ(region=dt$region, period=dt$period,
           vehicle_type=unique(dt[subsector_l3 == "trn_freight_road", vehicle_type]),
           technology="FCEV", unit=fr_unit, unique=TRUE))

      dt[, c("sector", "subsector_l3", "subsector_l2", "subsector_l1", "univocal_name") := NULL]
      dt <- rbind(
        dt,
        dt[full, on=c("region", "period", "vehicle_type", "technology", "unit")]
      )
      ## cycle and walk has to be replaced by global averages (this data is simply missing)
      dt[, total := sum(value, na.rm=TRUE), by=c("region", "period")]
      dt[vehicle_type == "Cycle_tmp_vehicletype", share := value/total]
      dt[, mean_share := mean(share, na.rm=TRUE), by="period"]
      dt[vehicle_type == "Cycle_tmp_vehicletype" & is.na(value), value := total * mean_share]
      dt[, c("share", "mean_share") := NULL]

      dt[vehicle_type == "Walk_tmp_vehicletype", share := value/total]
      dt[, mean_share := mean(share, na.rm=TRUE), by="period"]
      dt[vehicle_type == "Walk_tmp_vehicletype" & is.na(value), value := total * mean_share]
      dt[, c("share", "mean_share", "total") := NULL]
      ## everything else is probably 0
      dt[is.na(value), value := 0]
      ## full structure so that we can replace values
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]


      es_TRACCS <- rbind(
        toolEDGETprepareTRACCS(readSource("TRACCS", "roadTkmDemand"), "roadTkmDemand")[, unit := fr_unit],
        toolEDGETprepareTRACCS(readSource("TRACCS", "roadPkmDemand"), "roadPkmDemand")[, unit := pa_unit]
      )[period %in% c(2005, 2010)]
      es_TRACCS[, c("model", "scenario", "variable") := NULL]

      dt[es_TRACCS, value := i.value, on=colnames(dt)[1:10]]

      q <- as.quitte(dt)

      ## expand and check
      q <- q[lstruct, on=intersect(colnames(q), colnames(lstruct))]

      weight <- calcOutput("GDP", aggregate = F)[, unique(q$period), "gdp_SSP2"]
      unit <- sprintf("%s or %s", pa_unit, fr_unit)
      description <- "Energy service demand. Sources: TRACCS, GCAM."

    },

    "loadFactor" = {
      fr_unit <- "tkm/veh"
      pa_unit <- "pkm/veh"
      dt <- toolEDGETprepareGCAM(readSource("GCAM", subtype), subtype)
      dt[sector == "trn_freight", unit := fr_unit]
      dt[sector == "trn_pass", unit := pa_unit]
      dt[, c("model", "scenario", "variable") := NULL]

      dt <- rbind(
        dt,
        dt[vehicle_type == "International Aviation_tmp_vehicletype" & technology == "Liquids"][
          , technology := "Hydrogen"],
        dt[vehicle_type == "Domestic Aviation_tmp_vehicletype" & technology == "Liquids"][
          , technology := "Hydrogen"],
        dt[subsector_l1 == "trn_pass_road_LDV_4W" & technology == "Liquids"][
          , technology := "Hybrid Electric"],
        dt[vehicle_type == "Bus_tmp_vehicletype" & technology == "Liquids"][
          , technology := "BEV"],
        dt[vehicle_type == "Bus_tmp_vehicletype" & technology == "Liquids"][
          , technology := "FCEV"],
        dt[subsector_l3 == "trn_freight_road" & technology == "Liquids"][
          , technology := "BEV"],
        dt[subsector_l3 == "trn_freight_road" & technology == "Liquids"][
          , technology := "FCEV"])

      ## full structure
      dt[, c("sector", "subsector_l3", "subsector_l2", "subsector_l1",
             "univocal_name") := NULL]
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]


      lf_TRACCS <- toolEDGETprepareTRACCS(readSource("TRACCS", "loadFactor"), "loadFactor")
      lf_TRACCS[sector == "trn_freight", unit := fr_unit]
      lf_TRACCS[sector == "trn_pass", unit := pa_unit]
      lf_TRACCS[, c("model", "scenario", "variable") := NULL]

      dt[lf_TRACCS, value := i.value, on=colnames(dt)[1:10]]

      q <- as.quitte(dt)

      ## expand and check
      lstruct <- lstruct[!subsector_l3 %in% c("Walk", "Cycle")]
      q <- q[lstruct, on=intersect(colnames(q), colnames(lstruct))]

      weight <- calcOutput("GDP", aggregate = F)[, unique(q$period), "gdp_SSP2"]
      unit <- sprintf("%s or %s", pa_unit, fr_unit)
      description <- "Load factor, also called occupancy ration for passenger vehicles. Sources: TRACCS, GCAM."

    },

    "energyIntensity" = {
      unit <- "MJ/vkm"
      dt <- toolEDGETprepareGCAM(readSource("GCAM", "feVkmIntensity"), subtype)
      dt[, c("model", "scenario", "variable") := NULL]
      CONV_MJ_btu <- 947.777
      dt[, value := value/CONV_MJ_btu]
      lstruct <- lstruct[!subsector_l3 %in% c("Walk", "Cycle")]

      ## full structure
      setkey(lstruct[, k := 1], "k")
      full <- setkey(CJ(region=dt$region, period=dt$period, k=1, unique=TRUE), "k")
      full <- lstruct[full, allow.cartesian=TRUE][, k := NULL]

      dt <- dt[full, on=intersect(colnames(dt), colnames(full))]

      intUCD <- toolEDGETprepareUCD(readSource("UCD", "feVkmIntensity"), subtype)
      intUCD <- rbind(intUCD,
                      intUCD[period == 2005][, period := 2010],
                      intUCD[period == 2005][, period := 1990])

      ## we use UCD timesteps
      dt <- dt[period %in% unique(intUCD$period)]

      ## update from UCD
      dt[intUCD, value := i.value, on=colnames(dt)[1:10]]


      ## PSI data


      dt <- rbind(
        dt,
        ## energy efficiency for hydrogen airplanes
        ## http://dx.doi.org/10.1016/j.ijhydene.2015.04.055
        dt[vehicle_type == "International Aviation_tmp_vehicletype" & technology == "Liquids"][
          , `:=`(technology = "Hydrogen", value = 0.9 * value)],
        dt[vehicle_type == "Domestic Aviation_tmp_vehicletype" & technology == "Liquids"][
        , `:=`(technology = "Hydrogen", value = 0.85 * value)])



    },

    "speed" = {
      unit <- "km/h"
      magpieobj <- readSource("GCAM", "speedNonMotorized")
      mo2 <- readSource("GCAM", "speedNonMotorized")
      q <- rbind(
        toolEDGETprepareGCAM(readSource("GCAM", "speedMotorized"), "speedMotorized"),
        toolEDGETprepareGCAM(readSource("GCAM", "speedNonMotorized"), "speedNonMotorized"))

      ## expand and check
      q <- q[lstruct, on=intersect(colnames(q), colnames(lstruct))]

      weight <- calcOutput("GDP", aggregate = F)[, unique(q$period), "gdp_SSP2"]
      unit <- "unknown"
      description <- "Vehicle speed. Source: GCAM."
    }
  )

  tst <- q[is.na(value)]
  if(nrow(tst) > 0) {
    print(sprintf("Missing elements in inputdata subtype %s.", subtype))
    browser()
  }

  nonvalcols <- colnames(q)[colnames(q) != "value"]
  tst <- q[, ..nonvalcols]
  tst <- tst[duplicated(tst)]
  if(nrow(tst) > 0) {
    print(sprintf("Duplicated elements in inputdata subtype %s.", subtype))
    browser()
  }

  if(adjustments) {
    q <- toolEDGETadjustments(q, subtype)
  }


  return(list(
    x           = as.magpie(as.data.frame(q)),
    weight      = weight,
    unit        = unit,
    description = description
  ))
}
