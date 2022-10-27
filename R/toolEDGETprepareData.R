#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Alois Dirnaichner
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolEDGETprepareGCAM <- function(magpieobj, subtype) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))
  dt <- magpie2dt(magpieobj)
  mapfile <- system.file("extdata", "mapping_GCAM_categories.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_GCAM_cat = fread(mapfile)

  mapfile <- system.file("extdata", "mapping_GCAM_technologies.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_GCAM_tech = fread(mapfile)

  switch(
    subtype,
    "esDemand" = {
      dt <- mapping_GCAM_cat[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]
      dt <- mapping_GCAM_tech[dt, on="GCAM_technology"]
      dt[is.na(technology), technology := GCAM_technology]
      dt <- unique(dt[, .(value=sum(value)), by=c("iso", "year", "vehicle_type", "technology", "Units")])
      ## add full logit
      setnames(dt, "Units", "unit")
      ## only use vehicle types and techs that are in lstruct
      dt <- lstruct[dt, on=c("vehicle_type", "technology")]
      dt <- dt[!is.na(sector)]

    },
    "energyIntensity" = {
      ## we use 2010 weights, this is sufficient
      weight <- readSource("GCAM", subtype="esDemand")
      weight <- magpie2dt(weight)[year == 2010][, c("year", "Units") := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, "tranSubsector", "subsector")
      dt <- weight[dt, on=c("iso", "subsector", "GCAM_technology")]
      ## using a very low demand leads to equal distribution if there is no demand
      ## for all available technologies
      dt[is.na(esdem) | esdem == 0, esdem := min(dt$value, na.rm=TRUE)/100]

      dt <- mapping_GCAM_cat[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]
      dt <- mapping_GCAM_tech[dt, on="GCAM_technology"]
      dt[is.na(technology), technology := GCAM_technology]

      dt <- dt[, .(value=sum(value*esdem)/sum(esdem)),
               by=c("iso", "year", "vehicle_type", "technology")]

      dt <- lstruct[dt, on=c("vehicle_type", "technology")]

    },
    "speedMotorized" = {
      ## weights only available for these years
      dt <- dt[year %in% c(1990, 2005, 2010)]
      weight <- readSource("GCAM", subtype="esDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")
      weight <- weight[, .(esdem = sum(esdem)), by=c("iso", "year", "subsector")]

      setnames(dt, "tranSubsector", "subsector")
      dt <- weight[dt, on=c("iso", "year", "subsector")]
      ## using a very low demand leads to equal distribution if there is no demand
      ## for all available technologies
      dt[is.na(esdem) | esdem == 0, esdem := min(dt$value, na.rm=TRUE)/100]

      dt <- mapping_GCAM_cat[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]

      dt <- dt[, .(value=sum(value*esdem)/sum(esdem)),
               by=c("iso", "year", "vehicle_type")]

      lstruct <- lstruct[!subsector_l3 %in% c("Walk", "Cycle")]
      dt <- lstruct[dt, on="vehicle_type", allow.cartesian=TRUE]

      ## join the other way round to see if data is complete
      dt[, c("sector", "subsector_l3", "subsector_l2", "subsector_l1",
             "univocal_name") := NULL]
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]

    },
    "speedNonMotorized" = {
      lstruct <- lstruct[subsector_l3 %in% c("Walk", "Cycle")]
      setnames(dt, "tranSubsector", "subsector")
      dt <- mapping_GCAM_cat[dt, on="subsector"]
      dt[, c("subsector", "technology") := NULL]
      dt <- lstruct[dt, on="vehicle_type"]

    },
    "loadFactor" = {
      ## weights only available for these years
      dt <- dt[year %in% c(1990, 2005, 2010)]
      weight <- readSource("GCAM", subtype="esDemand")
      weight <- magpie2dt(weight)[, Units := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "GCAM_technology"))
      dt <- weight[dt, on=c("iso", "year", "subsector", "GCAM_technology")]
      ## using a very low demand leads to equal distribution if there is no demand
      ## for all available technologies
      dt[is.na(esdem) | esdem == 0, esdem := min(dt$value, na.rm=TRUE)/100]

      dt <- mapping_GCAM_cat[dt, on="subsector"]
      dt[is.na(vehicle_type), vehicle_type := subsector]
      dt <- mapping_GCAM_tech[dt, on="GCAM_technology"]
      dt[is.na(technology), technology := GCAM_technology]

      dt <- dt[, .(value=sum(value*esdem)/sum(esdem)),
               by=c("iso", "year", "vehicle_type", "technology")]

      lstruct <- lstruct[!subsector_l3 %in% c("Walk", "Cycle")]
      dt <- lstruct[dt, on=c("vehicle_type", "technology")]

    })

  test <- dt[is.na(value)]
  if(nrow(test) > 0){
    print(sprintf("Missing %s data in GCAM prepare", subtype))
    browser()
  }

  nc <- colnames(dt)[colnames(dt) != "value"]
  test <- dt[, ..nc]
  test <- test[duplicated(test)]
  if(nrow(test) > 0){
    print(sprintf("Duplicates found in %s data in GCAM prepare", subtype))
    browser()
  }
  setnames(dt, "iso", "region", skip_absent = TRUE)
  return(as.quitte(dt))

}


#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Alois Dirnaichner
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolEDGETprepareTRACCS <- function(magpieobj, subtype) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  ## load mappings
  mapfile <- system.file("extdata", "mapping_TRACCS_roadvehicles.csv",
                         package = "edgeTransport", mustWork = TRUE)
  mapping_TRACCS = fread(mapfile, skip = 0)
  techmapfile <- system.file("extdata", "mapping_TRACCS_techs.csv",
                         package = "edgeTransport", mustWork = TRUE)
  techmap = fread(techmapfile, skip = 0)

  weight <- readSource("TRACCS", subtype="roadVkmDemand")
  weight <- magpie2dt(weight)[, unit := NULL]
  setnames(weight, "value", "vkm")
  dt <- magpie2dt(magpieobj)
  dt <- dt[TRACCS_technology != "Other"]

  switch(
    subtype,
    "loadFactor" = ,
    "annualMileage" = {
      wcols <- c("iso", "period", "TRACCS_category", "TRACCS_vehicle_type", "TRACCS_technology")
      dt <- weight[dt, on=wcols]

      dt <- techmap[dt, on="TRACCS_technology"]
      dt <- mapping_TRACCS[dt, on=c("TRACCS_category", "TRACCS_vehicle_type")]

      dt <- unique(dt[,
               .(unit, value=sum(value*vkm)/sum(vkm)),
               by=c("iso", "period", "vehicle_type", "technology")])
      ## some small countries do not have 40t, we use mean mileage
      dt[, value := ifelse(is.na(value), mean(value, na.rm=TRUE), value),
         by=c("period", "vehicle_type", "technology")]

      dt <- lstruct[dt, on=c("vehicle_type", "technology")]
    },
    ## note that leaving the case empty defaults to the next case in R
    "roadPkmDemand" = ,
    "roadTkmDemand" = {
      dt <- techmap[dt, on="TRACCS_technology"]
      dt <- mapping_TRACCS[dt, on=c("TRACCS_category", "TRACCS_vehicle_type")]

      dt <- unique(dt[
        , .(unit, value=sum(value)),
        by=c("iso", "period", "vehicle_type", "technology")])

      lstruct <- lstruct[vehicle_type %in% unique(dt$vehicle_type)]
      full_table <- CJ(iso=dt$iso, period=dt$period, vehicle_type=dt$vehicle_type,
                       technology=lstruct$technology, unit=dt$unit, unique=T)
      dt <- dt[full_table, on=c("iso", "period", "vehicle_type", "technology", "unit")]
      dt[is.na(value), value := 0]
      dt <- dt[lstruct, on=c("vehicle_type", "technology")]

    }
  )

  nc <- colnames(dt)[colnames(dt) != "value"]
  test <- dt[, ..nc]
  test <- test[duplicated(test)]
  if(nrow(test) > 0){
    print("Duplicates in data in TRACCS prepare")
    browser()
  }

  test <- dt[is.na(value)]
  if(nrow(test) > 0) {
    print("Missing data in TRACCS prepare")
    browser()
  }

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}


#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full logit structure.
#'
#' @author Alois Dirnaichner
#' @param magpieobj the input data read via readSource, a magpie object
#' @param sourcetype one of the different EDGE-T inputdata sources
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table fread

toolEDGETprepareUCD <- function(magpieobj, subtype) {
  lstruct <- fread(system.file("extdata/logit_structure.csv", package="edgeTransport", mustWork=TRUE))

  ## mapping_UCD <- fread("~/git/edgeTransport/inst/extdata/mapping_UCD_categories.csv")
  mapfile_cat <- system.file("extdata", "mapping_UCD_categories.csv",
                             package = "edgeTransport", mustWork = TRUE)
  mapping_UCD_cat = fread(mapfile_cat, skip = 0)

  mapfile_tech <- system.file("extdata", "mapping_UCD_technologies.csv",
                             package = "edgeTransport", mustWork = TRUE)
  mapping_UCD_tech = fread(mapfile_tech, skip = 0)

  weight <- readSource("UCD", subtype="feDemand")
  weight <- magpie2dt(weight)[, unit := NULL]
  weight <- weight[, year := NULL]

  dt <- magpie2dt(magpieobj)
  setnames(weight, "value", "fe")

  switch(
    subtype,
    "annualMileage" = {
      ## fe data only available for 2005
      wcols <- c("iso", "UCD_sector", "mode", "size_class")
      weight <- weight[, .(fe=sum(fe), UCD_technology="All", UCD_fuel="All"), by=wcols]
      dt <- weight[dt, on=c(wcols, "UCD_technology", "UCD_fuel")]

      dt <- mapping_UCD_cat[dt, on=c("UCD_sector", "mode", "size_class")]
      dt <- unique(dt[, .(unit, value=sum(value*fe)/sum(fe)), by=c("iso", "year", "vehicle_type")])
      dt <- lstruct[dt, on="vehicle_type", allow.cartesian=T]
    },
    "energyIntensity" = {
      dt <- dt[UCD_technology != "Coal"]

      wcols <- c("iso", "UCD_sector", "mode", "size_class", "UCD_technology", "UCD_fuel")
      dt <- weight[dt, on=wcols]
      dt[is.na(fe) | fe == 0, fe := min(dt$value, na.rm=TRUE)/100]

      dt <- mapping_UCD_tech[dt, on="UCD_technology"]
      dt <- mapping_UCD_cat[dt, on=c("UCD_sector", "mode", "size_class")]
      dt[is.na(technology), technology := UCD_technology]
      dt <- unique(dt[, .(unit, value=sum(value*fe)/sum(fe)),
                      by=c("iso", "year", "vehicle_type", "technology")])
      dt <- lstruct[dt, on=c("vehicle_type", "technology")]

    }
  )

  setnames(dt, "iso", "region")
  return(as.quitte(dt))

}
