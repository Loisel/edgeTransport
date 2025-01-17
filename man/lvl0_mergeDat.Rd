% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lvl0_mergeDat.R
\name{lvl0_mergeDat}
\alias{lvl0_mergeDat}
\title{Merge input data}
\usage{
lvl0_mergeDat(
  UCD_output,
  EU_data,
  PSI_costs,
  GDP_MER,
  altCosts,
  CHN_trucks,
  GCAM_data,
  PSI_int,
  trsp_incent,
  fcr_veh,
  nper_amort_veh,
  smartlifestyle,
  SSP_scen,
  years,
  REMIND2ISO_MAPPING,
  ariadne_adjustments = TRUE
)
}
\arguments{
\item{UCD_output}{UCD data}

\item{EU_data}{EU data}

\item{PSI_costs}{PSI-based costs}

\item{GDP_MER}{GDP MER per capita}

\item{altCosts}{alternative trucks cost}

\item{CHN_trucks}{CHN trucks costs}

\item{GCAM_data}{GCAM data}

\item{PSI_int}{PSI-based intensity}

\item{trsp_incent}{transport incentives on capital costs}

\item{fcr_veh}{annualization factor for LDVs}

\item{nper_amort_veh}{years of amortization which a LDV}

\item{smartlifestyle}{switch activatinf sustainable lifestyles}

\item{SSP_scen}{REMIND SSP scenario}

\item{years}{time steps}

\item{REMIND2ISO_MAPPING}{REMIND regional mapping}

\item{ariadne_adjustments}{adjust intensity levels according to the ARIADNE project.
Affects mainly DEU and EU countries.}
}
\value{
costs, intensity, LF, AM, demand
}
\description{
Final values:
}
\author{
Marianna Rottoli, Alois Dirnaichner
}
