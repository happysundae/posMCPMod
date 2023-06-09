% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_design.R
\name{gen_design}
\alias{gen_design}
\title{Generate a dose design of interest for dose finding study.}
\usage{
gen_design(dls, ngrp.equal = NULL, ntot = NULL, ratio = NULL)
}
\arguments{
\item{dls}{A vector that contains the dose levels of interest. MCPMod requires at least three active doses for the Mod step.}

\item{ngrp.equal}{An optional numeric value indicating the number of subjects in each dose group when the dose groups are of equal size.}

\item{ntot}{An optional numeric value indicating the total sample size.}

\item{ratio}{An optional vector indicating the allocation ratio of sample size between the different dose groups when the dose groups are of unequal size. The vector requires to be the same length as the dose level vector \code{dls}.}
}
\value{
A list of key variables that contains the dose levels, the total sample size, and the number of subjects in each dose group.
}
\description{
This function will create key variables for a specific dose design.
}
\details{
Author: Qing Zhao
}
\examples{
\dontrun{
# when the dose groups are of equal size 
gd.obj <- gen_design(dls = c(0, 6, 12, 18, 30), ngrp.equal = 60)
# when the dose groups are of unequal size
gd.obj <- gen_design(dls = c(0, 4, 7, 10), ntot = 300, ratio =  c(2, 1, 2, 2))
}
}
