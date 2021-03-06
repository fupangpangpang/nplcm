% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/combine_subsites.R
\name{combine_subsites}
\alias{combine_subsites}
\title{Combine subsites from the actual PERCH data set}
\usage{
combine_subsites(MeasDir, subsites_list, newsites_vec)
}
\arguments{
\item{MeasDir}{The file path to the data file (.csv)}

\item{subsites_list}{The list of subsite group names. Each group is a vector of
subsites to be combined}

\item{newsites_vec}{A vector of new site names.
Its length should equal \code{"subsites_list"}}
}
\value{
A data frame that has combined sites
}
\description{
In the Actual PERCH data set, a study site may have multiple subsites. This
function combines the all the study subjects from the same site.
}

