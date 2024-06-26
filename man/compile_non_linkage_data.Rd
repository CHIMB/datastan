% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flag_standardizing_script.R
\name{compile_non_linkage_data}
\alias{compile_non_linkage_data}
\title{Compile Health and Program Data}
\usage{
compile_non_linkage_data(source_data_frame, db_conn, dataset_id)
}
\arguments{
\item{source_data_frame}{The source data frame, containing both the health and program data we’re interested in extracting, as well as the valid linkage fields found by checking the metadata database with the dataset_id.}

\item{db_conn}{A connection to the metadata database, used with the dataset_id argument to determine what fields are valid to also include in the health and program data output.}

\item{dataset_id}{The dataset_id of the dataset we’re currently cleaning.}
}
\value{
A processed data frame containing the non-linkage fields in their original format.
}
\description{
Fields that were not included in the processed data frame that is to be used during linkage, as well as some additional fields
that may be included such as postal code, gender, birthday, are also included in the output program and health data.
}
\examples{
non_linkage_df <- compile_non_linkage_data(source_df, db, 1)
}
