% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flag_standardizing_script.R
\name{impute_sex}
\alias{impute_sex}
\title{Impute Sex}
\usage{
impute_sex(source_df, processed_names, processed_sexes, flag_lookup_table)
}
\arguments{
\item{source_df}{The current processed/cleaned data frame of the input file.}

\item{processed_names}{The current processed primary given names on this chunk iteration or after processing finishes.}

\item{processed_sexes}{The current processed sexes on this chunk iteration or after processing finishes.}

\item{flag_lookup_table}{Lookup table used to determine what output file format is to be used. Output format can be of type rdata, csv, xlsx or just sqlite.}
}
\value{
The original data frame with an additional column named "imputed_sex".
}
\description{
The impute_sex() function will take in a source data frame, two vectors and flag lookup table, using that
table it will identify whether we will be imputing sex using a custom file of first names and cooresponding
sexes, otherwise we may attempt to impute sex internally by using the data within the processed file.
}
\examples{
df <- impute_sex(df, c("John", "Jane", "Cole", "John"), c("M", "F", "C", NA), flag_lookup)
}
