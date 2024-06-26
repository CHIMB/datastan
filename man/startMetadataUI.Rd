% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_ui.R
\name{startMetadataUI}
\alias{startMetadataUI}
\title{Start Metadata UI}
\usage{
startMetadataUI(metadata_file_path)
}
\arguments{
\item{metadata_file_path}{A path to the sqlite metadata file.}
}
\description{
Upon call, and passing it a valid metadata file, the application will run using the metadata file as the file that the user can
make modifications to, this involves viewing entries, adding new entries, updating them, or deleting them.
}
\examples{
startMetadataUI("path/to/metadata.sqlite")
}
