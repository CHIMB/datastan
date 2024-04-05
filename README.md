
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated Data Standardization <img src="man/figures/chimb_logo.jpg" align="right" height="138" />

<br><br><br>

`automated.data.standardization` provides an easy and user friendly way
for data analysts to process data files and manage the standardization
rules responsible for processing the source fields of a dataset. This
package provides both programmatic and graphical ways to manage and
standardize the data:

- Standardize Source Data in a Programmatic Way:
  - `standardize_data(<input_file_path>, "input_dataset_code", <input_flags>, <output_folder>, <metadata_file>)`
- Standardize Data using a Graphical User Interface:
  - `startDataStandardizationUI()`
- Manage Standardization Rules Using a Graphical User Interface:
  - `startMetadataUI(<metadata_file_path>)`
- Create an Empty Metadata File:
  - `create_new_metadata("file_name", <output_folder>)`

For more details on how the code processes and cleans data, reads from
the metadata, how the user interfaces work on the back-end, consider
reading the <b>Developer Facing Documentation</b>.

For more details on how to work function calls, how to navigate the
pages of the user interfaces, and how to make changes, or add new
information to the metadata, consider reading the <b>User Facing
Documentation</b>.

# Installation

To install automated.data.standardization from GitHub, begin by
installing and loading the `devtools` package:

``` r
# install.packages("devtools")
library(devtools)
```

Afterwards, you may install the automated.data.standardization package
using `install_github()`:

``` r
devtools::install_github("Cole-Chuchmach/data.standardization.v1")
```
