
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Automated Data Standardization <img src="man/figures/chimb_logo.jpg" align="right" height="138" />

<br><br><br>

# Introduction

`datastan` provides an easy and user friendly way for data analysts to
process data files and manage the standardization rules responsible for
processing the source fields of a dataset. This package provides both
programmatic and graphical ways to manage and standardize source files.

This package would be most beneficial in the field of data science,
specifically data-linkage and data analysis as the `datastan` package
would help minimize the time spent on writing standardization rules for
each dataset by using the standardization rules of the package to
normalize disparate source files to a common format to ready it for
record-linkage and analysis.

# Installation

To install datastan from GitHub, begin by installing and loading the
`devtools` package:

``` r
# install.packages("devtools")
library(devtools)
```

Afterwards, you may install the automated.data.standardization package
using `install_github()`:

``` r
devtools::install_github("CHIMB/datastan")
```

To install datastan locally from GitHub, select the most recent release
from the right-hand tab on the GitHub repository page. Download the
<b>Source code (zip)</b> file, then move over to RStudio. You may then
run the code:

``` r
path_to_pkg <- file.choose() # Select the unzipped package you downloaded from GitHub.
devtools::install_local(path_to_pkg)
```

# Usage

Before standardizing data can take place, you must enter the
standardizing rules for the desired columns using the Metadata User
Interface application, which must be supplied with an <b>.sqlite</b>
file. To create an empty file for modification, you may run the
`create_new_metadata()` function from the `datastan` package:

``` r
output_folder <- choose.dir()
create_new_metadata("my_standardizing_rules", output_folder)
```

You may then add your desired source file information and choose the
desired standardizing rules in the Metadata application, which can be
run using the function `startMetadataUI()` while supplying an existing
metadata file:

``` r
my_metadata <- file.choose() # Choose the "my_standardizing_rules.sqlite" file.
startMetadataUI(my_metadata)
```

Once the standardizing rules are in place, you may standardize the data
in one of two ways:

- <b>OPTION 1:</b> To standardize data programmatically, you may call
  the `standardize_data()` function, consider the example:

  - Consider you added a new data set with standardizing rules in the
    metadata user interface called “mydataset”, you may then call the
    `standardize_data()` function the following way:

  ``` r
  input_file <- file.choose() # Choose the input file that contains the source dataset.
  dataset_code <- "mydataset" # This is the dataset_code we chose in the metadata application.
  standardizing_options <- setNames(c("chunk_size", "convert_name_case", "compress_address_whitespace"), c(200000, "upper", "yes"))
  output_folder <- choose.dir() # Choose the output folder where the cleaned data file will be output to.
  standardizing_rules <- choose.file() # Choose the .sqlite file containing the rules you just added.

  df <- standardize_data(input_file, dataset_code, standardizing_options, output_folder, standardizing_rules)
  ```

- <b>OPTION 2:</b> To standardize the data using a Graphical User
  Interface, you may run the `startDataStandardizationUI()` and follow
  along with the instructions in the application:

  ``` r
  startDataStandardizationUI()
  ```

# Additional Information & Documentation

For more details on how the code processes and cleans data, reads from
the metadata, how the user interfaces work on the back-end, consider
reading the <b>Developer Facing Documentation</b>.

For more details on how to work function calls, how to navigate the
pages of the user interfaces, and how to make changes, or add new
information to the metadata, consider reading the <b>User Facing
Documentation</b>.
