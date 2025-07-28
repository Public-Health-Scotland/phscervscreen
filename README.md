
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Public Health Scotland cervical screening analysis help

<!-- badges: start -->
<!-- badges: end -->

This package includes functions used in the Public Health Scotland’s
analysis of the Scottish Cervical Screening Programme data.

These functions are made available in a package to build more robust
quality assurance into the code, and reducing the chance of errors when
running scripts.

Currently, there are 1 functions exported:

- cerv_get_pct()
- create_palette()

<br>

***`cerv_get_pct()`***

This function creates a new data frame with summarised counts and
percentage for each grouping. If coverage is calculated, then a 2-split
age grouping can be used to calculate the combined coverage for those,
i.e. for look-bakc periods of 3.5- and 5.5-years. See the Help pages of
the functions for more details.

***`create_palette()`***

This function creates a vector with HEX code colours to use with graphs.
The colour palette generated is created by PHS to be accessible when
used in the order provided.

### Installation

You can install the development version of phscervscreen from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Public-Health-Scotland/phscervscreen")
```
