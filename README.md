
# Statewide Survey Tools

<!-- badges: start -->
<!-- badges: end -->

`statewide.survey.tools` provides some helper functions for working with Statewide Survey data in R. It includes helpers for downloading data from REDCap, cleaning data using Stata code, and connection to the Airtable API.

## Installation

You can install the development version of statewide.survey.tools from [GitHub](https://github.com/ucsf-bhhi/statewide.survey.tools) with:

``` r
# install.packages("remotes")
remotes::install_github("ucsf-bhhi/statewide.survey.tools")
```

## API Keys

To download REDCap data or connect to Airtable, you'll need API keys.

For information on REDCap API keys (also called tokens), see: https://redcap.ucsf.edu/api/help/?content=tokens

For information on Airtable API keys, see: https://airtable.com/appOLCptG2wxvoGtH/api/docs#curl/authentication

After obtaining the API keys you'll need to store them in a `.Renviron` file in the same directory as your R code. See [here](https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf) for more information. 

Think of these API keys as a password, so don't share them. If you're using Git make sure to add `.Renviron` to your `.gitignore`.

The package expects the REDCap main project key to be stored as `MAIN_REDCAP_API_KEY` and the RDS project key to be stored as `RDS_REDCAP_API_KEY`.

The package expects the Airtable API key to be stored as `AIRTABLE_API_KEY`.
