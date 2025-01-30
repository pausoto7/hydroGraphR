Introduction to hydroGraphR
================
Paula Soto<br>
2025-01-30

`hydroGraphR` is an R package designed to simplify the process of
creating hydrographs that compare historical hydrometric data with a
specific year’s traces. This vignette provides an overview of the
package’s functionality and demonstrates how to use it in a typical
workflow.

## Installation

To install the development version of `hydroGraphR` from GitHub:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install hydroGraphR
install_github("pausoto7/hydroGraphR")

# Download library
library(hydroGraphR)
```

## Workflow Overview

A typical workflow for hydroGraphR involves the following steps:

1.  Download hydrometric data for specific WSC stations.
2.  Generate single-year and historical statistics.
3.  Visualize the data by creating hydrographs.

### Step 1: Download Hydrometric Data

Use the `dl_hydro()` function to download data for specific Water Survey
of Canada (WSC) station numbers. You can find station numbers on the
[WSC website](https://wateroffice.ec.gc.ca/search/real_time_e.html).

You may also assign nick names to stations for easier identification in
subsequent analyses. If you are going to use nick names ensure that the
number of nick names matches the number of station numbers.

Examples:

``` r
#example of station(s) download without nicknames
hydro_data <- dl_hydro(station_number = c("08LD001", "08LC002"))

#example of station(s) download with nick names. 
hydro_data_nickname <- dl_hydro(station_number = "08LD001", nick_name = "Adams River")
```

### Step 2: Create Statistics for Single Year and Historical Data

Once the hydrometric data is downloaded, you can calculate statistics
for:

- A specific year of interest (YOI) using
  `create_hydro_stats_singleYr()`.
- Historical data using `create_hydro_stats_historical()`.

WY is a logical value indicating whether to present hydrograph by water
year (Nov-Oct) (TRUE) or calendar year (Jan-Dec) (FALSE).

Max/min dates can also be selected for historical dates if focus is on a
specific period.For example, you could enter date_minimum =
“2010-01-01”, date_maximum - “2019-12-31” and YOI = 2020 if you wanted
to compare 2020 with the 2010’s.

***Important Note: Data from the past two years may be provisional, as
such should be used with caution when a recent YOI is selected. Status
of data can be found on the WSC website.***

<br>

``` r
all_hydro_sites_1yr <- create_hydro_stats_singleYr(hydro_data, 
                                                   YOI = 2021, 
                                                   WY = FALSE) # Calendar year

all_hydro_sites_hist <- create_hydro_stats_historical(hydro_data) # Use all available hydrometric data
```

### Step 3: Create Hydrographs

Visualize your data using one of two functions, depending on your
preferred output style.

- `create_hydrograph_separate()`- Generates individual hydrographs for
  each station.

- `create_hydrograph_faceted()` - Creates a single faceted plot
  displaying multiple stations together.

#### Variables

- **Parameter**: Options are `"flow"` for a discharge hydrograph, and
  `"level"` for a water level hydrograph.

- **WY**: Logical value indicating whether to present hydrograph by
  water year (Nov-Oct)(`TRUE`) or calendar year (Jan-Dec)(`FALSE`).

  - No matter the output selection ensure that the water year (WY)
    choice matches the selection made in Step 2. For example, if
    “calendar year” was chosen for statistics in Step 2, it must also be
    selected for the hydrograph presentation. Selecting a different year
    type will result in missing data on the hydrograph and trigger a
    warning.

- **output_type**:

  - `"print"` will print your image in R, useful for embedding in
    rmarkdown or shiny type outputs.
  - `"jpeg"` will produce a jpeg image and save it to the “figures/”
    folder of this project.

<br>

#### **Option 1**: `create_hydrograph_separate()`

- This function creates separate hydrographs as individual images which
  are ideal for standalone use or printing. When the “jpeg” option is
  selected in this option, an additional title will appear on the image
  with the station name to avoid confusion of station ID if the file
  name is not easily available (for example if the image is pasted into
  a word document).

``` r

create_hydrograph_separate(
  all_hydro_sites_hist,
  all_hydro_sites_1yr,
  parameter = "flow", # Discharge hydrograph
  output_type = "print",
  WY = FALSE # calendar year
)
```

![](README_files/figure-gfm/hydrographsep-1.png)<!-- -->![](README_files/figure-gfm/hydrographsep-2.png)<!-- -->
<br><br><br>

#### **Option 2**: `create_hydrograph_faceted()`

- This function creates a single faceted hydrograph, making it easy to
  compare multiple stations side by side. There are a few additional
  variabels that can also be modified in this function:

  - `fixed_y_scales` = A character string specifying whether the y-axis
    scale is “fixed” or “free” across facets. Defaults to “fixed”.
  - `custom_ymax_input` = A numeric value for a custom maximum y-axis
    value. Leave as NA for automatic ymax.
  - `custom_ymin_input` = A numeric value for a custom minimum y-axis
    value. Leave as NA for automatic xmax.
  - `jpeg_width` = A numeric value specifying the width of the figure
    (in inches) for JPEG output. 6 inches is automatic output.  
  - `jpeg_height` = A numeric value specifying the height of the figure
    (in inches) for JPEG output. 8 inches is automatic output.

``` r

create_hydrograph_faceted(
  all_hydro_sites_hist,
  all_hydro_sites_1yr,
  parameter = "flow",
  WY = FALSE, # calendar year
  output_type = "print", 
  fixed_y_scales = "fixed",
  custom_ymax_input = NA, 
  custom_ymin_input = NA
)
```

![](README_files/figure-gfm/hydrofacet-1.png)<!-- -->

### Additional tools

You can combine these tools with other R packages to create streamlined
workflows for hydrologic data analysis. For example, the `streamTrackR`
package complements `hydroGraphR` by enabling easy comparison of current
conditions across selected rivers.

### Summary

`hydroGraphR` simplifies hydrometric data analysis and visualization
with functions for downloading data, generating statistics, and creating
hydrographs. Whether working with individual rivers or a larger dataset,
the package offers flexibility for both exploratory and
presentation-ready outputs.

For further details, consult the package documentation
