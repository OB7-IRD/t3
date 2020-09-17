# How to install **T3**

## R package

The **T3 package** contains all methods for standardising the data and processing it in order to estimate :

- Nominal annual catch by species, region, gear, flag, separated between EEZ and High Seas, when its possible.
- Catch and fishing effort statistics for each species by small area (1x1 degree squares for most gears, 5x5 degree squares for longlines), gear, flag, and month. 
- Size frequencies of samples measured for each species by small area, gear, flag and month.

The package offers the possibilty to analyse data from three sources: RData file, CSV file and a dedicated database. The RData and CSV format are explained in the [User Guide](userguide.md). The installation of the database and its populatation are presented in the section below (see  "database and web application" section).

### R package from GitHub

To install **T3 package** from [Github](https://github.com/OB7-IRD/t3), the `devtools` package is necessary. So you need to run the commands below to *install*, *compile* and *load* the **T3 package**:
```R

install.packages("devtools")
# download the package from GitHub
devtools::install_github("https://github.com/OB7-IRD/t3")
# load the library
library(t3)
# package documentation
?t3
```

<!-- ### R package from CRAN -->

Once the package installed you can read the [tutorial](tutorial.md)

## Database and web application

As explained above, you can extract the data from a dedicated database. This section present how to install the database and the web application.

### Database's installation

#### Requirements
#### Step by step

### Web application's installation

#### Requirements
#### Step by step