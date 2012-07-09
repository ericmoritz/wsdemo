# R-script for producing statistics output:

# Load all the necessary packages, installing missing ones when necessary
packages.to.install <- c("plyr", "ggplot2", "RPostgreSQL", "quantreg")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
    if (p == "ggplot2") suppressWarnings(library(ggplot2))
}

# Make sure needed libraries are there
require(ggplot2)
require(quantreg)
library(RPostgreSQL)

con <- dbConnect(dbDriver("PostgreSQL"), dbname = 'wsdemo')

read_times <- function(dbTable) {
    handshake <- dbReadTable(con, dbTable)
    transform(handshake,
              elapsed_ms = elapsed / 1000,
              timestamp_ms = timestamp / 1000,
              timestamp_s = timestamp / 1000000)
}
