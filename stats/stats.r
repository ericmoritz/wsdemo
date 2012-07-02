# R-script for producing statistics output:

# Load all the necessary packages, installing missing ones when necessary
packages.to.install <- c("plyr", "ggplot2", "RPostgreSQL")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
    if (p == "ggplot2") suppressWarnings(library(ggplot2))
}

# Make sure needed libraries are there
require(ggplot2)
library(RPostgreSQL)

# Set some global variables
base_size <- 9

# Read in data
con <- dbConnect(dbDriver("PostgreSQL"), dbname = 'wsdemo')

counts <- read.csv("counts.csv", header=TRUE)
handshake <- dbReadTable(con, "handshakes_skew")
handshake <- transform(handshake, elapsed_ms = elapsed / 1000)

latencies <- dbReadTable(con, "latencies_small")
latencies <- transform(latencies, elapsed_ms = elapsed / 1000)

options <- opts(axis.text.x = theme_text(size = base_size * 0.8,
                                         angle = 330,
                                         hjust = 0,
                                         colour = "grey50"),
                axis.text.x = theme_text(size = base_size * 0.5),
                axis.text.y = theme_text(size = base_size * 0.5),
                strip.text.x = theme_text(size = base_size * 0.5))


# A plot of the connection timeouts as a function
ws.plot.conn_timeout <- function() {
    conn_timeouts <- ggplot(counts, aes(x = framework, y = connection_timeouts))
    (conn_timeouts
       + geom_bar()
       + xlab('Framework')
       + ylab('Connections Lost')
       + options)
}

ws.plot.bin2d <- function(T, YLab) {
   dots <- ggplot(T, aes(x=timestamp/1000000, y=elapsed/1000))
   (dots
     + facet_wrap(~ framework) 
     + ylab(YLab)
     + xlab('Second')
     + stat_bin2d()
     + options)
}

ws.plot.box <- function(T, Y) {
    box <- ggplot(T, aes(x = factor(framework), y = elapsed/1000))
    (box 
       + geom_boxplot(outlier.shape = NA, alpha=0.5)
       + scale_y_log10()
       + xlab('Framework')
       + ylab(Y)
       + options)
}

ws.plot.histogram <- function(T, Xlab) {
    x <- ggplot(T, aes(x = elapsed_ms))
    (x + geom_histogram()
       + xlab(Xlab)
       + scale_x_log10()
       + facet_wrap(~ framework) 
       + options)
}

## BEGIN PLOTTING
pdf("stat_results/results.pdf")

ws.plot.conn_timeout()

ws.plot.box(handshake, 'Handshake time (ms)')
ws.plot.histogram(handshake, 'Handshake time (ms)')
ws.plot.bin2d(handshake, 'Handshake time (ms)')

ws.plot.box(latencies, 'Message latency (ms)')
ws.plot.histogram(latencies, 'Message latency (ms)')
ws.plot.bin2d(latencies, 'Message latency (ms)')

dev.off()

