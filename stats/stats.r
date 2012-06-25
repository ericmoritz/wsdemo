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

# A plot of the connection timeouts as a function
ws.plot.conn_timeout <- function() {
    conn_timeouts <- ggplot(counts, aes(x = framework, y = connection_timeouts))
    (conn_timeouts
       + geom_bar()
       + xlab('Framework')
       + ylab('Connections Lost')
       + opts(axis.ticks = theme_blank(),
              axis.text.x = theme_text(size = base_size * 0.8,
                                       angle = 330,
                                       hjust = 0,
                                       colour = "grey50")))
}

ws.plot.box <- function(T, Y) {
    box <- ggplot(T, aes(x = factor(framework), y = elapsed_ms))
    (box + geom_jitter(alpha = 0.2, size = 0.9) + geom_boxplot(outlier.shape = NA, alpha=0.5) + coord_trans(y = "log10")
       + xlab('Framework')
       + ylab(Y)
       + opts(axis.text.x = theme_text(size = base_size * 0.8,
                                       angle = 330,
                                       hjust = 0,
                                       colour = "grey50")))
}

ws.plot.density <- function(T, Xlab) {
    x <- ggplot(T, aes(x = elapsed_ms))
    (x + geom_density()
       + xlab(Xlab)
       + facet_grid(framework ~ .) + scale_x_log10()
       + opts(strip.text.y = theme_text()))
}

# Volcano
ws.plot.volcano <- function(T) {
    v <- ggplot(T, aes(x = log10(elapsed_ms)))
    (v      + stat_density(aes(ymax = ..density..,  ymin = -..density..),
                           fill = "grey50",
                           colour = "grey50",
                           geom = "ribbon",
                           position = "identity")
            + facet_grid(. ~ framework)
            + coord_flip())
}

## BEGIN PLOTTING
pdf("stat_results/results.pdf")
ws.plot.conn_timeout()
#ws.plot.box(handshake, 'Handshake time (ms)')
ws.plot.density(latencies, 'Message Latency (ms)')
#ws.plot.box(latencies, 'Message latency (ms)')
dev.off()

