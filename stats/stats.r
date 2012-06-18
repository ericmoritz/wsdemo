# R-script for producing statistics output:

# Load all the necessary packages, installing missing ones when necessary
packages.to.install <- c("plyr", "ggplot2")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
    if (p == "ggplot2") suppressWarnings(library(ggplot2))
}

# Make sure needed libraries are there
require(ggplot2)

# Set some global variables
base_size <- 9

# Read in data
counts <- read.csv("counts.csv", header=TRUE)
handshake <- read.csv("handshake.csv", header=TRUE)
handshake <- transform(handshake, elapsed_ms = elapsed / 1000)

#latencies <- read.csv("latencies.csv", header=TRUE)

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

# Jitter plot of the handshake times
ws.plot.jitter <- function(T) {
    box <- ggplot(T, aes(factor(framework), elapsed_ms))
    (box + geom_jitter(alpha = 0.08) + coord_trans(y = "log10")
       + xlab('Framework')
       + ylab('Handshake Time (ms)')
       + opts(axis.ticks = theme_blank(),
              axis.text.x = theme_text(size = base_size * 0.8,
                                       angle = 330,
                                       hjust = 0,
                                       colour = "grey50")))
}

ws.plot.box <- function(T) {
    box <- ggplot(T, aes(x = factor(framework), y = elapsed_ms))
    (box + geom_boxplot() + coord_trans(y = "log10")
       + xlab('Framework')
       + ylab('Handshake Time (ms)')
       + opts(axis.text.x = theme_text(size = base_size * 0.8,
                                       angle = 330,
                                       hjust = 0,
                                       colour = "grey50")))
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
# Conn timeout
pdf("stat_results/conn_timeouts.pdf")
ws.plot.conn_timeout()
dev.off()
png("stat_results/conn_timeouts.png")
ws.plot.conn_timeout()
dev.off()

# Handshake Jitter
pdf("stat_results/handshake_jitter.pdf")
ws.plot.jitter(handshake)
dev.off()
png("stat_results/handshake_jitter.png")
ws.plot.jitter(handshake)
dev.off()

# Handshake Volcano
pdf("stat_results/handshake_volcano.pdf", width=15)
ws.plot.volcano(handshake)
dev.off()

# Handshake Box
pdf("stat_results/handshake_box.pdf")
ws.plot.box(handshake)
dev.off()
png("stat_results/handshake_box.png")
ws.plot.box(handshake)
dev.off()

# Latencies Box
# pdf("stat_results/latencies_box.pdf")
# ws.plot.box(latencies)
# dev.off()
# png("stat_results/latencies_box.png")
# ws.plot.box(latencies)
# dev.off()

