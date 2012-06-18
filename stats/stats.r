# R-script for producing statistics output:
# Before runnning this script, makes sure you have the right packages installed:
# install.packages("ggplot2")

# Make sure needed libraries are there
require(ggplot2)

# Set some global variables
base_size <- 9

# Read in data
counts <- read.csv("counts.csv", header=TRUE)
handshake <- read.csv("handshake.csv", header=TRUE)
handshake <- transform(handshake, elapsed_ms = elapsed / 1000)

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

# Volcano plot of the kernel density estimate to compare time elapsed for
# handshakes
ws.plot.jitter <- function() {
    box <- ggplot(handshake, aes(factor(framework), elapsed_ms))
    (box + geom_jitter(alpha = 0.08)
       + xlab('Framework')
       + ylab('Handshake Time (ms)')
       + opts(axis.ticks = theme_blank(),
              axis.text.x = theme_text(size = base_size * 0.8,
                                       angle = 330,
                                       hjust = 0,
                                       colour = "grey50")))

#       + stat_density(aes(ymax = ..density.., ymin = -..density..),
#                      fill = "grey50", colour = "grey50",
#                      geom = "ribbon", position = "identity")
#       + facet_grid(. ~ framework) + coord_flip())
}

## BEGIN PLOTTING
# Conn timeout
pdf("stat_results/conn_timeouts.pdf")
ws.plot.conn_timeout()
dev.off()
png("stat_results/conn_timeouts.png")
ws.plot.conn_timeout()
dev.off()

# Jitter
pdf("stat_results/handshake_jitter.pdf")
ws.plot.jitter()
dev.off()
png("stat_results/handshake_jitter.png")
ws.plot.jitter()
dev.off()
