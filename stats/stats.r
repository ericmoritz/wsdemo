# R-script for producing statistics output:
# Before runnning this script, makes sure you have the right packages installed:
# install.packages("ggplot2")

# Make sure needed libraries are there
require(ggplot2)

# Set some global variables
base_size <- 9

# Read in data
counts <- read.csv("counts.csv", header=TRUE)

# First plot, connection timeouts
pdf("stat_results/conn_timeouts.pdf")
conn_timeouts <- ggplot(counts, aes(x = type, y = connection_timeouts))
(conn_timeouts
   + geom_bar()
   + xlab('Framework')
   + ylab('Connections Lost')
   + opts(axis.ticks = theme_blank(),
          axis.text.x = theme_text(size = base_size * 0.8,
                                   angle = 330,
                                   hjust = 0,
                                   colour = "grey50")))
dev.off()

