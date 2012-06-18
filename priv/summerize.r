#!/usr/bin/env Rscript --vanilla

stats <- function(times) {
      elapsed = times$elapsed / 1000
      quant = quantile(elapsed, c(.75, .95, .99, .999))
      s <- c(mean(elapsed), 
             min(elapsed), 
             median(elapsed), 
             quant, 
             max(elapsed))
      names(s) <- c("Mean", "Min", "Median", names(quant), "Max")
      return(s)
}

root <- commandArgs(trailingOnly=T)[1]
handshake_times_csv = file.path(root, "handshake_times.csv")
latencies_csv = file.path(root, "message_latencies.csv")

handshakes <- read.csv(handshake_times_csv, head=T, sep=",", as.is=T, strip.white=T)
latencies <- read.csv(latencies_csv, head=T, sep=",", as.is=T, strip.white=T)

print(root)
print("Handshake Times")
print(stats(handshakes))
print("Latencies")
print(stats(latencies))
