#!/usr/bin/env Rscript

# initialization
cat("\014")
rm(list = ls())
require(ggplot2)
theme_set(theme_grey(base_size = 25))
cat("Loading helper scripts.\n")
source('./helpers/params.r')
source('./helpers/gaussian.r')
source('./helpers/threshold.r')

# load 2D dataset
cat("\nLoading 2D dataset.\n")
data <- read.csv("./data/X1.csv")
val.x <- read.csv("./data/XVal1.csv")
val.y <- read.csv("./data/YVal1.csv")

# visualize dataset
cat("Visualizing dataset.\n")
plot <- cbind(data, Legend = factor(c("TRUE")))
png("./plots/latency.png", height = 1000, width = 1000)
ggplot(plot, aes(x = Latency, fill = Legend)) + 
    geom_histogram(breaks = seq(1, 25, by = 0.5), color="royalblue4", alpha = 0.5) + 
    scale_x_continuous(breaks = seq(2, 24, by = 2)) + 
    scale_y_continuous(breaks = seq(0, 65, by = 10)) + 
    scale_fill_manual(values=c("royalblue1"), labels = c("Latency Frequency"), name = "Legend\n") +
    theme(legend.position = c(0.85, 0.9), legend.title.align = 0.5, legend.background = element_rect(fill = alpha("white", 0.0)), 
          legend.title = element_text(face = "bold", lineheight = 0.4)) +
    labs(x = "Latency (ms)", y = "Frequency")
graphics.off()
png("./plots/throughput.png", height = 1000, width = 1000)
ggplot(plot, aes(x = Throughput, fill = Legend)) + 
    geom_histogram(breaks = seq(1, 25, by = 0.5), color="royalblue4", alpha = 0.5) + 
    scale_x_continuous(breaks = seq(2, 24, by = 2)) + 
    scale_y_continuous(breaks = seq(0, 65, by = 10)) + 
    scale_fill_manual(values=c("royalblue1"), labels = c("Throughput Frequency"), name = "Legend\n") +
    theme(legend.position = c(0.85, 0.9), legend.title.align = 0.5, legend.background = element_rect(fill = alpha("white", 0.0)), 
          legend.title = element_text(face = "bold", lineheight = 0.4)) +
    labs(x = "Throughput (mb/s)", y = "Frequency")
graphics.off()

# estimate dataset statistics
cat("Fitting gaussian model on dataset.\n")
params <- Params(data)
mu <- unlist(params[1])
sigma <- unlist(params[2])
rm(params)
p <- Gaussian(data, mu, sigma)

# select threshold (ε)
val.p <- Gaussian(val.x, mu, sigma)
params <- Threshold(val.y, val.p)
ε <- unlist(params[1])
f1.score <- unlist(params[2])
rm(params)
cat("Best threshold found on cross-validation set: ", format(ε, digits = 5))
cat("\nBest F1 score on cross-validation set: ", format(f1.score, digits = 4))

# find outliers
cat("\nVisualizing outliers.\n")
plot$Legend <- p<ε
png("./plots/outliers.png", height = 1000, width = 1000)
ggplot(plot, aes(x = Latency, y = Throughput, color = Legend)) + geom_point(size = 6) + 
    scale_color_manual(values=c("royalblue1", "red"), labels = c(" Non-Anomalous Server\n", " Anomalous Server"), name = "Legend\n") +
    theme(legend.position = c(0.82, 0.92), legend.title.align = 0.5, legend.background = element_rect(fill = alpha("white", 0.0)), 
          legend.title = element_text(face = "bold", lineheight = 0.5), legend.text = element_text(lineheight = 0.3), 
          legend.key = element_rect(color = alpha("white", 0.0), fill = alpha("white", 0.0))) +
    labs(x = "Latency (ms)", y = "Throughput (mb/s)")
graphics.off()
cat(sum(p<ε), "anomalies found in 2D dataset!\n")

# high dimensional dataset
cat("\nLoading 11D dataset.\n")
data <- read.csv("./data/X2.csv", header = FALSE)
val.x <- read.csv("./data/XVal2.csv", header = FALSE)
val.y <- read.csv("./data/YVal2.csv", header = FALSE)
params <- Params(data)
mu <- unlist(params[1])
sigma <- unlist(params[2])
rm(params)
cat("Fitting gaussian model on dataset.\n")
p <- Gaussian(data, mu, sigma)
val.p <- Gaussian(val.x, mu, sigma)
params <- Threshold(val.y, val.p)
ε <- unlist(params[1])
f1.score <- unlist(params[2])
cat("Best threshold found on cross-validation set: ", format(ε, digits = 5))
cat("\nBest F1 score on cross-validation set: ", format(f1.score, digits = 4), "\n")
cat(sum(p<ε), "anomalies found in 11D dataset!\n\n")
rm(params)