library(readr)
library(ggplot2)
library(dplyr)
library(tools)
library(GGally)
library(RColorBrewer)
library(shinyWidgets)
library(tidyr)
library(quantreg)


#colorblind friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")

#ercot_data <- read_csv("data/ERCOT_Dataset_8June2021.csv", na = ".")

#ercot_ts <- read.zoo("data/ercot_dataset_zoo.csv", header = TRUE, sep=",", aggregate = function(x) tail(x, 1))

ercot_ts <- read.csv("data/ercot_dataset_zoo.csv")
ercot_ts <- ercot_ts[!duplicated(ercot_ts$Date),]
ercot_ts <- na.fill0(ercot_ts,0)
