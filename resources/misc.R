library(xtable)
library(reshape)
library(heplots)
library(vcd)
library(ggplot2)
library(combinat)
library(car)
library(rms)

Cramer <- function(v){
    if(v < .01){
        v <- "Negligible"
    }else if(v < .2){
        v <- "Weak"
    }else if(v < .4){
        v <- "Moderate"
    }else if(v < .6){
        v <- "Relatively strong"
    }else if(v < .8){
        v <- "Strong"
    }else{
        v <- "Very strong"
    }
}


data <- read.csv("rawdata.csv")
data$rowid <- NULL
data$PaperId <- NULL
data$AuthorGrp <- factor(data$AuthorGrp)
data$Year <- NULL
indep <- c("ResearcherGroup","ClassifierFamily","DatasetFamily","MetricFamily")
colnames(data) <- c(indep,"MCC")

