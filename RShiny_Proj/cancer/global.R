library(shinydashboard)
library(shiny)
library(DT)
library(googleVis)
library(ggthemes)
library(vcd)

options(scipen=20)

cancer <- read.csv(file = './Cancer.csv')
cancer$Age.Groups.Code <- substr(cancer$Age.Groups.Code, 1, 2)
cancer$Age.Groups.Code <- gsub('-', '', cancer$Age.Groups.Code)
cancer$Age.Groups.Code <- as.numeric(cancer$Age.Groups.Code)

cancer_stat <- data.frame(cancer[c(-2, -4, -5, -8, -10)])
#choice <- colnames(cancer_stat)[-1]
