rm(list=ls())
setwd("C:/Users/Jose/Desktop/RSpatialTutorial")
 
is.installed <- function(package) is.element(package, installed.packages())

if(!is.installed('rgdal'))
  install.packages('rgdal')
library(rgdal)

if(!is.installed('rgeos'))
  install.packages('rgeos')
library(rgeos)

if(!is.installed('tmap'))
  install.packages('tmap')
library(tmap)

if(!is.installed('dplyr'))
  install.packages('dplyr')
library(dplyr)

