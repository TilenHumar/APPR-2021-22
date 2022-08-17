library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(tibble)
library(dplyr)
library(readxl)
library(tidyverse)
library(XML)
library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(readr)
library(tidyr)
library(cluster)
library(ggalt)
library(GGally)
library(ranger)
library(iml)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
