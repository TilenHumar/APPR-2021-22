# 2. faza: Uvoz podatkov
library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

### Prva tabela
# Tabela ima v nekaterih poljih vrednost "z". To pomeni, da je podatek statistično zaščiten oziroma ga zaradi varovanja zaupnosti poročevalskih enot ne objavijo. Ta polja bom izpustil iz nadaljne analize, za zdaj pa sem jih nastavil na vrednost NA.

starost_spol_po_regijah = read_csv("podatki/povprecne_place_glede_na_spol_starost_po_regijah.csv", na=c("z","-"),locale=locale(encoding="Windows-1250"),skip = 1,
                                    col_names = c("bruto","regija","starost","spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                   col_types = cols(
                                     .default = col_guess(),
                                     bruto = col_skip(),
                                     spol = col_factor(),
                                     starost = col_factor()
                                   ))

starost_spol_po_regijah = pivot_longer(starost_spol_po_regijah,
                                       cols = colnames(starost_spol_po_regijah)[c(4:15)],
                                       names_to = "leto",
                                       values_to = "placa")

starost_spol_po_regijah = starost_spol_po_regijah[, c(4,1,3,2,5)]
