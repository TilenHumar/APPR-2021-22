# 2. faza: Uvoz podatkov
library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

### Prva tabela

# Tabela ima v nekaterih poljih vrednost "z". To pomeni, da je podatek statistično zaščiten oziroma ga zaradi varovanja zaupnosti poročevalskih enot ne objavijo.
# Ta polja bom izpustil iz nadaljne analize, za zdaj pa sem jih nastavil na vrednost NA. Na NA sem nastavil tudi polja, kjer ni podatkov.


starost_spol_po_regijah = read_csv("podatki/povprecne_place_glede_na_spol_starost_po_regijah.csv", na=c("z","-"),locale=locale(encoding="Windows-1250"), skip = 1,
                                    col_names = c("bruto","regija","starost","spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                   col_types = cols(
                                     .default = col_guess(),
                                     bruto = col_skip(),
                                     spol = col_factor(),
                                     starost = col_factor(),
                                     regija = col_factor()
                                   ))

starost_spol_po_regijah = pivot_longer(starost_spol_po_regijah,
                                       cols = colnames(starost_spol_po_regijah)[c(4:15)],
                                       names_to = "leto",
                                       values_to = "placa")

starost_spol_po_regijah = starost_spol_po_regijah[, c(4,1,3,2,5)]

### Druga tabela

izobrazba_spol_po_dejavnostih = read_csv("podatki/povprecne_place_glede_na_dejavnost_izobrazbo_spol.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                         col_names = c("dejavnost", "izobrazba", "spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                          col_types= cols(.default = col_guess(),
                                                          spol = col_factor(),
                                                          dejavnost = col_factor(),
                                                          izobrazba = col_factor()
                                                          ))
izobrazba_spol_po_dejavnostih = pivot_longer(izobrazba_spol_po_dejavnostih,cols = colnames(izobrazba_spol_po_dejavnostih)[c(4:15)],
                                             names_to = "leto",
                                             values_to = "placa")
izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih[, c(4,1,2,3,5)]

### Tretja tabela

prihodek_podjetij_po_obcinah = read_excel("podatki/prihodek_podjetij_po_obcinah.xlsx", skip = 4, na = "-",
                                          col_names = c("obcina","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
prihodek_podjetij_po_obcinah = pivot_longer(prihodek_podjetij_po_obcinah,cols = colnames(prihodek_podjetij_po_obcinah)[-1], names_to = "leto", values_to = "prihodek")


### Četrta tabela

izobrazba_spol_po_sektorjih = read_csv("podatki/povprecne_place_glede_na_izobrazbo_spol_po_sektorjih.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                       col_names = c("sektor", "izobrazba", "spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                       col_types= cols(.default = col_guess(),
                                                       spol = col_factor(),
                                                       izobrazba = col_factor()
                                       ))
izobrazba_spol_po_sektorjih = pivot_longer(izobrazba_spol_po_sektorjih, cols = colnames(izobrazba_spol_po_sektorjih)[c(4:15)],
                                           names_to = "leto",
                                           values_to = "placa")
izobrazba_spol_po_sektorjih = izobrazba_spol_po_sektorjih[, c(4,1,2,3,5)]