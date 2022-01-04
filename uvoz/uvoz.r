# 2. faza: Uvoz podatkov
library(readr)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

### Pretvorna tabela (iz občin v regije)

obcine_v_regije = read_csv("podatki/obcine-regije.csv")
obcine_v_regije %>% arrange(obcina)

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
starost_spol_po_regijah = starost_spol_po_regijah %>% pivot_longer(cols = colnames(starost_spol_po_regijah)[c(4:15)],
                                                                   names_to = "leto",
                                                                   values_to = "placa"
                                                                   ) %>% relocate(leto,regija,spol,starost,placa)

### Druga tabela

izobrazba_spol_po_dejavnostih = read_csv("podatki/povprecne_place_glede_na_dejavnost_izobrazbo_spol.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                         col_names = c("dejavnost", "izobrazba", "spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                          col_types= cols(.default = col_guess(),
                                                          spol = col_factor(),
                                                          dejavnost = col_factor(),
                                                          izobrazba = col_factor()
                                                          ))
izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih %>% pivot_longer(cols = colnames(izobrazba_spol_po_dejavnostih)[c(4:15)],
                                                                               names_to = "leto",
                                                                               values_to = "placa"
                                                                               ) %>% relocate(leto,dejavnost,izobrazba,spol,placa)

### Tretja tabela

prihodek_podjetij_po_obcinah = read_excel("podatki/prihodek_podjetij_po_obcinah.xlsx", skip = 5, na = "-",
                                          col_names = c("obcina","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
prihodek_podjetij_po_obcinah = head(prihodek_podjetij_po_obcinah, -43)

prihodek_podjetij_po_regijah = prihodek_podjetij_po_obcinah %>% right_join(obcine_v_regije) %>% pivot_longer(cols = colnames(prihodek_podjetij_po_obcinah)[c(2:13)], names_to = "leto", values_to = "prihodek")
prihodek_podjetij_po_regijah = subset(prihodek_podjetij_po_regijah, select = -obcina)

### Četrta tabela

izobrazba_spol_po_sektorjih = read_csv("podatki/povprecne_place_glede_na_izobrazbo_spol_po_sektorjih.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                       col_names = c("sektor", "izobrazba", "spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                       col_types= cols(.default = col_guess(),
                                                       spol = col_factor(),
                                                       izobrazba = col_factor()
                                       ))
izobrazba_spol_po_sektorjih =  izobrazba_spol_po_sektorjih %>% pivot_longer(cols = colnames(izobrazba_spol_po_sektorjih)[c(4:15)],
                                           names_to = "leto",
                                           values_to = "placa"
                                           ) %>% relocate(leto, sektor, izobrazba, spol, placa)

