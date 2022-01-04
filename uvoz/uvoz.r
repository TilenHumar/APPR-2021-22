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

starost_spol_po_regijah %>% write_csv("starost_spol_po_regijah.csv")

### Druga tabela

#Pomožna tabela za preimenovanje dejavnosti
preimenovanje_dejavnosti = tibble(dejavnost_uradno = c(
  "SKD DEJAVNOST - SKUPAJ",
  "A KMETIJSTVO IN LOV, GOZDARSTVO, RIBIŠTVO",
  "B RUDARSTVO",
  "C PREDELOVALNE DEJAVNOSTI",
  "D OSKRBA Z ELEKTRIČNO ENERGIJO, PLINOM IN PARO",
  "E OSKRBA Z VODO, RAVNANJE Z ODPLAKAMI IN ODPADKI, SANIRANJE OKOLJA",
  "F GRADBENIŠTVO",
  "G TRGOVINA, VZDRŽEVANJE IN POPRAVILA MOTORNIH VOZIL",
  "H PROMET IN SKLADIŠČENJE",
  "I GOSTINSTVO",
  "J INFORMACIJSKE IN KOMUNIKACIJSKE DEJAVNOSTI",
  "K FINANČNE IN ZAVAROVALNIŠKE DEJAVNOSTI",
  "L POSLOVANJE Z NEPREMIČNINAMI",
  "M STROKOVNE, ZNANSTVENE IN TEHNIČNE DEJAVNOSTI",
  "N DRUGE RAZNOVRSTNE POSLOVNE DEJAVNOSTI",
  "O DEJAVNOST JAVNE UPRAVE IN OBRAMBE, DEJAVNOST OBVEZNE SOCIALNE VARNOSTI",
  "P IZOBRAŽEVANJE",
  "Q ZDRAVSTVO IN SOCIALNO VARSTVO",
  "R KULTURNE, RAZVEDRILNE IN REKREACIJSKE DEJAVNOSTI",
  "S DRUGE DEJAVNOSTI"),
  
  dejavnost = c(
    "skupaj",
    "kmetijstvo, lov, gozdarstvno in ribištvo",
    "rudarstvo",
    "predelovalne dejavnosti",
    "oskrba z električno energijo, plinom in paro",
    "oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja",
    "gradbeništvo",
    "trgovina, vzdrževnaje in popravila motornih vozil",
    "promet in skladiščenje",
    "gostinstvo",
    "informacijske in komunikacijske dejavnosti",
    "finančne in zavarovalniške dejavnosti",
    "poslovanje z nepremičninami",
    "strokovne, znanstvene in tehnične dejavnosti",
    "druge poslovne dejavnosti",
    "dejavnost javne uprave in obrambe ter dejavnost obvezne socialne varnosti",
    "izobraževanje",
    "zdravstvo in socialno varstvo",
    "kulturne, razvedrilne in rekreacijske dejavnosti",
    "druge dejavnosti")
  )
    

izobrazba_spol_po_dejavnostih = read_csv("podatki/povprecne_place_glede_na_dejavnost_izobrazbo_spol.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                         col_names = c("dejavnost_uradno", "izobrazba", "spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                          col_types= cols(.default = col_guess(),
                                                          spol = col_factor(),
                                                          dejavnost_uradno = col_factor(),
                                                          izobrazba = col_factor()
                                                          ))


izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih %>% pivot_longer(cols = colnames(izobrazba_spol_po_dejavnostih)[c(4:15)],
                                                                               names_to = "leto",
                                                                               values_to = "placa"
                                                                               ) %>% left_join(preimenovanje_dejavnosti, by = "dejavnost_uradno") %>% relocate(dejavnost_uradno,leto,dejavnost,izobrazba,spol,placa)
izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih[-1]

izobrazba_spol_po_dejavnostih %>% write_csv("izobrazba_spol_po_dejavnostih.csv")

### Tretja tabela

prihodek_podjetij_po_obcinah = read_excel("podatki/prihodek_podjetij_po_obcinah.xlsx", skip = 5, na = "-",
                                          col_names = c("obcina","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
prihodek_podjetij_po_obcinah = head(prihodek_podjetij_po_obcinah, -43)

prihodek_podjetij_po_regijah = prihodek_podjetij_po_obcinah %>% right_join(obcine_v_regije, by = "obcina") %>% pivot_longer(cols = colnames(prihodek_podjetij_po_obcinah)[c(2:13)], names_to = "leto", values_to = "prihodek")

prihodek_podjetij_po_regijah = subset(prihodek_podjetij_po_regijah, select = -obcina)

prihodek_podjetij_po_regijah[is.na(prihodek_podjetij_po_regijah)] = 0 # Z vrednostjo NA ne moremo računati, zato jo nastavimo na 0

prihodek_podjetij_po_regijah = prihodek_podjetij_po_regijah %>% group_by(regija, leto) %>% summarise(prihodek = sum(prihodek))

prihodek_podjetij_po_regijah %>% write_csv("prihodek_podjetij_po_regijah.csv")

### Četrta tabela

izobrazba_spol_po_sektorjih = read_csv("podatki/povprecne_place_glede_na_izobrazbo_spol_po_sektorjih.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                       col_names = c("sektor_uradno", "izobrazba", "spol","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                       col_types= cols(.default = col_guess(),
                                                       spol = col_factor(),
                                                       izobrazba = col_factor()
                                       ))

#Pomožna tabela za preimenovanje sektorjev
preimenovanje_sektorjev = tibble(sektor_uradno = c( "1 Javni in zasebni sektor - SKUPAJ", "11 Javni sektor - SKUPAJ", "12 Zasebni sektor - SKUPA"), sektor = c("javni in zasebni sektor", "javni sektor", "zasebni sektor"))

izobrazba_spol_po_sektorjih =  izobrazba_spol_po_sektorjih %>% pivot_longer(cols = colnames(izobrazba_spol_po_sektorjih)[c(4:15)],
                                           names_to = "leto",
                                           values_to = "placa"
                                           )  %>% left_join(preimenovanje_sektorjev, by = "sektor_uradno")  %>% relocate(sektor_uradno,leto, sektor, izobrazba, spol, placa)

izobrazba_spol_po_sektorjih = izobrazba_spol_po_sektorjih[-1]
 
izobrazba_spol_po_sektorjih %>% write_csv("izobrazba_spol_po_sektorjih.csv")
