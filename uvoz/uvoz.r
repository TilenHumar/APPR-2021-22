# 2. faza: Uvoz podatkov

library(readr)
library(dplyr)
library(XML)
library(tidyverse)
library(tidyr)

source("lib/libraries.r")
sl <- locale("sl", decimal_mark=",", grouping_mark=".")


### Prva tabela

#Pomožna tabela za preimenovanje spola
preimenovanje_spol = tibble(spol_uradno = c(
  "Spol - SKUPAJ",
  "Moški",
  "Ženske"),
  spol = c(
    "skupaj",
    "m",
    "ž")
  )

# Tabela ima v nekaterih poljih vrednost "z". To pomeni, da je podatek statistično zaščiten oziroma ga zaradi varovanja zaupnosti poročevalskih enot ne objavijo.
# Ta polja bom izpustil iz nadaljne analize, za zdaj pa sem jih nastavil na vrednost NA. Na NA sem nastavil tudi polja, kjer ni podatkov.


starost_spol_po_regijah = read_csv("podatki/povprecne_place_glede_na_spol_starost_po_regijah.csv", na=c("z","-"),locale=locale(encoding="Windows-1250"), skip = 1,
                                  col_names = c("bruto","regija","starost","spol_uradno","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                  col_types = cols(
                                    .default = col_guess(),
                                    bruto = col_skip(),
                                    spol_uradno = col_factor(),
                                    starost = col_factor(),
                                    regija = col_factor()
                                   ))

starost_spol_po_regijah = starost_spol_po_regijah %>%
                          pivot_longer(cols = colnames(starost_spol_po_regijah)[c(4:15)],
                            names_to = "leto",
                            values_to = "placa") %>%
                          left_join(preimenovanje_spol, by = "spol_uradno") %>%
                          relocate(spol_uradno, leto, regija, spol, starost, placa) %>% 
                          select(-spol_uradno) %>% 
                          filter(regija != "SLOVENIJA") %>%
                          filter(starost != "15-64 let") %>%
                          na.omit()

#preverimo, da so tipi stolpcev ustrezni
sapply(starost_spol_po_regijah, class)

#pretvorimo stolpec leto v numeričnega in stolpec spol v faktor
starost_spol_po_regijah$leto = as.numeric(as.character(starost_spol_po_regijah$leto))
starost_spol_po_regijah$spol = as.factor(starost_spol_po_regijah$spol)

starost_spol_po_regijah %>% write_csv("starost_spol_po_regijah.csv")

#POVPREČNA PLAČA PO LETIH
placa_slo = starost_spol_po_regijah %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))

#PLAČE PO SPOLU
placa_m = starost_spol_po_regijah %>% filter(spol != "ž") %>% group_by(leto) %>% summarise(placa_m = mean(placa))
placa_z = starost_spol_po_regijah %>% filter(spol != "m") %>% group_by(leto) %>% summarise(placa_z = mean(placa))

placa_spol = placa_slo %>% left_join(placa_m) %>% left_join(placa_z) %>%
                      mutate(odstopanje_m = placa_m - povrprecna_placa, odstopanje_z = placa_z - povrprecna_placa)

#PLAČE PO REGIJAH

placa_pomurska = starost_spol_po_regijah %>% filter(regija == "Pomurska") %>% group_by(leto) %>% summarise(placa_pomurska = mean(placa))
placa_podravska = starost_spol_po_regijah %>% filter(regija == "Podravska") %>% group_by(leto) %>% summarise(placa_podravska = mean(placa))
placa_koroska = starost_spol_po_regijah %>% filter(regija == "Koroška") %>% group_by(leto) %>% summarise(placa_koroska = mean(placa))
placa_savinjska = starost_spol_po_regijah %>% filter(regija == "Savinjska") %>% group_by(leto) %>% summarise(placa_savinjska = mean(placa))
placa_zasavska = starost_spol_po_regijah %>% filter(regija == "Zasavska") %>% group_by(leto) %>% summarise(placa_zasavska = mean(placa))
placa_posavska = starost_spol_po_regijah %>% filter(regija == "Posavska") %>% group_by(leto) %>% summarise(placa_posavska = mean(placa))
placa_JV_slovenija = starost_spol_po_regijah %>% filter(regija == "Jugovzhodna Slovenija") %>% group_by(leto) %>% summarise(placa_JV_slovenija = mean(placa))
placa_osrednjeslovenska = starost_spol_po_regijah %>% filter(regija == "Osrednjeslovenska") %>% group_by(leto) %>% summarise(placa_osrednjeslovenska = mean(placa))
placa_gorenjska = starost_spol_po_regijah %>% filter(regija == "Gorenjska") %>% group_by(leto) %>% summarise(placa_gorenjska = mean(placa))
placa_primorska = starost_spol_po_regijah %>% filter(regija == "Primorsko-notranjska") %>% group_by(leto) %>% summarise(placa_primorska = mean(placa))
placa_goriška = starost_spol_po_regijah %>% filter(regija == "Goriška") %>% group_by(leto) %>% summarise(placa_goriška = mean(placa))
placa_obala = starost_spol_po_regijah %>% filter(regija == "Obalno-kraška") %>% group_by(leto) %>% summarise(placa_obala = mean(placa))

placa_regije = placa_slo %>%
               left_join(placa_pomurska) %>%
               left_join(placa_podravska) %>%
               left_join(placa_koroska) %>%
               left_join(placa_savinjska) %>%
               left_join(placa_zasavska) %>%
               left_join(placa_posavska) %>%
               left_join(placa_JV_slovenija) %>%
               left_join(placa_osrednjeslovenska) %>%
               left_join(placa_gorenjska) %>%
               left_join(placa_primorska) %>%
               left_join(placa_goriška) %>%
               left_join(placa_obala)

#PLAČE PO STAROSTI

placa_15_24 = starost_spol_po_regijah %>% filter(starost == "15-24 let") %>% group_by(leto) %>% summarise(placa_15_24 = mean(placa))
placa_25_34 = starost_spol_po_regijah %>% filter(starost == "25-34 let") %>% group_by(leto) %>% summarise(placa_25_34 = mean(placa))
placa_35_44 = starost_spol_po_regijah %>% filter(starost == "35-44 let") %>% group_by(leto) %>% summarise(placa_35_44 = mean(placa))
placa_45_54 = starost_spol_po_regijah %>% filter(starost == "45-54 let") %>% group_by(leto) %>% summarise(placa_45_54 = mean(placa))
placa_55_64 = starost_spol_po_regijah %>% filter(starost == "55-64 let") %>% group_by(leto) %>% summarise(placa_55_64 = mean(placa))
placa_65plus = starost_spol_po_regijah %>% filter(starost == "65 let ali več") %>% group_by(leto) %>% summarise(placa_65plus = mean(placa))

placa_starost = placa_slo %>%
  left_join(placa_15_24) %>%
  left_join(placa_25_34) %>%
  left_join(placa_35_44) %>%
  left_join(placa_45_54) %>%
  left_join(placa_55_64) %>%
  left_join(placa_65plus)

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
    "kmetijstvo, lov, gozdarstvo in ribištvo",
    "rudarstvo",
    "predelovalne dejavnosti",
    "oskrba z električno energijo, plinom in paro",
    "oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja",
    "gradbeništvo",
    "trgovina, vzdrževanje in popravila motornih vozil",
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

#Pomožna tabela za preimenovanje stopnje izobrazbe
preimenovanje_izobrazba = tibble(izobrazba_uradno = c(
  "Izobrazba - SKUPAJ",
  "Osnovnošolska ali manj",
  "Srednješolska",
  "Višješolska, visokošolska"),
  izobrazba = c(
    "skupaj",
    "osnovna",
    "srednja",
    "visoka")
)


izobrazba_spol_po_dejavnostih = read_csv("podatki/povprecne_place_glede_na_dejavnost_izobrazbo_spol.csv",
                                        na="...", locale=locale(encoding="Windows-1250"),
                                        skip = 1,
                                        col_names = c("dejavnost_uradno", "izobrazba_uradno", "spol_uradno","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                        col_types= cols(.default = col_guess(),
                                                          spol_uradno = col_factor(),
                                                          dejavnost_uradno = col_factor(),
                                                          izobrazba_uradno = col_factor()
                                                          )
                                        )


izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih %>%
                                pivot_longer(cols = colnames(
                                  izobrazba_spol_po_dejavnostih)[c(4:15)],
                                  names_to = "leto",
                                  values_to = "placa") %>%
                                left_join(preimenovanje_dejavnosti, by = "dejavnost_uradno") %>%
                                left_join(preimenovanje_spol, by = "spol_uradno") %>% 
                                left_join(preimenovanje_izobrazba, by = "izobrazba_uradno") %>%
                                relocate(dejavnost_uradno, spol_uradno, izobrazba_uradno, leto, dejavnost, izobrazba, spol, placa) %>% 
                                select(-c(dejavnost_uradno, spol_uradno, izobrazba_uradno)) %>%
                                filter(dejavnost != "skupaj") %>%
                                filter(izobrazba != "skupaj") %>%
                                filter(spol != "skupaj")

                                                                                
#preverimo, da so tipi stolpcev ustrezni
sapply(izobrazba_spol_po_dejavnostih, class)

#pretvorimo stolpec leto v numeričnega, stolpce dejavnost, spol in izobrazba pa v faktor
izobrazba_spol_po_dejavnostih$leto = as.numeric(as.character(izobrazba_spol_po_dejavnostih$leto))
izobrazba_spol_po_dejavnostih$dejavnost = as.factor(izobrazba_spol_po_dejavnostih$dejavnost)
izobrazba_spol_po_dejavnostih$izobrazba = as.factor(izobrazba_spol_po_dejavnostih$izobrazba)
izobrazba_spol_po_dejavnostih$spol = as.factor(izobrazba_spol_po_dejavnostih$spol)


izobrazba_spol_po_dejavnostih %>% write_csv("izobrazba_spol_po_dejavnostih.csv")

#PLAČE PO IZOBRAZBI

placa_slo = izobrazba_spol_po_dejavnostih %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))

placa_osnovna = izobrazba_spol_po_dejavnostih %>% filter(izobrazba == "osnovna") %>% group_by(leto) %>% summarise(placa_osnovna = mean(placa))
placa_srednja = izobrazba_spol_po_dejavnostih %>% filter(izobrazba == "srednja") %>% group_by(leto) %>% summarise(placa_srednja = mean(placa))
placa_visoka = izobrazba_spol_po_dejavnostih %>% filter(izobrazba == "visoka") %>% group_by(leto) %>% summarise(placa_visoka = mean(placa))

placa_izobrazba = placa_slo %>%
  left_join(placa_osnovna) %>%
  left_join(placa_srednja) %>%
  left_join(placa_visoka)

### Tretja tabela

### Pretvorna tabela (iz občin v regije)
obcine_v_regije = read_csv("podatki/obcine-regije.csv")
obcine_v_regije %>% arrange(obcina)

prihodek_podjetij_po_obcinah = read_excel("podatki/prihodek_podjetij_po_obcinah.xlsx",
                                          skip = 5,
                                          na = "-",
                                          col_names = c("obcina","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
                                          )

#Izpustimo nepomembne vrstice
prihodek_podjetij_po_obcinah = head(prihodek_podjetij_po_obcinah, -43)



prihodek_podjetij_po_regijah = prihodek_podjetij_po_obcinah %>%
                               replace(is.na(.),0) %>%
                               right_join(obcine_v_regije, by = "obcina") %>%
                               pivot_longer(cols = colnames(prihodek_podjetij_po_obcinah)[c(2:13)],
                                            names_to = "leto",
                                            values_to = "prihodek") %>%
                               select(-obcina) %>%
                               group_by(regija, leto) %>%
                               summarise(prihodek = sum(prihodek)) %>%
                               relocate(leto, regija, prihodek)

#preverimo, da so tipi stolpcev ustrezni
sapply(prihodek_podjetij_po_regijah, class)

#pretvorimo stolpec leto v numeričnega, stolpec regija pa v faktor
prihodek_podjetij_po_regijah$leto = as.numeric(as.character(prihodek_podjetij_po_regijah$leto))
prihodek_podjetij_po_regijah$regija = as.factor(prihodek_podjetij_po_regijah$regija)

prihodek_podjetij_po_regijah %>% write_csv("prihodek_podjetij_po_regijah.csv")

#POVPREČEN PRIHODEK PO LETIH

prihodek_slo = prihodek_podjetij_po_regijah %>% group_by(leto) %>% summarise(povrprecen_prihodek = mean(prihodek))

#PRIHODEK PO REGIJAH

prihodek_pomurska = prihodek_podjetij_po_regijah %>% filter(regija == "Pomurska") %>% group_by(leto) %>% summarise(prihodek_pomurska = mean(prihodek))
prihodek_podravska = prihodek_podjetij_po_regijah %>% filter(regija == "Podravska") %>% group_by(leto) %>% summarise(prihodek_podravska = mean(prihodek))
prihodek_koroska = prihodek_podjetij_po_regijah %>% filter(regija == "Koroška") %>% group_by(leto) %>% summarise(prihodek_koroska = mean(prihodek))
prihodek_savinjska = prihodek_podjetij_po_regijah %>% filter(regija == "Savinjska") %>% group_by(leto) %>% summarise(prihodek_savinjska = mean(prihodek))
prihodek_zasavska = prihodek_podjetij_po_regijah %>% filter(regija == "Zasavska") %>% group_by(leto) %>% summarise(prihodek_zasavska = mean(prihodek))
prihodek_posavska = prihodek_podjetij_po_regijah %>% filter(regija == "Posavska") %>% group_by(leto) %>% summarise(prihodek_posavska = mean(prihodek))
prihodek_JV_slovenija = prihodek_podjetij_po_regijah %>% filter(regija == "Jugovzhodna Slovenija") %>% group_by(leto) %>% summarise(prihodek_JV_slovenija = mean(prihodek))
prihodek_osrednjeslovenska = prihodek_podjetij_po_regijah %>% filter(regija == "Osrednjeslovenska") %>% group_by(leto) %>% summarise(prihodek_osrednjeslovenska = mean(prihodek))
prihodek_gorenjska = prihodek_podjetij_po_regijah %>% filter(regija == "Gorenjska") %>% group_by(leto) %>% summarise(prihodek_gorenjska = mean(prihodek))
prihodek_primorska = prihodek_podjetij_po_regijah %>% filter(regija == "Primorsko-notranjska") %>% group_by(leto) %>% summarise(prihodek_primorska = mean(prihodek))
prihodek_goriška = prihodek_podjetij_po_regijah %>% filter(regija == "Goriška") %>% group_by(leto) %>% summarise(prihodek_goriška = mean(prihodek))
prihodek_obala = prihodek_podjetij_po_regijah %>% filter(regija == "Obalno-kraška") %>% group_by(leto) %>% summarise(prihodek_obala = mean(prihodek))

prihodek_regije = prihodek_slo %>%
  left_join(prihodek_pomurska) %>%
  left_join(prihodek_podravska) %>%
  left_join(prihodek_koroska) %>%
  left_join(prihodek_savinjska) %>%
  left_join(prihodek_zasavska) %>%
  left_join(prihodek_posavska) %>%
  left_join(prihodek_JV_slovenija) %>%
  left_join(prihodek_osrednjeslovenska) %>%
  left_join(prihodek_gorenjska) %>%
  left_join(prihodek_primorska) %>%
  left_join(prihodek_goriška) %>%
  left_join(prihodek_obala)




### Četrta tabela

izobrazba_spol_po_sektorjih = read_csv("podatki/povprecne_place_glede_na_izobrazbo_spol_po_sektorjih.csv", na="...",
                                       locale=locale(encoding="Windows-1250"),
                                       skip = 1,
                                       col_names = c("sektor_uradno", "izobrazba_uradno", "spol_uradno","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                       col_types= cols(.default = col_guess(),
                                                       spol_uradno = col_factor(),
                                                       izobrazba_uradno = col_factor()
                                                       )
                                       )

#Pomožna tabela za preimenovanje sektorjev
preimenovanje_sektorjev = tibble(sektor_uradno = c( 
  "1 Javni in zasebni sektor - SKUPAJ",
  "11 Javni sektor - SKUPAJ",
  "12 Zasebni sektor - SKUPAJ"),
  sektor = c("skupaj",
             "javni",
             "zasebni")
  )

#Popravek v pomožni tabeli za preimenovanje izobrazbe
preimenovanje_izobrazba[1,1] = "Izobrazba - Skupaj"

izobrazba_spol_po_sektorjih =  izobrazba_spol_po_sektorjih %>%
                               pivot_longer(cols = colnames(izobrazba_spol_po_sektorjih)[c(4:15)],
                                            names_to = "leto",
                                            values_to = "placa"
                                            ) %>%
                               left_join(preimenovanje_sektorjev, by = "sektor_uradno") %>%
                               left_join(preimenovanje_izobrazba, by = "izobrazba_uradno") %>%
                               left_join(preimenovanje_spol, by = "spol_uradno") %>%
                               relocate(sektor_uradno, spol_uradno, leto, sektor, izobrazba, spol, placa) %>%
                               select(-c(sektor_uradno, spol_uradno, izobrazba_uradno)) %>%
                               filter(sektor != "skupaj") %>%
                               filter(izobrazba != "skupaj") %>%
                               filter(spol != "skupaj")

#preverimo, da so tipi stolpcev ustrezni
sapply(izobrazba_spol_po_sektorjih, class)

#pretvorimo stolpec leto v numeričnega, stolpce sektor, izobrazba in spol pa v faktor
izobrazba_spol_po_sektorjih$leto = as.numeric(as.character(izobrazba_spol_po_sektorjih$leto))
izobrazba_spol_po_sektorjih$sektor = as.factor(izobrazba_spol_po_sektorjih$sektor)
izobrazba_spol_po_sektorjih$izobrazba = as.factor(izobrazba_spol_po_sektorjih$izobrazba)
izobrazba_spol_po_sektorjih$spol = as.factor(izobrazba_spol_po_sektorjih$spol)

izobrazba_spol_po_sektorjih %>% write_csv("izobrazba_spol_po_sektorjih.csv")

#PLAČE PO SEKTORJU
placa_slo = izobrazba_spol_po_sektorjih %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))

placa_javni = izobrazba_spol_po_sektorjih %>% filter(sektor == "javni") %>% group_by(leto) %>% summarise(placa_javni = mean(placa))
placa_zasebni = izobrazba_spol_po_sektorjih %>% filter(sektor == "zasebni") %>% group_by(leto) %>% summarise(placa_zasebni = mean(placa))

placa_sektor = placa_slo %>% left_join(placa_javni) %>% left_join(placa_zasebni)

#PLAČE PO SEKTORJU, SPOLU

placa_javni_sektor = izobrazba_spol_po_sektorjih %>% filter(sektor == "javni") %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))
placa_javni_zenske = izobrazba_spol_po_sektorjih %>% filter(sektor == "javni") %>% filter(spol == "z") %>% group_by(leto) %>% summarise(placa_javni_z = mean(placa))
placa_javni_moski = izobrazba_spol_po_sektorjih %>% filter(sektor == "javni") %>% filter(spol == "m") %>% group_by(leto) %>% summarise(placa_javni_m = mean(placa))

placa_javni_spol = placa_javni_sekto %>% left_join(placa_javni_moski) %>% left_join(placa_javni_zenske)


placa_zasebni_sektor = izobrazba_spol_po_sektorjih %>% filter(sektor == "zasebni") %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))
placa_zasebni_zenske = izobrazba_spol_po_sektorjih %>% filter(sektor == "zasebni") %>% filter(spol == "z") %>% group_by(leto) %>% summarise(placa_zasebni_z = mean(placa))
placa_zasebni_moski = izobrazba_spol_po_sektorjih %>% filter(sektor == "zasebni") %>% filter(spol == "m") %>% group_by(leto) %>% summarise(placa_zasebni_m = mean(placa))

placa_zasebni_spol = placa_zasebni_sekto %>% left_join(placa_zasebni_moski) %>% left_join(placa_zasebni_zenske)
