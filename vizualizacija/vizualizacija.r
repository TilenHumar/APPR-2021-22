# 3. faza: Vizualizacija podatkov

library(tidyverse)
library(ggplot2)
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(tmap)
library("scales")

source("uvoz/uvoz.r", encoding="UTF-8")

###GRAF 1: povprečne plače v statističnih regijah med leti 2008 in 2019

g1 = read_csv("starost_spol_po_regijah.csv")

#povprečna plača
povprecna_plača = g1 %>% group_by(leto) %>% summarise(Slovenija = mean(placa))

#plača po regijah
placa_pomurska = g1 %>% filter(regija == "Pomurska") %>% group_by(leto) %>% summarise(Pomurska = mean(placa))
placa_podravska = g1 %>% filter(regija == "Podravska") %>% group_by(leto) %>% summarise(Podravska = mean(placa))
placa_koroska = g1 %>% filter(regija == "Koroška") %>% group_by(leto) %>% summarise(Koroška = mean(placa))
placa_savinjska = g1 %>% filter(regija == "Savinjska") %>% group_by(leto) %>% summarise(Savinjska = mean(placa))
placa_zasavska = g1 %>% filter(regija == "Zasavska") %>% group_by(leto) %>% summarise(Zasavska = mean(placa))
placa_posavska = g1 %>% filter(regija == "Posavska") %>% group_by(leto) %>% summarise(Posavska = mean(placa))
placa_JV_slovenija = g1 %>% filter(regija == "Jugovzhodna Slovenija") %>% group_by(leto) %>% summarise(Jugovzhodna_Slovenija = mean(placa))
placa_osrednjeslovenska = g1 %>% filter(regija == "Osrednjeslovenska") %>% group_by(leto) %>% summarise(Osrednjeslovenska = mean(placa))
placa_gorenjska = g1 %>% filter(regija == "Gorenjska") %>% group_by(leto) %>% summarise(Gorenjska = mean(placa))
placa_primorska = g1 %>% filter(regija == "Primorsko-notranjska") %>% group_by(leto) %>% summarise(Primorsko_notranjska = mean(placa))
placa_goriška = g1 %>% filter(regija == "Goriška") %>% group_by(leto) %>% summarise(Goriška = mean(placa))
placa_obala = g1 %>% filter(regija == "Obalno-kraška") %>% group_by(leto) %>% summarise(Obalno_kraška = mean(placa))


placa_regije = povprecna_plača %>%
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
  left_join(placa_obala) %>%
  pivot_longer(., cols = c(Slovenija, Pomurska, Podravska, Koroška, Savinjska, Zasavska, Posavska, Jugovzhodna_Slovenija, Osrednjeslovenska, Gorenjska, Primorsko_notranjska, Goriška, Obalno_kraška), names_to = "Regija", values_to = "placa")

stevila = 2008:2019


graf1 = placa_regije %>% filter(Regija != "Slovenija") %>%
  ggplot(
    mapping = aes(x = leto, y = placa, color = Regija)
  ) +
  geom_line(size = 2) +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous("leto", labels = as.character(stevila), breaks = stevila) +
  theme_classic() +
  labs(
    x = "leto",
    y = "višina plače v evrih",
    title = "Povprečne plače po statističnih regijah med letoma 2008 in 2019"
  ) +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

graf1 = graf1 +
  placa_regije %>%
  filter(Regija == "Slovenija") %>%
  geom_line(
    mapping = aes(x = leto, y = placa, color = Regija),
    size = 3, linetype = "dashed", color = "black"
  ) +
  labs(caption = "Črna, črtkana črta predstavlja višino povprečne plače v danem letu") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###GRAF 2: vpliv spola in stopnje izobrazbe na višino plače v javnem in zasebnem sektorju med letoma 2008 in 2019

g2 = read_csv("izobrazba_spol_po_sektorjih.csv")

graf2 = g2 %>%
  ggplot(
    mapping = aes(x = izobrazba, y = placa, color = sektor, shape = spol)
  ) +
  geom_point(
    position = position_jitter(width = 0.05),
    size = 3
  ) +
  theme_classic() +
  labs(
    x = "stopnja izobrazbe",
    y = "povprečna višina plače v evrih za pripadajočo skupino",
    title = "vpliv spola in stopnje izobrazbe na višino plače v javnem in zasebnem sektorju med letoma 2008 in 2019"
  ) +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  ) +
  labs(caption = " Opomba: osnovna stopnja izobrazbe pomeni osnovnošolsko izobrazbo ali manj, srednja pomeni srednjošolsko in visoka višje ali visokošolsko izobrazbo. \n Vsaka točka prikazuje skupino oseb, združenih po spolu, sektorju zaposlitve in izobrazbi v določenem letu.") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))

graf2
  