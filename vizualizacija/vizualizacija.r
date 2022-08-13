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

library(readr)
library(XML)
library(tidyr)

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
placa_JV_slovenija = g1 %>% filter(regija == "Jugovzhodna Slovenija") %>% group_by(leto) %>% summarise('Jugovzhodna Slovenija' = mean(placa))
placa_osrednjeslovenska = g1 %>% filter(regija == "Osrednjeslovenska") %>% group_by(leto) %>% summarise(Osrednjeslovenska = mean(placa))
placa_gorenjska = g1 %>% filter(regija == "Gorenjska") %>% group_by(leto) %>% summarise(Gorenjska = mean(placa))
placa_primorska = g1 %>% filter(regija == "Primorsko-notranjska") %>% group_by(leto) %>% summarise('Primorsko notranjska' = mean(placa))
placa_goriška = g1 %>% filter(regija == "Goriška") %>% group_by(leto) %>% summarise(Goriška = mean(placa))
placa_obala = g1 %>% filter(regija == "Obalno-kraška") %>% group_by(leto) %>% summarise('Obalno kraška' = mean(placa))


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
  pivot_longer(., cols = c(Slovenija, Pomurska, Podravska, Koroška, Savinjska, Zasavska, Posavska, 'Jugovzhodna Slovenija', Osrednjeslovenska, Gorenjska, 'Primorsko notranjska', Goriška, 'Obalno kraška'), names_to = "Regija", values_to = "placa")

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
###GRAF 2: Vpliv spola in stopnje izobrazbe na višino plače v javnem in zasebnem sektorju med letoma 2008 in 2019

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
    title = "Vpliv spola in stopnje izobrazbe na višino plače v javnem in zasebnem sektorju med letoma 2008 in 2019"
  ) +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  ) +
  labs(caption = " Opomba: osnovna stopnja izobrazbe pomeni osnovnošolsko izobrazbo ali manj, srednja pomeni srednjošolsko in visoka višje ali visokošolsko izobrazbo. \n Vsaka točka prikazuje skupino oseb, združenih po spolu, sektorju zaposlitve in izobrazbi v določenem letu.") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###GRAF 3: Delovno aktivno prebivalstvo po dejavnostih med letoma 2008 in 2019

g3 = read_csv("izobrazba_spol_po_dejavnostih.csv")
g3 = dplyr::select(g3, c(leto, dejavnost, delovno_aktivno_prebivalstvo))
g3 = distinct(g3)

graf3 = g3 %>% ggplot(
  mapping = aes(fill = dejavnost, x = leto, y = delovno_aktivno_prebivalstvo, )
  ) +
  geom_bar(stat="identity",
           width = 0.5
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous("leto", labels = as.character(stevila), breaks = stevila) +
  theme_classic() +
  labs(
    x = "leto",
    y = "delovno aktivno prebivalstvo",
    title = "Delovno aktivno prebivalstvo po dejavnostih med letoma 2008 in 2019"
  ) +
  theme(axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  )

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###GRAF 4: Vpliv velikosti deleža aktivnega prebivalstva od celotne populacije na višino plače

g4 = read_csv("izobrazba_spol_po_dejavnostih.csv")
#g4 = dplyr::select(g4, c(leto, dejavnost, placa,  delovno_aktivni_kot_delez_populacije))
g4 = g4 %>% mutate(delovno_aktivni_kot_delez_populacije_v_odstotkih = round(100 * delovno_aktivni_kot_delez_populacije, 2)) %>% group_by(dejavnost, delovno_aktivni_kot_delez_populacije_v_odstotkih) %>% 
    summarise(placa = mean(placa)) %>% group_by(dejavnost) %>% summarise(placa = mean(placa), delovno_aktivni_kot_delez_populacije_v_odstotkih = mean(delovno_aktivni_kot_delez_populacije_v_odstotkih))

graf4 = g4 %>%
  ggplot(
    mapping = aes(x = delovno_aktivni_kot_delez_populacije_v_odstotkih, y = placa, color = dejavnost)
  ) +
  geom_point(
    position = position_jitter(width = 0.05),
    size = 7
  ) +
  theme_classic() +
  labs(
    x = "delovno aktivno prebivalstvo v odstotkih celotne populacije Slovenije",
    y = "višina plače v evrih",
    title = "Vpliv velikosti deleža aktivnega prebivalstva od celotne populacije na višino plače"
  ) +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  ) +
  labs(caption = " Opomba: za vsako dejavnost sta izračunana povprečen odstotek delovno aktivnega prebivalstva od celotne populacije in višina plače, med letoma 2008 in 2019.") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###GRAF 5: Vpliv števila študentov na 1000 prebivalcev v regijah na višino plače

g5 = read_csv("starost_spol_po_regijah.csv")
g5 = g5 %>% group_by(regija) %>% summarise(placa = mean(placa), `število študentov na 1000 prebivalcev` = mean(`število študentov na 1000 prebivalcev`))

graf5 = g5 %>%
  ggplot(
    mapping = aes(x = `število študentov na 1000 prebivalcev`, y = placa, color = regija)
  ) +
  geom_point(
    position = position_jitter(width = 0.05),
    size = 7
  ) +
  theme_classic() +
  labs(
    x = "število študentov na 1000 prebivalcev v regiji",
    y = "višina plače v evrih",
    title = "Vpliv števila študentov na 1000 prebivalcev v regijah na višino plače"
  ) +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  ) +
  labs(caption = " Opomba: za vsako regijo sta izračunana povprečno število študentov na 1000 prebivalcev in višina plače, med letoma 2008 in 2019.") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###GRAF 6: Primerjava gibanja prihodkov podjetij in plač v zasebnem sektorju med letoma 2009 in 2019

g6 =  read_csv("primerjava_prihodki_place.csv")
g6 = g6 %>% pivot_longer(cols = c(rel_sprememba_prihodka, rel_sprememba_place_zasebni_sektor),
                         values_to = "sprememba",
                         names_to = "kategorija"
                         ) %>% 
            mutate_all(funs(str_replace(., "rel_sprememba_prihodka", "relativna sprememba prihodka"))) %>%
            mutate_all(funs(str_replace(., "rel_sprememba_place_zasebni_sektor", "relativna sprememba plače")))
g6$sprememba = as.numeric(as.character(g6$sprememba))

graf6 = g6 %>% ggplot(
  mapping = aes(fill = kategorija, x = leto, y = sprememba)
) +
  geom_bar(stat="identity",
           width = 0.5,
           position = "dodge"
  ) +
  scale_y_continuous() +
  theme_classic() +
  labs(
    x = "leto",
    y = "sprememba v odstotkih",
    title = "Primerjava gibanja prihodkov podjetij in plač v zasebnem sektorju med letoma 2009 in 2019"
  ) +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
           axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
           plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ZEMLJEVIDI
source("lib/uvozi.zemljevid.r")

slovenija_regije <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1", encoding="UTF-8") %>% fortify()
colnames(slovenija_regije)[12]<-'regija'
slovenija_regije$regija = gsub('Notranjsko-kraška', 'Primorsko-notranjska', slovenija_regije$regija)
slovenija_regije$regija = gsub('Spodnjeposavska', 'Posavska', slovenija_regije$regija)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###ZEMLJEVID 1: Povprečne plače po statističnih regijah v letu 2019

z1 = read_csv("starost_spol_po_regijah.csv")
z1 = z1 %>% filter(leto == 2019) %>% group_by(regija) %>% summarise(placa = mean(placa))


zemljevid1 = ggplot() +
  geom_polygon(data = right_join(z1, slovenija_regije, by = "regija"),
               aes(x = long, y = lat, group = group, fill = placa))+
  ggtitle("Povprečne plače po statističnih regijah v letu 2019") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) +
  scale_fill_gradient(low = 'white', high = 'dark green') +
  labs(fill="Višina plače v evrih") +
  geom_path(data = right_join(z1, slovenija_regije,
                              by = "regija"), aes(x = long, y = lat, 
                                                  group = group), 
            color = "black", size = 0.1)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###ZEMLJEVID 2: Relativne spremembe povprečnih plač po statističnih regijah med letoma 2008 in 2019

placa_regije$Regija = gsub('Primorsko notranjska', 'Primorsko-notranjska', placa_regije$Regija)
placa_regije$Regija = gsub('Obalno kraška', 'Obalno-kraška', placa_regije$Regija)


z2 = placa_regije
z2 = z2 %>% group_by(Regija) %>% filter(leto == c(2008, 2019)) %>% filter(Regija != "Slovenija") %>% arrange(Regija) %>%
     mutate(rel_sprememba_povp_place = 100 * (placa - lag(placa))/lag(placa)) %>% filter(leto == 2019) %>% rename(regija = Regija)

zemljevid2 = ggplot() +
  geom_polygon(data = right_join(z2, slovenija_regije, by = "regija"),
               aes(x = long, y = lat, group = group, fill = rel_sprememba_povp_place))+
  ggtitle("Relativne spremembe povprečnih plač po statističnih regijah med letoma 2008 in 2019") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) +
  scale_fill_gradient(low = 'white', high = 'violet') +
  labs(fill="Sprememba povprečne plače v odstotkih") +
  geom_path(data = right_join(z2, slovenija_regije,
                              by = "regija"), aes(x = long, y = lat, 
                                                  group = group), 
            color = "black", size = 0.1)

zemljevid2
