# 2. faza: Uvoz podatkov

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
  dplyr::select(-spol_uradno) %>% 
  filter(regija != "SLOVENIJA") %>%
  filter(starost != "15-64 let") %>%
  na.omit()

#preverimo, da so tipi stolpcev ustrezni
sapply(starost_spol_po_regijah, class)

#pretvorimo stolpec leto v numeričnega in stolpec spol v faktor
starost_spol_po_regijah$leto = as.numeric(as.character(starost_spol_po_regijah$leto))
starost_spol_po_regijah$spol = as.factor(starost_spol_po_regijah$spol)


st_studentov_na_1000 = read_csv("podatki/st_studentov_na_1000_po_regijah.csv", na=c("z","-"),locale=locale(encoding="Windows-1250"), skip = 1,
                                col_names = c("regija", "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                col_types = cols(
                                  .default = col_guess(),
                                  regija = col_factor()
                                ))
st_studentov_na_1000 = st_studentov_na_1000 %>% pivot_longer(cols = colnames(st_studentov_na_1000)[c(2:13)], names_to = "leto", values_to = "število študentov na 1000 prebivalcev")
st_studentov_na_1000$leto = as.numeric(as.character(st_studentov_na_1000$leto))

starost_spol_po_regijah = starost_spol_po_regijah %>% left_join(st_studentov_na_1000, by = c("regija", "leto"))

starost_spol_po_regijah %>% write_csv("starost_spol_po_regijah.csv")

#POVPREČNA PLAČA PO LETIH
placa_slo = starost_spol_po_regijah %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))
spremembe_placa_slo = placa_slo %>% mutate(abs_sprememba = povrprecna_placa - lag(povrprecna_placa)) %>% mutate(rel_sprememba = (povrprecna_placa - lag(povrprecna_placa))/lag(povrprecna_placa) )

#PLAČE PO SPOLU
placa_m = starost_spol_po_regijah %>% filter(spol != "ž") %>% group_by(leto) %>% summarise(placa_m = mean(placa))
placa_z = starost_spol_po_regijah %>% filter(spol != "m") %>% group_by(leto) %>% summarise(placa_z = mean(placa))

placa_spol = placa_slo %>% left_join(placa_m) %>% left_join(placa_z) %>%
  mutate(odstopanje_m = placa_m - povrprecna_placa, odstopanje_z = placa_z - povrprecna_placa)

#pobrišimo nepotrebne tabele
rm(placa_m, placa_z, spremembe_placa_slo, st_studentov_na_1000)

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

#pobrišimo nepotrebne tabele
rm(placa_15_24, placa_25_34, placa_35_44, placa_45_54, placa_55_64, placa_65plus)


### Druga tabela

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
  filter(dejavnost_uradno  != "SKD DEJAVNOST - SKUPAJ") %>%
  left_join(preimenovanje_spol, by = "spol_uradno") %>% 
  left_join(preimenovanje_izobrazba, by = "izobrazba_uradno") %>%
  relocate(leto, dejavnost_uradno, spol_uradno, izobrazba_uradno, izobrazba, spol, placa) %>% 
  dplyr::select(-c(spol_uradno, izobrazba_uradno)) %>%
  filter(izobrazba != "skupaj") %>%
  filter(spol != "skupaj") %>%
  rename(dejavnost = dejavnost_uradno)

crka = "^([:alpha:]{1}\\s*)"
izobrazba_spol_po_dejavnostih$dejavnost = izobrazba_spol_po_dejavnostih$dejavnost %>% str_replace(crka, "") %>% str_to_lower()

#preverimo, da so tipi stolpcev ustrezni
sapply(izobrazba_spol_po_dejavnostih, class)

#pretvorimo stolpec leto v numeričnega, stolpce dejavnost, spol in izobrazba pa v faktor
izobrazba_spol_po_dejavnostih$leto = as.numeric(as.character(izobrazba_spol_po_dejavnostih$leto))
izobrazba_spol_po_dejavnostih$dejavnost = as.factor(izobrazba_spol_po_dejavnostih$dejavnost)
izobrazba_spol_po_dejavnostih$izobrazba = as.factor(izobrazba_spol_po_dejavnostih$izobrazba)
izobrazba_spol_po_dejavnostih$spol = as.factor(izobrazba_spol_po_dejavnostih$spol)


st_delovno_aktivnih_po_dejavnostih = read_excel("podatki/st_delovno_aktivnih_po_dejavnostih.xlsx",
                                                skip = 3,
                                                na = "-",
                                                col_names = c("dejavnost", "število delovno aktivnega prebivalstva", "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
)
#Izpustimo nepomembne vrstice
st_delovno_aktivnih_po_dejavnostih = head(st_delovno_aktivnih_po_dejavnostih, -32)

st_delovno_aktivnih_po_dejavnostih = st_delovno_aktivnih_po_dejavnostih %>% dplyr::select(-2) %>% pivot_longer(cols = colnames(st_delovno_aktivnih_po_dejavnostih)[c(3:14)], names_to = "leto", values_to = "delovno_aktivno_prebivalstvo")

st_delovno_aktivnih_po_dejavnostih$dejavnost = st_delovno_aktivnih_po_dejavnostih$dejavnost %>% str_replace(crka, "") %>% str_to_lower()
st_delovno_aktivnih_po_dejavnostih$leto = as.numeric(as.character(st_delovno_aktivnih_po_dejavnostih$leto))


prebivalstvo_slo = read_csv("podatki/prebivalstvo_slo.csv", na="...", locale=locale(encoding="Windows-1250"),
                            col_names = c("slovenija", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                            col_types= cols(.default = col_guess())
)
prebivalstvo_slo = prebivalstvo_slo %>% filter(!row_number() %in% c(1)) %>% pivot_longer(cols = colnames(
  prebivalstvo_slo)[c(2:13)],
  names_to = "leto",
  values_to = "prebivalstvo_Slovenije"
)
prebivalstvo_slo$leto = as.numeric(as.character(prebivalstvo_slo$leto))
prebivalstvo_slo$prebivalstvo_Slovenije = as.numeric(as.character(prebivalstvo_slo$prebivalstvo_Slovenije))

izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih %>% left_join(st_delovno_aktivnih_po_dejavnostih, by = c("leto", "dejavnost")) %>% left_join(prebivalstvo_slo, by = "leto") %>%
  dplyr::select(-slovenija) %>% mutate(delovno_aktivni_kot_delez_populacije = delovno_aktivno_prebivalstvo/prebivalstvo_Slovenije) %>% dplyr::select(-prebivalstvo_Slovenije)

#pobrišimo nepotrebne tabele
rm(st_delovno_aktivnih_po_dejavnostih, prebivalstvo_slo)

#PLAČE PO IZOBRAZBI

placa_slo = izobrazba_spol_po_dejavnostih %>% group_by(leto) %>% summarise(povrprecna_placa = mean(placa))

placa_osnovna = izobrazba_spol_po_dejavnostih %>% filter(izobrazba == "osnovna") %>% group_by(leto) %>% summarise(placa_osnovna = mean(placa))
placa_srednja = izobrazba_spol_po_dejavnostih %>% filter(izobrazba == "srednja") %>% group_by(leto) %>% summarise(placa_srednja = mean(placa))
placa_visoka = izobrazba_spol_po_dejavnostih %>% filter(izobrazba == "visoka") %>% group_by(leto) %>% summarise(placa_visoka = mean(placa))

placa_izobrazba = placa_slo %>%
  left_join(placa_osnovna) %>%
  left_join(placa_srednja) %>%
  left_join(placa_visoka)

#pobrišimo nepotrebne tabele
rm(placa_osnovna, placa_srednja, placa_visoka)

#PLAČE PO DEJAVNOSTIH

placa_kmetijstvo = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "kmetijstvo in lov, gozdarstvo, ribištvo") %>% group_by(leto) %>% summarise(placa_kmetijstvo = mean(placa))
placa_rudarstvo = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "rudarstvo") %>% group_by(leto) %>% summarise(placa_rudarstvo = mean(placa))
placa_predelovalne = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "predelovalne dejavnosti") %>% group_by(leto) %>% summarise(placa_predelovalne = mean(placa))
placa_oskrba_el = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "oskrba z električno energijo, plinom in paro") %>% group_by(leto) %>% summarise(placa_oskrba_el = mean(placa))
placa_oskrba_voda = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "oskrba z vodo, ravnanje z odplakami in odpadki, saniranje okolja") %>% group_by(leto) %>% summarise(placa_oskrba_voda = mean(placa))
placa_gradbenistvo = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "gradbeništvo") %>% group_by(leto) %>% summarise(placa_gradbenistvo = mean(placa))
placa_trgovina = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "trgovina, vzdrževanje in popravila motornih vozil") %>% group_by(leto) %>% summarise(placa_trgovina = mean(placa))
placa_promet = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "promet in skladiščenje") %>% group_by(leto) %>% summarise(placa_promet = mean(placa))
placa_gostinstvo = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "gostinstvo") %>% group_by(leto) %>% summarise(placa_gostinstvo = mean(placa))
placa_ikt = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "informacijske in komunikacijske dejavnosti") %>% group_by(leto) %>% summarise(placa_ikt = mean(placa))
placa_finance = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "finančne in zavarovalniške dejavnosti") %>% group_by(leto) %>% summarise(placa_finance = mean(placa))
placa_nepremicnine = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "poslovanje z nepremičninami") %>% group_by(leto) %>% summarise(placa_nepremicnine = mean(placa))
placa_znanost = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "strokovne, znanstvene in tehnične dejavnosti") %>% group_by(leto) %>% summarise(placa_znanost = mean(placa))
placa_druge_poslovne = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "druge raznovrstne poslovne dejavnosti") %>% group_by(leto) %>% summarise(placa_druge_poslovne = mean(placa))
placa_javna_uprava = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "dejavnost javne uprave in obrambe, dejavnost obvezne socialne varnosti") %>% group_by(leto) %>% summarise(placa_javna_uprava = mean(placa))
placa_izobrazevanje = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "izobraževanje") %>% group_by(leto) %>% summarise(placa_izobrazevanje = mean(placa))
placa_zdravstvo = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "zdravstvo in socialno varstvo") %>% group_by(leto) %>% summarise(placa_zdravstvo = mean(placa))
placa_kultura = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "kulturne, razvedrilne in rekreacijske dejavnosti") %>% group_by(leto) %>% summarise(placa_kultura = mean(placa))
placa_drugo = izobrazba_spol_po_dejavnostih %>% filter(dejavnost == "druge dejavnosti") %>% group_by(leto) %>% summarise(placa_drugo = mean(placa))

placa_dejavnosti = placa_slo %>%
  left_join(placa_kmetijstvo) %>%
  left_join(placa_rudarstvo) %>%
  left_join(placa_predelovalne) %>%
  left_join(placa_oskrba_el) %>%
  left_join(placa_oskrba_voda) %>%
  left_join(placa_gradbenistvo) %>%
  left_join(placa_trgovina) %>%
  left_join(placa_promet) %>%
  left_join(placa_gostinstvo) %>%
  left_join(placa_ikt) %>%
  left_join(placa_finance) %>%
  left_join(placa_nepremicnine) %>%
  left_join(placa_znanost) %>%
  left_join(placa_druge_poslovne) %>%
  left_join(placa_javna_uprava) %>%
  left_join(placa_izobrazevanje) %>%
  left_join(placa_zdravstvo) %>%
  left_join(placa_kultura) %>%
  left_join(placa_drugo)

#pobrišimo nepotrebne tabele
rm(placa_kmetijstvo, placa_rudarstvo, placa_predelovalne, placa_oskrba_el, placa_oskrba_voda, placa_gradbenistvo, placa_trgovina,
   placa_promet, placa_gostinstvo, placa_ikt, placa_finance, placa_nepremicnine, placa_znanost, placa_druge_poslovne, placa_javna_uprava,
   placa_izobrazevanje, placa_zdravstvo, placa_kultura, placa_drugo)

izobrazba_spol_po_dejavnostih %>% write_csv("izobrazba_spol_po_dejavnostih.csv")

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
  dplyr::select(-obcina) %>%
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

spremembe_prihodek_slo = prihodek_slo %>% mutate(abs_sprememba = povrprecen_prihodek - lag(povrprecen_prihodek)) %>% mutate(rel_sprememba = (povrprecen_prihodek - lag(povrprecen_prihodek))/lag(povrprecen_prihodek) )

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

#Izpustimo nepomembne vrstice
rm(prihodek_pomurska, prihodek_podravska, prihodek_koroska, prihodek_savinjska, prihodek_zasavska, prihodek_posavska, prihodek_JV_slovenija,
   prihodek_osrednjeslovenska, prihodek_gorenjska, prihodek_primorska, prihodek_goriška, prihodek_obala)



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
  dplyr::select(-c(sektor_uradno, spol_uradno, izobrazba_uradno)) %>%
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

placa_javni_zenske = izobrazba_spol_po_sektorjih %>% filter(sektor == "javni") %>% filter(spol == "ž") %>% group_by(leto) %>% summarise(placa_javni_z = mean(placa))
placa_javni_moski = izobrazba_spol_po_sektorjih %>% filter(sektor == "javni") %>% filter(spol == "m") %>% group_by(leto) %>% summarise(placa_javni_m = mean(placa))

placa_javni_spol = placa_javni %>% left_join(placa_javni_moski) %>% left_join(placa_javni_zenske)


placa_zasebni_zenske = izobrazba_spol_po_sektorjih %>% filter(sektor == "zasebni") %>% filter(spol == "ž") %>% group_by(leto) %>% summarise(placa_zasebni_z = mean(placa))
placa_zasebni_moski = izobrazba_spol_po_sektorjih %>% filter(sektor == "zasebni") %>% filter(spol == "m") %>% group_by(leto) %>% summarise(placa_zasebni_m = mean(placa))

placa_zasebni_spol = placa_zasebni %>% left_join(placa_zasebni_moski) %>% left_join(placa_zasebni_zenske)

spremembe_placa_zasebni_sektor = placa_zasebni %>% mutate(abs_sprememba = placa_zasebni - lag(placa_zasebni)) %>% mutate(rel_sprememba = (placa_zasebni - lag(placa_zasebni))/lag(placa_zasebni) )



primerjava_prihodki_place = spremembe_prihodek_slo %>% left_join(spremembe_placa_zasebni_sektor, by = "leto") %>%
  rename(abs_sprememba_prihodka = abs_sprememba.x,
         abs_sprememba_place = abs_sprememba.y,
         rel_sprememba_prihodka = rel_sprememba.x,
         rel_sprememba_place_zasebni_sektor = rel_sprememba.y
  ) %>%
  dplyr::select(leto, rel_sprememba_prihodka, rel_sprememba_place_zasebni_sektor) %>%
  na.omit(primerjava_prihodki_place) %>%
  mutate(rel_sprememba_prihodka = rel_sprememba_prihodka * 100, rel_sprememba_place_zasebni_sektor = rel_sprememba_place_zasebni_sektor * 100)

primerjava_prihodki_place %>% write_csv("primerjava_prihodki_place.csv")

#Izpustimo nepomembne vrstice
rm(placa_slo, placa_javni, placa_zasebni, placa_sektor, placa_javni_zenske, placa_javni_moski, obcine_v_regije, preimenovanje_izobrazba, preimenovanje_sektorjev, preimenovanje_spol, prihodek_podjetij_po_obcinah,
   placa_javni_spol, placa_zasebni_zenske, placa_zasebni_moski, spremembe_placa_zasebni_sektor, prihodek_slo, spremembe_prihodek_slo)

