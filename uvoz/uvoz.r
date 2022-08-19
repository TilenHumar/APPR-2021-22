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
                                         ))

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
                                                col_names = c("dejavnost", "število delovno aktivnega prebivalstva", "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))
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

izobrazba_spol_po_dejavnostih %>% write_csv("izobrazba_spol_po_dejavnostih.csv")

### Tretja tabela, Četrta tabela

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

#POVPREČEN PRIHODEK PO LETIH

prihodek_slo = prihodek_podjetij_po_regijah %>% group_by(leto) %>% summarise(povrprecen_prihodek = mean(prihodek))

spremembe_prihodek_slo = prihodek_slo %>% mutate(abs_sprememba = povrprecen_prihodek - lag(povrprecen_prihodek)) %>% mutate(rel_sprememba = (povrprecen_prihodek - lag(povrprecen_prihodek))/lag(povrprecen_prihodek) )

izobrazba_spol_po_sektorjih = read_csv("podatki/povprecne_place_glede_na_izobrazbo_spol_po_sektorjih.csv", na="...",
                                       locale=locale(encoding="Windows-1250"),
                                       skip = 1,
                                       col_names = c("sektor_uradno", "izobrazba_uradno", "spol_uradno","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                       col_types= cols(.default = col_guess(),
                                                       spol_uradno = col_factor(),
                                                       izobrazba_uradno = col_factor())
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
               values_to = "placa") %>%
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

spremembe_placa_zasebni_sektor = placa_zasebni %>% mutate(abs_sprememba = placa_zasebni - lag(placa_zasebni)) %>% mutate(rel_sprememba = (placa_zasebni - lag(placa_zasebni))/lag(placa_zasebni) )

primerjava_prihodki_place = spremembe_prihodek_slo %>% left_join(spremembe_placa_zasebni_sektor, by = "leto") %>%
  rename(abs_sprememba_prihodka = abs_sprememba.x,
         abs_sprememba_place = abs_sprememba.y,
         rel_sprememba_prihodka = rel_sprememba.x,
         rel_sprememba_place_zasebni_sektor = rel_sprememba.y) %>%
  dplyr::select(leto, rel_sprememba_prihodka, rel_sprememba_place_zasebni_sektor) %>%
  na.omit(primerjava_prihodki_place) %>%
  mutate(rel_sprememba_prihodka = rel_sprememba_prihodka * 100, rel_sprememba_place_zasebni_sektor = rel_sprememba_place_zasebni_sektor * 100)

primerjava_prihodki_place %>% write_csv("primerjava_prihodki_place.csv")

