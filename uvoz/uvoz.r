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

starost_spol_po_regijah = starost_spol_po_regijah %>% pivot_longer(cols = colnames(starost_spol_po_regijah)[c(4:15)],
                                                                   names_to = "leto",
                                                                   values_to = "placa"
                                                                   ) %>% left_join(preimenovanje_spol, by = "spol_uradno") %>% relocate(spol_uradno, leto, regija, spol, starost, placa) %>% select(-spol_uradno) %>% filter(regija != "SLOVENIJA")
#preverimo, da so tipi stolpcev ustrezni
sapply(starost_spol_po_regijah, class)

#pretvorimo stolpec leto v numeričnega in stolpec spol v faktor
starost_spol_po_regijah$leto = as.numeric(as.character(starost_spol_po_regijah$leto))
starost_spol_po_regijah$spol = as.factor(starost_spol_po_regijah$spol)

#Ker vrstic z Na ne moremo naprej proučevati jih izpustimo
starost_spol_po_regijah = na.omit(starost_spol_po_regijah)

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


izobrazba_spol_po_dejavnostih = read_csv("podatki/povprecne_place_glede_na_dejavnost_izobrazbo_spol.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                         col_names = c("dejavnost_uradno", "izobrazba_uradno", "spol_uradno","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                          col_types= cols(.default = col_guess(),
                                                          spol_uradno = col_factor(),
                                                          dejavnost_uradno = col_factor(),
                                                          izobrazba_uradno = col_factor()
                                                          ))


izobrazba_spol_po_dejavnostih = izobrazba_spol_po_dejavnostih %>% pivot_longer(cols = colnames(izobrazba_spol_po_dejavnostih)[c(4:15)],
                                                                               names_to = "leto",
                                                                               values_to = "placa") %>% left_join(preimenovanje_dejavnosti, by = "dejavnost_uradno") %>% left_join(preimenovanje_spol, by = "spol_uradno") %>% 
                                                                                left_join(preimenovanje_izobrazba, by = "izobrazba_uradno") %>%
                                                                                relocate(dejavnost_uradno, spol_uradno, izobrazba_uradno, leto, dejavnost, izobrazba, spol, placa) %>% select(-c(dejavnost_uradno, spol_uradno, izobrazba_uradno))
#preverimo, da so tipi stolpcev ustrezni
sapply(izobrazba_spol_po_dejavnostih, class)

#pretvorimo stolpec leto v numeričnega, stolpce dejavnost, spol in izobrazba pa v faktor
izobrazba_spol_po_dejavnostih$leto = as.numeric(as.character(izobrazba_spol_po_dejavnostih$leto))
izobrazba_spol_po_dejavnostih$dejavnost = as.factor(izobrazba_spol_po_dejavnostih$dejavnost)
izobrazba_spol_po_dejavnostih$izobrazba = as.factor(izobrazba_spol_po_dejavnostih$izobrazba)
izobrazba_spol_po_dejavnostih$spol = as.factor(izobrazba_spol_po_dejavnostih$spol)


izobrazba_spol_po_dejavnostih %>% write_csv("izobrazba_spol_po_dejavnostih.csv")

### Tretja tabela

### Pretvorna tabela (iz občin v regije)
obcine_v_regije = read_csv("podatki/obcine-regije.csv")
obcine_v_regije %>% arrange(obcina)

prihodek_podjetij_po_obcinah = read_excel("podatki/prihodek_podjetij_po_obcinah.xlsx", skip = 5, na = "-",
                                          col_names = c("obcina","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"))

#Izpustimo nepomembne vrstice
prihodek_podjetij_po_obcinah = head(prihodek_podjetij_po_obcinah, -43)

#Ker vrstic z Na ne moremo naprej proučevati, prihodke nastavimo na 0
prihodek_podjetij_po_obcinah[is.na(prihodek_podjetij_po_obcinah)] = 0

prihodek_podjetij_po_regijah = prihodek_podjetij_po_obcinah %>% right_join(obcine_v_regije, by = "obcina") %>% pivot_longer(cols = colnames(prihodek_podjetij_po_obcinah)[c(2:13)], names_to = "leto", values_to = "prihodek") %>%
  select(-obcina) %>% group_by(regija, leto) %>% summarise(prihodek = sum(prihodek)) %>% relocate(leto, regija, prihodek)

#preverimo, da so tipi stolpcev ustrezni
sapply(prihodek_podjetij_po_regijah, class)

#pretvorimo stolpec leto v numeričnega, stolpec regija pa v faktor
prihodek_podjetij_po_regijah$leto = as.numeric(as.character(prihodek_podjetij_po_regijah$leto))
prihodek_podjetij_po_regijah$regija = as.factor(prihodek_podjetij_po_regijah$regija)

prihodek_podjetij_po_regijah %>% write_csv("prihodek_podjetij_po_regijah.csv")

### Četrta tabela

izobrazba_spol_po_sektorjih = read_csv("podatki/povprecne_place_glede_na_izobrazbo_spol_po_sektorjih.csv", na="...", locale=locale(encoding="Windows-1250"),skip = 1,
                                       col_names = c("sektor_uradno", "izobrazba_uradno", "spol_uradno","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019"),
                                       col_types= cols(.default = col_guess(),
                                                       spol_uradno = col_factor(),
                                                       izobrazba_uradno = col_factor()
                                       ))

#Pomožna tabela za preimenovanje sektorjev
preimenovanje_sektorjev = tibble(sektor_uradno = c( 
  "1 Javni in zasebni sektor - SKUPAJ",
  "11 Javni sektor - SKUPAJ",
  "12 Zasebni sektor - SKUPAJ"),
  sektor = c("skupaj",
             "javni sektor",
             "zasebni sektor")
  )

#Popravek v pomožni tabeli za preimenovanje izobrazbe
#Pomožna tabela za preimenovanje stopnje izobrazbe
preimenovanje_izobrazba = tibble(izobrazba_uradno = c(
  "Izobrazba - Skupaj",
  "Osnovnošolska ali manj",
  "Srednješolska",
  "Višješolska, visokošolska"),
  izobrazba = c(
    "skupaj",
    "osnovna",
    "srednja",
    "visoka")
)

izobrazba_spol_po_sektorjih =  izobrazba_spol_po_sektorjih %>% pivot_longer(cols = colnames(izobrazba_spol_po_sektorjih)[c(4:15)],
                                           names_to = "leto",
                                           values_to = "placa"
                                           )  %>% left_join(preimenovanje_sektorjev, by = "sektor_uradno") %>% left_join(preimenovanje_izobrazba, by = "izobrazba_uradno") %>% left_join(preimenovanje_spol, by = "spol_uradno")  %>%
                                        relocate(sektor_uradno, spol_uradno, leto, sektor, izobrazba, spol, placa) %>% select(-c(sektor_uradno, spol_uradno, izobrazba_uradno))

#preverimo, da so tipi stolpcev ustrezni
sapply(izobrazba_spol_po_sektorjih, class)

#pretvorimo stolpec leto v numeričnega, stolpce sektor, izobrazba in spol pa v faktor
izobrazba_spol_po_sektorjih$leto = as.numeric(as.character(izobrazba_spol_po_sektorjih$leto))
izobrazba_spol_po_sektorjih$sektor = as.factor(izobrazba_spol_po_sektorjih$sektor)
izobrazba_spol_po_sektorjih$izobrazba = as.factor(izobrazba_spol_po_sektorjih$izobrazba)
izobrazba_spol_po_sektorjih$spol = as.factor(izobrazba_spol_po_sektorjih$spol)

izobrazba_spol_po_sektorjih %>% write_csv("izobrazba_spol_po_sektorjih.csv")

