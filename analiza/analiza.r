#3. faza: Analiza podatkov

#funkciji, ki bosta dodali zamanknjene stolpce
Lag = function(x,n){c(rep(NA,n),x)[1:length(x)]}
add.lags = function(x){data.frame(prihodek = x, prihodek_naslednje_leto = Lag(x,1))}

#Spremembe prihodkov po regijah
spremembe_prihodkov_Gorenjska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Gorenjska") %>% select(prihodek)
spremembe_prihodkov_Gorenjska = add.lags(c(spremembe_prihodkov_Gorenjska$prihodek,NA))

spremembe_prihodkov_Goriška = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Goriška") %>% select(prihodek)
spremembe_prihodkov_Goriška = add.lags(c(spremembe_prihodkov_Goriška$prihodek,NA))

spremembe_prihodkov_Jugovzhodna_Slovenija = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Jugovzhodna Slovenija") %>% select(prihodek)
spremembe_prihodkov_Jugovzhodna_Slovenija = add.lags(c(spremembe_prihodkov_Jugovzhodna_Slovenija$prihodek,NA))

spremembe_prihodkov_Koroška = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Koroška") %>% select(prihodek)
spremembe_prihodkov_Koroška = add.lags(c(spremembe_prihodkov_Koroška$prihodek,NA))

spremembe_prihodkov_Obalno_kraška = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Obalno-kraška") %>% select(prihodek)
spremembe_prihodkov_Obalno_kraška = add.lags(c(spremembe_prihodkov_Obalno_kraška$prihodek,NA))

spremembe_prihodkov_Osrednjeslovenska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Osrednjeslovenska") %>% select(prihodek)
spremembe_prihodkov_Osrednjeslovenska = add.lags(c(spremembe_prihodkov_Osrednjeslovenska$prihodek,NA))

spremembe_prihodkov_Podravska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Podravska") %>% select(prihodek)
spremembe_prihodkov_Podravska = add.lags(c(spremembe_prihodkov_Podravska$prihodek,NA))

spremembe_prihodkov_Pomurska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Pomurska") %>% select(prihodek)
spremembe_prihodkov_Pomurska = add.lags(c(spremembe_prihodkov_Pomurska$prihodek,NA))

spremembe_prihodkov_Posavska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Posavska") %>% select(prihodek)
spremembe_prihodkov_Posavska = add.lags(c(spremembe_prihodkov_Posavska$prihodek,NA))

spremembe_prihodkov_Primorsko_notranjska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Primorsko-notranjska") %>% select(prihodek)
spremembe_prihodkov_Primorsko_notranjska = add.lags(c(spremembe_prihodkov_Primorsko_notranjska$prihodek,NA))

spremembe_prihodkov_Savinjska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Savinjska") %>% select(prihodek)
spremembe_prihodkov_Savinjska = add.lags(c(spremembe_prihodkov_Savinjska$prihodek,NA))

spremembe_prihodkov_Zasavska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Zasavska") %>% select(prihodek)
spremembe_prihodkov_Zasavska = add.lags(c(spremembe_prihodkov_Zasavska$prihodek,NA))



# tu bom vse vrstice, kjer se pojavi Na izpustil, zato analiza ne bo povsem resnična (nekaterih plačnih skupin ne bom vključil, zato bodo vsote plač manjše)
spremembe_plac = starost_spol_po_regijah %>% group_by(regija, leto)
spremembe_plac = na.omit(spremembe_plac)
spremembe_plac = spremembe_plac %>% summarise(placa = sum(placa))

# Spremembe vsot plač po regijah
spremembe_plac_Gorenjska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Gorenjska") %>% select(placa)
spremembe_plac_Gorenjska = add.lags(c(spremembe_plac_Gorenjska$placa,NA))

spremembe_plac_Goriška = spremembe_plac %>% group_by(regija) %>% filter(regija == "Goriška") %>% select(placa)
spremembe_plac_Goriška = add.lags(c(spremembe_plac_Goriška$placa,NA))

spremembe_plac_Jugovzhodna_Slovenija = spremembe_plac %>% group_by(regija) %>% filter(regija == "Jugovzhodna Slovenija") %>% select(placa)
spremembe_plac_Jugovzhodna_Slovenija = add.lags(c(spremembe_plac_Jugovzhodna_Slovenija$placa,NA))

spremembe_plac_Koroška = spremembe_plac %>% group_by(regija) %>% filter(regija == "Koroška") %>% select(placa)
spremembe_plac_Koroška = add.lags(c(spremembe_plac_Koroška$placa,NA))

spremembe_plac_Obalno_kraška = spremembe_plac %>% group_by(regija) %>% filter(regija == "Obalno-kraška") %>% select(placa)
spremembe_plac_Obalno_kraška = add.lags(c(spremembe_plac_Obalno_kraška$placa,NA))

spremembe_plac_Osrednjeslovenska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Osrednjeslovenska") %>% select(placa)
spremembe_plac_Osrednjeslovenska = add.lags(c(spremembe_plac_Osrednjeslovenska$placa,NA))

spremembe_plac_Podravska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Podravska") %>% select(placa)
spremembe_plac_Podravska = add.lags(c(spremembe_plac_Podravska$placa,NA))

spremembe_plac_Pomurska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Pomurska") %>% select(placa)
spremembe_plac_Pomurska = add.lags(c(spremembe_plac_Pomurska$placa,NA))

spremembe_plac_Posavska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Posavska") %>% select(placa)
spremembe_plac_Posavska = add.lags(c(spremembe_plac_Posavska$placa,NA))

spremembe_plac_Primorsko_notranjska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Primorsko-notranjska") %>% select(placa)
spremembe_plac_Primorsko_notranjska = add.lags(c(spremembe_plac_Primorsko_notranjska$placa,NA))

spremembe_plac_Savinjska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Savinjska") %>% select(placa)
spremembe_plac_Savinjska = add.lags(c(spremembe_plac_Savinjska$placa,NA))

spremembe_plac_Zasavska = spremembe_plac %>% group_by(regija) %>% filter(regija == "Zasavska") %>% select(placa)
spremembe_plac_Zasavska = add.lags(c(spremembe_plac_Zasavska$placa,NA))

# 4. faza: Napredna analiza podatkov
