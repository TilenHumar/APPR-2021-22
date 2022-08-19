# 4. faza: Napredna analiza podatkov

#prikaze dendrogramov, napak, ... sem moral zakomentirati, saj so se sicer pojavljali v poročilu


source("lib/libraries.r")

###RAZVRŠČANJE V SKUPINE

#---------------------------------------------------------------------------------------------------------------------------------------------
#Tabela relativne spremembe povprečne plače po dejavnostih med letoma 2008 in 2019 -> katere dejavnosti imajo podobno gibanje povprečne plače?
tabela_dejavnosti = read_csv("izobrazba_spol_po_dejavnostih.csv")
tabela_dejavnosti = tabela_dejavnosti %>% filter(leto == c(2008, 2019)) %>% group_by(dejavnost, leto) %>% summarise(placa = mean(placa)) %>%
  mutate(relativna_sprememba_place = round((placa - lag(placa)) / lag(placa), 6)) %>% filter(leto == 2019) %>%
  dplyr::select(dejavnost, relativna_sprememba_place)

#Tabela povprečnih plač po statističnih regijah v letu 2019 -> katere regije imajo podobno povprečno plač?
#Zato, da na zemljevidu prikažen gručenje.
tabela_regije = read_csv("starost_spol_po_regijah.csv")
tabela_regije = tabela_regije %>% filter(leto == 2019) %>% group_by(regija) %>% summarise(placa = mean(placa))
#---------------------------------------------------------------------------------------------------------------------------------------------

#FUNKCIJE S PREDAVANJ 
hc.kolena <- function(dendrogram, od = 1, do = NULL, eps = 0.05 ) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k, pri katerih opazujemo koleno
hc.kolena.k <- function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    dplyr::select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.kolena <- function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "#006699", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

obrisi = function(podatki, hc = TRUE, od = 2, do = NULL) {
  n = nrow(podatki)
  if (is.null(do)) {
    do = n - 1
  }
  
  razdalje = dist(podatki)
  
  k.obrisi = tibble()
  for (k in od:do) {
    if (hc) {
      o.k = hclust(razdalje) %>%
        cutree(k) %>%
        silhouette(razdalje)
    } else {
      set.seed(14) # zato, da so rezultati ponovljivi
      o.k = kmeans(podatki, k)$cluster %>%
        silhouette(razdalje)
    }
    k.obrisi = k.obrisi %>% bind_rows(
      tibble(
        k = rep(k, n),
        obrisi = o.k[, "sil_width"]
      )
    )
  }
  k.obrisi$k = as.ordered(k.obrisi$k)
  
  k.obrisi
}

obrisi.povprecje = function(k.obrisi) {
  k.obrisi.povprecje = k.obrisi %>%
    group_by(k) %>%
    summarize(obrisi = mean(obrisi))
}

obrisi.k = function(k.obrisi) {
  obrisi.povprecje(k.obrisi) %>%
    filter(obrisi == max(obrisi)) %>%
    summarize(k = min(k)) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

diagram.obrisi = function(k.obrisi) {
  ggplot() +
    geom_boxplot(
      data = k.obrisi,
      mapping = aes(x = k, y = obrisi)
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = k, y = obrisi),
      color = "red"
    ) +
    geom_line(
      data = obrisi.povprecje(k.obrisi),
      mapping = aes(x = as.integer(k), y = obrisi),
      color = "red"
    ) +
    geom_point(
      data = obrisi.povprecje(k.obrisi) %>%
        filter(obrisi == max(obrisi)) %>%
        filter(k == min(k)),
      mapping = aes(x = k, y = obrisi),
      color = "blue"
    ) +
    xlab("število skupin (k)") +
    ylab("obrisi (povprečje obrisov)") +
    ggtitle(paste("Maksimalno povprečje obrisov pri k =", obrisi.k(k.obrisi))) +
    theme_classic()
}
#---------------------------------------------------------------------------------------------------------------------------------------------

#Hierarhično razvrščanje v skupine

#---------------------DEJAVNOSTI---------------------

dejavnosti = tabela_dejavnosti[, 1] %>% unlist()
razdalje_dejavnosti = tabela_dejavnosti[, -1] %>% dist()
dendrogram_dejavnosti = razdalje_dejavnosti %>% hclust(method = "ward.D")

#plot(
#  dendrogram_dejavnosti,
#  labels = dejavnosti,
#  ylab = "višina",
#  xlab = "dejavnosti",
#  main = NULL
#)

# tabela s koleni za dendrogram
kolena_dejavnosti = hc.kolena(dendrogram_dejavnosti)
diagram.kolena(kolena_dejavnosti)
#kolena = 2, 3, 4

#ker sta primera za 2 in 4 skupine precej očitna, poglejmo kako grupira dejavnosti v 3 skupine
#plot(dendrogram_dejavnosti,
#     labels = dejavnosti,
#     ylab = "višina",
#     xlab = "dejavnosti",
#     main = NULL)
#oznacen_dendrogram_dejavnosti = rect.hclust(dendrogram_dejavnosti,k=3,border="red")
#p_dejavnosti = cutree(dendrogram_dejavnosti, k=3)

#---------------------REGIJE---------------------

regije = tabela_regije[, 1] %>% unlist()
razdalje_regije = tabela_regije[, -1] %>% dist()
dendrogram_regije = razdalje_regije %>% hclust(method = "ward.D")

#plot(
#  dendrogram_regije,
#  labels = regije,
#  ylab = "višina",
#  xlab = "regije",
#  main = "Dendrogram razvrščanja regij v skupine, glede na \nvišino povprečne plače v letu 2019"
#)

# tabela s koleni za dendrogram
kolena_regije = hc.kolena(dendrogram_regije)
#diagram.kolena(kolena_regije)
#kolena = 2, 3, 4, 6, 7, 8, 10

#recimo 2 skupini
#plot(dendrogram_regije, hang=-0.1, cex=1,
#     labels = regije,
#     ylab = "višina",
#     xlab = "regije",
#     main = "Dendrogram razvrščanja regij v skupine, glede na \nvišino povprečne plače v letu 2019 \n(z prikazom skupin)")
#rect.hclust(dendrogram_regije,k=2,border="red")
#p_regije = cutree(dendrogram_regije, k=2)

#---------------------------------------------------------------------------------------------------------------------------------------------

#Metoda k-tih voditeljev

#---------------------DEJAVNOSTI---------------------

vrednosti_dejavnosti = tabela_dejavnosti[, -1] %>% obrisi(hc = FALSE)
optimalno_stevilo_skupin_dejavnosti <- obrisi.k(vrednosti_dejavnosti)
diagram.obrisi(vrednosti_dejavnosti)
#optimalno_stevilo_skupin = 3


set.seed(14)
skupine_dejavnosti = tabela_dejavnosti[, -1] %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()
#print(skupine_dejavnosti)
#vidimo, da se razvrstitev ujema tisti na dendrogramu

#---------------------REGIJE---------------------

vrednosti_regije = tabela_regije[, -1] %>% obrisi(hc = FALSE)
optimalno_stevilo_skupin_regije <- obrisi.k(vrednosti_regije)
diagram.obrisi(vrednosti_regije)
#optimalno_stevilo_skupin = 2


set.seed(14)
skupine_regije = tabela_regije[, -1] %>%
  kmeans(centers = 2) %>%
  getElement("cluster") %>%
  as.ordered()
#print(skupine_regije)
#vidimo, da se razvrstitev ujema tisti na dendrogramu

#prikaz na zemljevidu -> narejeno kot Shiny aplikacija

##---------------------------------------------------------------------------------------------------------------------------------------------

###NAPOVEDNI MODEL

#poskusimo napovedati nekaj prihodnjih povprečnih plač za mlade v osrednjeslovenski regiji
#novo plačo bomo napovedali glede na povprečni plači v prejšnjih dveh letih

naredi = function(x){
  data.frame(placa  = x,
             "placa_eno_prej"  = lag(x, 1),
             "placa_dve_prej" = lag(x, 2))
}

tabela_napoved_mladi = read_csv("starost_spol_po_regijah.csv")
tabela_napoved_mladi = tabela_napoved_mladi %>% filter(starost == "15-24 let") %>% group_by(regija,leto) %>% dplyr::summarise(placa = mean(placa)) %>%
  filter(regija == "Osrednjeslovenska")

tabela_napoved_mladi = naredi(tabela_napoved_mladi$placa)
tabela_napoved_mladi$leto = c(2008:2019)

model = lm(placa ~ placa_eno_prej + placa_dve_prej, data=tabela_napoved_mladi %>% drop_na() )
tabela_napoved_mladi[nrow(tabela_napoved_mladi) + 1,] <- c(NA, tabela_napoved_mladi$placa[12], tabela_napoved_mladi$placa[11], 2020)

st_vrstic = nrow(tabela_napoved_mladi)

prva_napoved = predict(model, newdata = tabela_napoved_mladi[st_vrstic, ])
tabela_napoved_mladi$placa[st_vrstic] = prva_napoved

tabela_napoved_mladi[nrow(tabela_napoved_mladi) + 1,] <- c(NA, prva_napoved, tabela_napoved_mladi$placa[12], 2021)

druga_napoved = predict(model, newdata = tabela_napoved_mladi[st_vrstic + 1, ])
tabela_napoved_mladi$placa[st_vrstic + 1] = druga_napoved

stevila = 2008:2021

graf_napovedi = tabela_napoved_mladi %>% ggplot(
  mapping = aes(fill = leto > 2019, x = leto, y = placa)
) +
  geom_bar(stat="identity",
           width = 0.5,
           position = "dodge"
  ) +
  scale_y_continuous() +
  scale_x_continuous("leto", labels = as.character(stevila), breaks = stevila) +
  theme_classic() +
  labs(
    x = "leto",
    y = "višina plače v evrih",
    title = "Višina povprečne plače mladih Osrednjeslovenske \nregije v evrih med letoma 2008 in 2021 "
  ) +
  theme(axis.text.x = element_text(size = 14, angle = 90), axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 10, face = "bold")) +
  theme(legend.position="none") +
  labs(caption = "Opomba: rdeče obarvani stolpi predstavljajo izmerjene podatke, modra stolpca pa \nsta projekciji višine plače v pripadajočih letih.") +
  theme(plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))

graf_napovedi

#Zanima nas kakšno plačo pričakujemo, če poznamo svojo regijo, spol, starostno skupino in število študentov na 1000 prebivalcev v naši regiji
tabela_napoved = read_csv("starost_spol_po_regijah.csv")

tabela_napoved$regija = gsub('Jugovzhodna Slovenija', 'JugovzhodnaSlovenija', tabela_napoved$regija)
tabela_napoved$regija = gsub('Primorsko-notranjska', 'Primorskonotranjska', tabela_napoved$regija)
tabela_napoved$regija = gsub('Obalno-kraška', 'Obalnokraška', tabela_napoved$regija)
tabela_napoved$starost = gsub('15-24 let', '15do24', tabela_napoved$starost)
tabela_napoved$starost = gsub('25-34 let', '25do34', tabela_napoved$starost)
tabela_napoved$starost = gsub('35-44 let', '35do44', tabela_napoved$starost)
tabela_napoved$starost = gsub('45-54 let', '45do54', tabela_napoved$starost)
tabela_napoved$starost = gsub('55-64 let', '55do64', tabela_napoved$starost)
tabela_napoved$starost = gsub('65 let ali več', '65plus', tabela_napoved$starost)

#funkcija one_hot diskretno spremenljivko nadomesti z numerično, vzeta s predavanj

one_hot = function(podatki, spr) {
  domena = podatki %>%
    dplyr::select({{spr}}) %>%
    unlist() %>%
    unique()
  for (v in domena) {
    podatki = podatki %>%
      mutate(
        "{{spr}}.{v}" := ifelse({{spr}} == v, 1, 0)
      )
  }
  podatki %>% dplyr::select(-{{spr}})
}

tabela_napoved = tabela_napoved %>% one_hot(spol) %>% one_hot(regija) %>% one_hot(starost) %>% rename(studenti =`število študentov na 1000 prebivalcev`)

set.seed(321)
#linearna regresija za napoved plače glede na regijo
linearna_reg_regije = lm(placa ~ regija.Pomurska + regija.Podravska + regija.Koroška + regija.Savinjska + regija.Zasavska + 
                           regija.Posavska + regija.JugovzhodnaSlovenija + regija.Osrednjeslovenska + regija.Gorenjska +
                           regija.Primorskonotranjska + regija.Goriška + regija.Obalnokraška, data = tabela_napoved)

#print(linearna_reg_regije)

#linearna regresija za napoved plače glede na spol
linearna_reg_spol = lm(placa ~ spol.m + spol.ž, data = tabela_napoved)
#print(linearna_reg_spol)

#linearna regresija za napoved plače glede na starost
linearna_reg_starost = lm(placa ~ starost.15do24 + starost.25do34 + starost.35do44 + starost.45do54 + starost.55do64 + starost.65plus, data = tabela_napoved)
#print(linearna_reg_starost)

#linearna regresija za napoved plače glede na število študentov na 1000 prebivalcev v regiji
linearna_reg_st_studentov = lm(placa ~ studenti, data = tabela_napoved)
#print(linearna_reg_st_studentov)

#linearna regresija za napoved plače glede na regijo, spol, starostno skupino in število študentov na 1000 prebivalcev v regiji
linearna_reg_vse = lm(placa ~ regija.Pomurska + regija.Podravska + regija.Koroška + regija.Savinjska + regija.Zasavska + 
                        regija.Posavska + regija.JugovzhodnaSlovenija + regija.Osrednjeslovenska + regija.Gorenjska +
                        regija.Primorskonotranjska + regija.Goriška + regija.Obalnokraška + spol.m + spol.ž + 
                        starost.15do24 + starost.25do34 + starost.35do44 + starost.45do54 + starost.55do64 + starost.65plus + studenti,
                      data = tabela_napoved)
#print(linearna_reg_vse)

#formule za napovedi

formula_regije =
  placa ~ regija.Pomurska + regija.Podravska + regija.Koroška + regija.Savinjska + regija.Zasavska + 
  regija.Posavska + regija.JugovzhodnaSlovenija + regija.Osrednjeslovenska + regija.Gorenjska +
  regija.Primorskonotranjska + regija.Goriška + regija.Obalnokraška

formula_spol =
  placa ~ spol.m + spol.ž

formula_starost = 
  placa ~ starost.15do24 + starost.25do34 + starost.35do44 + starost.45do54 + starost.55do64 + starost.65plus

formula_studenti =
  placa ~ studenti

formula_vse = 
  placa ~ regija.Pomurska + regija.Podravska + regija.Koroška + regija.Savinjska + regija.Zasavska + 
  regija.Posavska + regija.JugovzhodnaSlovenija + regija.Osrednjeslovenska + regija.Gorenjska +
  regija.Primorskonotranjska + regija.Goriška + regija.Obalnokraška + spol.m + spol.ž + 
  starost.15do24 + starost.25do34 + starost.35do44 + starost.45do54 + starost.55do64 + starost.65plus + studenti

napaka_lm = function(k, formula, podatki){
  set.seed(321)
  n = nrow(podatki)
  # v je vektor premešanih števim od 1 do n:
  v = sample(1:n)
  #razrez vzame vektor 1:n in ga razreže v k delov (1:n elementom da cifre od 1 do k)
  razrez = cut(1:n, k, labels=FALSE)
  # vektor v razbije glede na razrez
  razbitje = split(v, razrez)
  #naredi n dolg vektor elementov 0
  pp.napovedi = rep(0, n)
  for (i in (1:k)){
    #ucni podatki:
    ucenje = podatki[-razbitje[[i]],]
    #testni podatki:
    test = podatki[razbitje[[i]],]
    model = lm(data=ucenje, formula=formula)
    #napovemo za testne podatke
    napovedi = predict(model, newdata=test)
    pp.napovedi[razbitje[[i]]] = napovedi
  }
  napaka = mean((pp.napovedi - podatki$placa)^2)
  return (napaka)
}

#poglejmo napake
napaka_regije = napaka_lm(10, formula_regije, tabela_napoved)
napaka_spol = napaka_lm(10, formula_spol, tabela_napoved)
napaka_starost = napaka_lm(10, formula_starost, tabela_napoved)
napaka_studenti = napaka_lm(10, formula_studenti, tabela_napoved)
napaka_vse = napaka_lm(10, formula_vse, tabela_napoved)

#print(c(napaka_regije, napaka_spol, napaka_starost, napaka_studenti, napaka_vse))

#prikaz linearnega modela s formula_studenti
graf_podatki = read_csv("starost_spol_po_regijah.csv")
graf_podatki = graf_podatki %>% group_by(regija) %>% summarise(placa = mean(placa), `število študentov na 1000 prebivalcev` = mean(`število študentov na 1000 prebivalcev`))
graf = ggplot(graf_podatki, aes(x = `število študentov na 1000 prebivalcev`, y = placa)) + geom_point() + geom_smooth(method = "lm", formula = y ~ x)
graf 

#naključni gozd
napaka_ng = function(k, formula, podatki){
  set.seed(321)
  n = nrow(podatki)
  v = sample(1:n)
  razrez = cut(1:n, k, labels=FALSE)
  razbitje = split(v, razrez)
  pp.napovedi = rep(0, n)
  for (i in (1:k)){
    ucenje = podatki[-razbitje[[i]],]
    test = podatki[razbitje[[i]],]
    model = ranger(formula, data=ucenje)
    napovedi = predict(model, test)$predictions
    pp.napovedi[razbitje[[i]]] = napovedi
  }
  napaka = mean((pp.napovedi - podatki$placa)^2)
  return (napaka)
}

#poglejmo napake
napaka_regije_ng = napaka_ng(10, formula_regije, tabela_napoved)
napaka_spol_ng = napaka_ng(10, formula_spol, tabela_napoved)
napaka_starost_ng = napaka_ng(10, formula_starost, tabela_napoved)
napaka_studenti_ng = napaka_ng(10, formula_studenti, tabela_napoved)
napaka_vse_ng = napaka_ng(10, formula_vse, tabela_napoved)

#print(c(napaka_regije_ng, napaka_spol_ng, napaka_starost_ng, napaka_studenti_ng, napaka_vse_ng))

#vidimo, da najmanjšo napako dobimo s formulo_vse in algoritmom naključni gozdovi


###MOČ POSAMEZNE SPREMENLJIVKE
model <- lm(formula = formula_vse, data = tabela_napoved)
X = tabela_napoved %>% dplyr:: select(!placa)

pfun = function(model, newdata) {
  predict(model,newdata = newdata)
}

lm.pred = Predictor$new(model= model, data = X, y = tabela_napoved$placa, predict.function = pfun)

lm.moci = FeatureImp$new(lm.pred, loss = "mse")

moc =  plot(lm.moci)
moc
#vidimo, da na napoved najbolj vplivajo spremeljivke, ki določajo starost
