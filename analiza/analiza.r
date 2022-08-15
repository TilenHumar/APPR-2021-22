# 4. faza: Napredna analiza podatkov
#to je test - lokalno
#in main - git
#ja vem :(

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(cluster)
library(ggalt)
library(dplyr)


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

plot(
  dendrogram_dejavnosti,
  labels = dejavnosti,
  ylab = "višina",
  xlab = "dejavnosti",
  main = "Dendrogram razvrščanja dejavnosti v skupine, glede na \ngibanje povprečne plače med letoma 2008 in 2019"
)

# tabela s koleni za dendrogram
kolena_dejavnosti = hc.kolena(dendrogram_dejavnosti)
diagram.kolena(kolena_dejavnosti)
#kolena = 2, 3, 4

#ker sta primera za 2 in 4 skupine precej očitna, poglejmo kako grupira dejavnosti v 3 skupine
plot(dendrogram_dejavnosti, hang=-0.1, cex=1,
     labels = dejavnosti,
     ylab = "višina",
     xlab = "dejavnosti",
     main = "Dendrogram razvrščanja dejavnosti v skupine, glede na \ngibanje povprečne plače med letoma 2008 in 2019 \n(z prikazom skupin)")
rect.hclust(dendrogram_dejavnosti,k=3,border="red")
p_dejavnosti = cutree(dendrogram_dejavnosti, k=3)

#---------------------REGIJE---------------------

regije = tabela_regije[, 1] %>% unlist()
razdalje_regije = tabela_regije[, -1] %>% dist()
dendrogram_regije = razdalje_regije %>% hclust(method = "ward.D")

plot(
  dendrogram_regije,
  labels = regije,
  ylab = "višina",
  xlab = "regije",
  main = "Dendrogram razvrščanja regij v skupine, glede na \nvišino povprečne plače v letu 2019"
)

# tabela s koleni za dendrogram
kolena_regije = hc.kolena(dendrogram_regije)
diagram.kolena(kolena_regije)
#kolena = 2, 3, 4, 6, 7, 8, 10

#recimo 2 skupini
plot(dendrogram_regije, hang=-0.1, cex=1,
     labels = regije,
     ylab = "višina",
     xlab = "regije",
     main = "Dendrogram razvrščanja regij v skupine, glede na \nvišino povprečne plače v letu 2019 \n(z prikazom skupin)")
rect.hclust(dendrogram_regije,k=2,border="red")
p_regije = cutree(dendrogram_regije, k=2)

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
print(skupine_dejavnosti)
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
print(skupine_regije)
#vidimo, da se razvrstitev ujema tisti na dendrogramu

#prikaz na zemljevidu
#---------------------------------------------------------------------------------------------------------------------------------------------
#osnova za zemljevid
source("lib/uvozi.zemljevid.r")

slovenija_regije <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                                    "SVN_adm1", encoding="UTF-8") %>% fortify()
colnames(slovenija_regije)[12]<-'regija'
slovenija_regije$regija = gsub('Notranjsko-kraška', 'Primorsko-notranjska', slovenija_regije$regija)
slovenija_regije$regija = gsub('Spodnjeposavska', 'Posavska', slovenija_regije$regija)
#---------------------------------------------------------------------------------------------------------------------------------------------

skupine_regije = tabela_regije[, -1] %>%
  kmeans(centers = 2)

skupine_zemljevid = data.frame(regija = tabela_regije$regija, skupina = factor(skupine_regije$cluster))

zemljevid_gručenje = ggplot() +
  geom_polygon(data = right_join(skupine_zemljevid, slovenija_regije, by = "regija"),
               aes(x = long, y = lat, group = group, fill = skupina))+
  ggtitle("Zemljevid gručenja regij glede na povprečno plačo v letu 2019") + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold")) +
  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) +
  scale_fill_discrete() +
  labs(fill="Skupina") +
  geom_path(data = right_join(skupine_zemljevid, slovenija_regije,
                              by = "regija"), aes(x = long, y = lat, 
                                                  group = group), 
            color = "black", size = 0.1)

zemljevid_gručenje

