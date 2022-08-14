# 4. faza: Napredna analiza podatkov

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(cluster)
library(ggalt)
library(dplyr)


###RAZVRŠČANJE V SKUPINE


#Tabela relativne spremembe povprečne plače po dejavnostih med letoma 2008 in 2019 -> katere dejavnosti imajo podobno gibanje povprečne plače?
tabela = read_csv("izobrazba_spol_po_dejavnostih.csv")
tabela = tabela %>% filter(leto == c(2008, 2019)) %>% group_by(dejavnost, leto) %>% summarise(placa = mean(placa)) %>%
         mutate(relativna_sprememba_place = round((placa - lag(placa)) / lag(placa), 6)) %>% filter(leto == 2019) %>%
         dplyr::select(dejavnost, relativna_sprememba_place)

#Hierarhično razvrščanje v skupine
dejavnosti = tabela[, 1] %>% unlist()
razdalje = tabela[, -1] %>% dist()
dendrogram = razdalje %>% hclust(method = "ward.D")

plot(
  dendrogram,
  labels = dejavnosti,
  ylab = "višina",
  xlab = "dejavnosti",
  main = "Dendrogram razvrščanja dejavnosti v skupine, glede na \ngibanje povprečne plače med letoma 2008 in 2019"
)

hc.kolena <- function(dendrogram, od = 1, do = NULL, eps = 0.05) {
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

# tabela s koleni za dendrogram
r = hc.kolena(dendrogram)

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

diagram.kolena(r)

#kolena = 2,3,4

#ker sta primera za 2 in 4 skupine precej očitna, poglejmo kako grupira dejavnosti v 3 skupine
plot(dendrogram, hang=-0.1, cex=1,
     labels = dejavnosti,
     ylab = "višina",
     xlab = "dejavnosti",
     main = "Dendrogram razvrščanja dejavnosti v skupine, glede na \ngibanje povprečne plače med letoma 2008 in 2019 \n(z prikazom skupin)")
rect.hclust(dendrogram,k=3,border="red")
p = cutree(dendrogram, k=3)

#Metoda k-tih voditeljev
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

vrednosti = tabela[, -1] %>% obrisi(hc = FALSE)
optimalno_stevilo_skupin <- obrisi.k(vrednosti)
diagram.obrisi(vrednosti)
#optimalno_stevilo_skupin = 3

skupine = tabela[, -1] %>%
  kmeans(centers = 3) %>%
  getElement("cluster") %>%
  as.ordered()
print(skupine)

#vidimo, da se razvrstitev ujema tisti na dendrogramu

