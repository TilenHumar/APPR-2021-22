# 4. faza: Napredna analiza podatkov

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(tidyverse)
library(cluster)
library(ggalt)
library(dplyr)

#Tabela relativne spremembe povprečne plače po dejavnostih med letoma 2008 in 2019 -> katere dejavnosti imajo podobno gibanje povprečne plače?
tabela = read_csv("izobrazba_spol_po_dejavnostih.csv")
tabela = tabela %>% filter(leto == c(2008, 2019)) %>% group_by(dejavnost, leto) %>% summarise(placa = mean(placa)) %>%
         mutate(relativna_sprememba_place = round((placa - lag(placa)) / lag(placa), 6)) %>% filter(leto == 2019) %>%
         dplyr::select(dejavnost, relativna_sprememba_place)

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
      set.seed(42) # zato, da so rezultati ponovljivi
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


r.km = tabela[, -1] %>% obrisi(hc = FALSE)
optimalno_stevilo_skupin <- obrisi.k(r.km)

#optimalno_stevilo_skupin = 3

set.seed(14)
#diagram.obrisi(r.km)

dejavnosti = tabela[, 1] %>% unlist()
razdalje = tabela[, -1] %>% dist()
dendrogram = razdalje %>% hclust(method = "ward.D")

plot(
  dendrogram,
  labels = dejavnosti,
  ylab = "višina",
  main = NULL
)
