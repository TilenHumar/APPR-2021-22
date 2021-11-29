# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza plač v Sloveniji

V tem projektu bom analiziral bruto in neto plače v Sloveniji med leti 2008 in 2020 po statističnih regijah. Plače bom proučeval z vidika spola, starosti in izobrazbe prebivalstva ter gospodarske dejavnosti. Ogledal si bom še plače po izobrazbi in spolu glede na sektor (javni ali zasebni), ter plače v zasebnem sektorju in njihovo gibanje primerjal z gibanjem prihodkov podjetij.

Poleg same analize me bo zanimala povezava med višino plače in izobrazbo ter med višino plače in poslovanjem podjetij. 

## Tabele
  *1. tabela: Povprečne plače glede na starost in spol po statističnih regijah.
    * Stolpci: leto, statistična regija, spol, starost, povprečna mesečna bruto plača, povprečna mesečna neto plača
    * [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0711321S.px) 
   
  *2. tabela: Povprečne plače glede na izobrazbo in spol po gospodarskih dejavnostih.
    * Stolpci: leto, gospodarska dejavnost, izobrazba, spol, povprečna mesečna bruto plača, povprečna mesečna neto plača
    * [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0711310S.px)
    
  *3. tabela: Prihodek podjetij po statističnih regijah.
    * Stolpci: leto, statistična regija, prihodek podjetij
    * [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/1418806S.px)
    
  *4. tabela: Povrpečne plače glede na izobrazbo in spol po sektorjih.
    * Stolpci: leto, sektor, izobrazba, spol, povprečna mesečna bruto plača, povprečna mesečna neto plača
    * [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0711340S.px)



## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
