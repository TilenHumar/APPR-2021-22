# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza plač v Sloveniji

V tem projektu bom analiziral povprečne bruto mesečne plače v Sloveniji med letoma 2008 in 2019 (končal bom z letom 2019, saj so podatki za prihodnja leta pomankljivi). Plače bom proučeval z vidika spola, starosti, izobrazbe  ter gospodarske dejavnosti. Ogledal si bom še plače glede na sektor (javni ali zasebni), ter plače v zasebnem sektorju in njihovo gibanje primerjal z gibanjem prihodkov podjetij.

Kot geografsko komponento bom uporabil statistične regije Slovenije in njihove plače primerjal med seboj ter s slovenskim povprečjem. 

## Tabele

__1. tabela__: Povprečne mesečne bruto plače glede na starost in spol po statističnih regijah.
  
Stolpci:

* leto
* regija
* spol
* starost
* plača
* število študentov na 1000 prebivalcev
      
__2. tabela__: Povprečne mesečne bruto plače glede na izobrazbo in spol po gospodarskih dejavnostih.
  
Stolpci:

* leto
* dejavnost
* izobrazba
* spol
* plača
* število delovno aktivnih v dejavnosti
* delovno aktivni kot odstotek celotne populacije
    
__3. tabela__: Primerjava gibanja prihodkov podjetij in plač v zasebnem sektorju
  
Stolpci:

* leto
* relativna sprememba plače v zasebnem sektorju
* relativna sprememba prihodkov podjetij
    
__4. tabela__: Povprečne mesečne bruto plače glede na izobrazbo in spol po sektorjih.
  
Stolpci:

* leto
* sektor
* izobrazba
* spol
* plača 


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
