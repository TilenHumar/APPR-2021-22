# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza plač v Sloveniji

V tem projektu bom analiziral povprečne bruto mesečne plače v Sloveniji med letoma 2008 in 2019 (končal bom z letom 2019, saj so podatki za prihodnja leta pomankljivi). Plače bom proučeval z vidika spola, starosti, izobrazbe  ter gospodarske dejavnosti. Ogledal si bom še plače glede na sektor (javni ali zasebni), ter plače v zasebnem sektorju in njihovo gibanje primerjal z gibanjem prihodkov podjetij.

Kot geografsko komponento bom uporabil statistične regije Slovenije in njihove plače primerjal med seboj ter s slovenskim povprečjem. 

## Tabele

  * 1. tabela: Povprečne mesečne bruto plače glede na starost in spol po statističnih regijah.
    + Stolpci: 
      + leto (integer)
      + statisticna_regija (factor)
      + spol (factor)
      + starost (factor)
      + povprecna_mesecna_bruto_placa (double)
      
    + [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0711321S.px) 
   
  * 2. tabela: Povprečne mesečne bruto plače glede na izobrazbo in spol po gospodarskih dejavnostih.
    + Stolpci:
      + leto (integer)
      + gospodarska_dejavnost (factor)
      + izobrazba (factor)
      + spol (factor)
      + povprecna_mesecna_bruto_placa (double)
      
    + [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0711310S.px)
    
  * 3. tabela: Prihodek podjetij po statističnih regijah.
    + Stolpci:
      + leto (integer)
      + statisticna_regija (factor)
      + prihodek_podjetij (double)
      
    + [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/1418806S.px)
    
  * 4. tabela: Povprečne mesečne bruto plače glede na izobrazbo in spol po sektorjih.
    + Stolpci:
      + leto (integer)
      + sektor (factor)
      + izobrazba (factor)
      + spol (factor)
      + povprecna_mesecna_bruto_placa (double)
      
    + [Vir](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/0711340S.px)

## Načrt dela

Najprej bom uvozil podatke in naložil tabele, potem pa iz tabel izračunal nove podatke, s katerimi bom nadaljeval analizo, npr.:
  
  * absolutna_sprememba_bruto_plače (double)
  
  * relativna_sprememba_bruto_plače (double)
  
  To bom izračunal za vsako starostno skupino, spol, gospodarsko dejavnost, izobrazbeno skupino, ... 
  
  * absolutna_sprememba_prihodka_podjetij (double)
  
  * relativna_sprememba_prihodka_podjetij (double)
  
  * ...


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
