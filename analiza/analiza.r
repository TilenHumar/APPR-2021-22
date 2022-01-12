#3. faza: Analiza podatkov

#funkciji, ki bosta dodali zamanknjene stolpce
Lag = function(x,n){c(rep(NA,n),x)[1:length(x)]}
add.lags = function(x){data.frame(prihodek = x, prihodek_naslednje_leto = Lag(x,1))}


spremembe_prihodkov_Gorenjska = prihodek_podjetij_po_regijah %>% group_by(regija) %>% filter(regija == "Gorenjska") %>% select(prihodek)
spremembe_prihodkov_Gorenjska = add.lags(c(spremembe_prihodkov_Gorenjska$prihodek,NA))





# 4. faza: Napredna analiza podatkov
