library(shiny)

shinyServer(function(input, output) {
  
  output$graf <- renderPlot({
    narisi_zemljevid(input$stevilo)
  })
})


narisi_zemljevid = function(stevilo){
  
  #osnova za zemljevid
  source("lib/uvozi.zemljevid.r")
  slovenija_regije <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                                      "SVN_adm1", encoding="UTF-8") %>% fortify()
  colnames(slovenija_regije)[12]<-'regija'
  slovenija_regije$regija = gsub('Notranjsko-kraška', 'Primorsko-notranjska', slovenija_regije$regija)
  slovenija_regije$regija = gsub('Spodnjeposavska', 'Posavska', slovenija_regije$regija)
  
  
  skupine_regije = tabela_regije[, -1] %>%
    kmeans(centers = stevilo)
  
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
  
  print(zemljevid_gručenje)
}