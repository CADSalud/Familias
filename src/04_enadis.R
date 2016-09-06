setwd("/Users/david/Documents/cad/infografias/Familias/doc")
enadis <- read.spss("ENADIS-2010 (07-06-2012).sav")
names(enadis)

etiquetas <- attributes(enadis)$variable.labels
write.table(etiquetas,file="etiquetas.txt")

table(enadis$o16_10)/sum(table(enadis$o16_10))

tab.just.mat <- enadis %>% 
  data.frame() %>% 
  group_by(o16_10) %>% 
  filter(!is.na(o16_10)) %>% 
  summarise(casos = sum(pondindi)) %>% 
  mutate(prop = casos/sum(casos))

tab.adop.hom <- enadis %>% 
  data.frame() %>%
  mutate(redad = cut(p8,c(0,39,60,120), right=TRUE, include.lowest=TRUE)) %>% 
  group_by(o6_3,redad) %>% 
  filter(!is.na(o6_3)) %>% 
  summarise(casos = sum(pondindi)) %>% 
  group_by(redad) %>% 
  mutate(prop = round((casos/sum(casos))*100,0)) #%>% 
  #dplyr::select(-casos) %>% 
  #spread(redad,prop)

tab.adop.muj <- enadis %>% 
  data.frame() %>%
  mutate(redad = cut(p8,c(0,39,60,120), right=TRUE, include.lowest=TRUE)) %>% 
  group_by(o7_1,redad) %>% 
  filter(!is.na(o7_1)) %>% 
  summarise(casos = sum(pondindi)) %>% 
  group_by(redad) %>% 
  mutate(prop = round((casos/sum(casos))*100,0)) #%>% 
  #dplyr::select(-casos) %>% 
  #spread(redad,prop)

names(tab.adop.muj)[1] <- "Adopciones"
names(tab.adop.hom)[1] <- "Adopciones"
tab.adop.hom$genero <- "Hombres"
tab.adop.muj$genero <- "Mujeres"

tab.adop <- rbind(tab.adop.muj,tab.adop.hom)

adop <- read.csv("/Users/david/Documents/cad/infografias/Familias/data/adopciones.csv")

#levels(adop$Redad) <- c("[0,39]","(39,60]","(60,120]")
adop$redad <- factor(adop$Redad, c("[0,39]", "(39,60]", "(60,120]"))

gg.adop <- ggplot(adop,aes(x=redad,y=Prop, fill = Adopciones, label = Prop)) + 
  theme_bw() +
  facet_wrap(~Genero) +
  geom_bar(stat = "identity", position = "stack") + 
  geom_text(position = "stack", vjust = 1, size = 6, color = "gray") + 
  xlab(" ") + ylab("%")

ggsave(plot = gg.adop, filename = '/Users/david/Documents/cad/infografias/Familias/graphs/adopcion.svg', 
       device = "svg",width = 10, height = 6)
       
