paquetes <- lapply(c('dplyr','tidyr','ggplot2','foreign','maptools'), require, character.only=T)

hogares <- read.spss("/Users/david/Documents/cad/ensanut2012/salud/hogar.sav")
integrantes <- read.spss("/Users/david/Documents/cad/ensanut2012/salud/integrantes.sav")

ponderador <- hogares %>% 
  data.frame() %>% 
  dplyr::select(folio,pondeh,entidad)
ponderador$entidad <- as.character(ponderador$entidad)
Encoding(ponderador$entidad) <- "latin1"

ponderador$folio <- as.character(ponderador$folio)
ponderador$folio <- gsub(" ","", ponderador$folio, perl=T)

hog.hijos <-  integrantes %>% 
  data.frame() %>% 
  filter(edad<=120) %>% 
  mutate(hijos = ifelse(h207=="Hijo(a)",1,0),
         casado = ifelse(h207=="Jefe(a)" & h219=="est\xe1 casado(a)?",1,0),
         edad.jefe = ifelse(h207=="Jefe(a)",edad,0),
         sc.jefe = ifelse(h207=="Jefe(a)",h219,NA)) %>% 
  mutate(casado.1 = ifelse(is.na(casado),0,casado),
         redad = cut(edad.jefe, breaks = c(18,39,60,120), right = TRUE, include.lowest = TRUE)) %>% 
  group_by(folio) %>% 
  summarise(hijos = max(hijos),
            casado = max(casado.1),
            sc.jefe = unique(sc.jefe)[1],
            edad = max(edad.jefe),
            redad = unique(redad)[1]) %>% 
  filter(!is.na(sc.jefe))

hog.hijos$sc.jefe <- factor(hog.hijos$sc.jefe, 
                            labels = c('unión libre','separado','divorciado','viudo','casado','soltero'))

hog.hijos$folio <- as.character(hog.hijos$folio)

tabla.hijos.casado <- hog.hijos %>% 
  left_join(ponderador) %>% 
  group_by(hijos,casado) %>% 
  summarise(casos = sum(pondeh)) %>% 
  mutate(freq = round((casos/29405552)*100,0))

tabla.sc.edad <- hog.hijos %>% 
  left_join(ponderador) %>% 
  group_by(redad,sc.jefe) %>% 
  summarise(casos = sum(pondeh)) %>% 
  mutate(freq = round((casos/sum(casos))*100,0))

ggplot(tabla.sc.edad,aes(x=redad,y=freq, fill = sc.jefe, label = freq)) + 
  theme_bw() +
  geom_bar(stat = "identity", position = "stack") + 
  geom_text(position = "stack", vjust = 1, size = 6, color = "gray") + 
  xlab(" ") + ylab("%")

    
tabla.sc.edad.ch <- hog.hijos %>% 
  left_join(ponderador) %>% 
  filter(hijos==1) %>% 
  group_by(redad,sc.jefe) %>% 
  summarise(casos = sum(pondeh)) %>% 
  mutate(freq = round((casos/sum(casos))*100,0))


tabla.map.tot <- hog.hijos %>% 
  left_join(ponderador) %>% 
  group_by(entidad) %>% 
  summarise(casos.tot = sum(pondeh),
            id = row_number())


tabla.fam.trad.map <- hog.hijos %>% 
  left_join(ponderador) %>% 
  filter(#redad=="[18,39]",
         sc.jefe=="casado",
         hijos==1) %>% 
  group_by(entidad) %>% 
  summarise(casos = sum(pondeh)) %>%
  left_join(tabla.map.tot) %>% 
  mutate(prop=casos/casos.tot) %>% 
  ungroup() %>% 
  arrange(desc(prop))

tabla.fam.trad.map.jov <- hog.hijos %>% 
  left_join(ponderador) %>% 
  filter(redad=="[18,39]",
    sc.jefe=="casado",
    hijos==1) %>% 
  group_by(entidad) %>% 
  summarise(casos = sum(pondeh)) %>%
  left_join(tabla.map.tot) %>% 
  mutate(prop=casos/casos.tot) %>% 
  ungroup() %>% 
  arrange(desc(prop))


# Info Mapas
edo<-readShapeSpatial("/Users/david/Documents/cad/infografias/Familias/data/mex_edos/Mex_Edos")
edo@data$id <- rownames(edo@data)
edo_df<- edo %>%
  fortify() %>%
  mutate(id=as.numeric(id)+1)

edo_subdiag <- left_join(edo_df, tabla.fam.trad.map, by = 'id') 
edo_subdiag.2 <- left_join(edo_df, tabla.fam.trad.map.jov, by = 'id') 

# Mapa proporcion de hogares objetivo
gg.1 <- ggplot(data = edo_subdiag, aes(long, lat, group=group)) + 
  theme_bw() + 
  geom_polygon( aes(fill = prop, group = group),
                color='black', size = .4) + 
  coord_fixed() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank()
  ) + 
  scale_fill_continuous(low = 'white', high = '#132B43') + 
  guides(fill = guide_legend(title = "Proporción de hogares tradicionales")) +
  theme(legend.position = 'top')

gg.2 <- ggplot(data = edo_subdiag.2, aes(long, lat, group=group)) + 
  theme_bw() + 
  geom_polygon( aes(fill = prop, group = group),
                color='black', size = .4) + 
  coord_fixed() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank()
  ) + 
  scale_fill_continuous(low = 'white', high = '#132B43') + 
  guides(fill = guide_legend(title = "Proporción de hogares tradicionales")) +
  theme(legend.position = 'top')


#ggsave(plot = gg, filename = 'graphs/hogares_tradicionales.pdf', 
#       width = 5.5, height = 6)

