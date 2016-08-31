paquetes <- lapply(c('dplyr','tidyr','ggplot2','foreign','maptools'), require, character.only=T)

entidades <- read.csv("/Users/david/Documents/cad/infografias/familias/doc/entidades.csv",
                      stringsAsFactors = FALSE)
Encoding(entidades$Entidad) <- "UTF-8"

### HOGARES
hogares <- read.dbf("/Users/david/Documents/cad/infografias/familias/doc/base_datos_enadid14/THogar.dbf")
#hogares09 <- read.dbf("/Users/david/Documents/cad/infografias/familias/doc/2009/base_datos_enadid09/tr_viv_hog.dbf")


### PRIMER CUADRO
# Definición de hogar: http://cuentame.inegi.org.mx/poblacion/hogares.aspx?tema=P
round(sum(hogares$FAC_VIV)/1000000,1)  ### TOTAL

tab.cls.hog <- hogares %>%
  group_by(CLS_HOG) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = round((casos/sum(casos))*100,0)) %>% 
  filter(CLS_HOG %in% c(1,2,3,5,6))

gg.1 <- ggplot(tab.cls.hog, aes(x = CLS_HOG, y = freq, label = freq)) + 
  theme_bw() +
  geom_bar(stat = "identity") + 
  geom_text(color = 'red',vjust=-0.7) + 
  xlab("") + 
  ylab("")

ggsave(plot = gg.1, filename = '/Users/david/Documents/cad/infografias/familias/graphs/cuadro1.pdf', 
       width = 5.5, height = 6)

### SEGUNDO CUADRO
round(round(sum(hogares$FAC_VIV)/1000000,1)*(tab.cls.hog[1,3]/100),0)
tab.scjefe.nuc.hog <- hogares %>%
  filter(CLS_HOG == 1) %>% 
  group_by(SC_JEFE) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = round((casos/sum(casos))*100,0)) %>% 
  filter(SC_JEFE %in% c(1,2,3,4))

levels(tab.scjefe.nuc.hog$SC_JEFE) <- c('Soltera(o)','Casada(o)','En unión libre o separada(o) de unión libre',
                                        'Separada(o) de un matrimonio, divorciada(o) o viuda(o)','No especificado')

tab.scjefe.nuc.hog$SC_JEFE <- factor(tab.scjefe.nuc.hog$SC_JEFE)
gg.2 <- ggplot(tab.scjefe.nuc.hog, aes(x = SC_JEFE, y = freq, label = freq)) + 
  theme_bw() +
  geom_bar(stat = "identity") + 
  geom_text(color = 'red',hjust=-0.7) + 
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(tab.scjefe.nuc.hog$SC_JEFE))) + 
  xlab("") + 
  ylab("")
  
ggsave(plot = gg.2, filename = '/Users/david/Documents/cad/infografias/familias/graphs/cuadro2.pdf', 
       width = 11, height = 6)

### TERCER CUADRO
round(round(round(sum(hogares$FAC_VIV)/1000000,1)*(tab.cls.hog[1,3]/100),0)*(tab.scjefe.nuc.hog[2,3])/100,1)
tab.happyfam <- hogares %>%
  filter(CLS_HOG == 1, 
         SC_JEFE == 2) %>% 
  group_by(TOT_PER) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos))

ggplot(tab.happyfam, aes(x = TOT_PER, y = freq)) + 
  geom_bar(stat = "identity")

### CUARTO CUADRO
hogares$TOT_PER <- as.numeric(hogares$TOT_PER)
tab.happyfam.edad <- hogares %>%
  filter(CLS_HOG == 1, 
         SC_JEFE == 2,
         TOT_PER >= 3) %>% 
  group_by(GQE_JEFE) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos))

tab.todos.edad <- hogares %>%
  group_by(GQE_JEFE) %>% 
  summarise(casos.tot = sum(FAC_VIV)) %>% 
  mutate(freq.tot = casos.tot/sum(casos.tot))

tab.jf.edad <- tab.todos.edad %>% 
  inner_join(tab.happyfam.edad) %>% 
  mutate(prop = casos/casos.tot) %>% 
  data.frame()

ggplot(tab.jf.edad, aes(x = GQE_JEFE, y = prop)) + 
  geom_bar(stat = "identity")

### QUINTO CUADRO
hogares$GQE_JEFE <- as.numeric(hogares$GQE_JEFE)
tab.happyfam.ent <- hogares %>%
  filter(CLS_HOG == 1, 
         SC_JEFE == 2,
         TOT_PER >= 3,
         GQE_JEFE <= 5
         ) %>% 
  group_by(ENT) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos)) %>% 
  ungroup() %>% 
  arrange(desc(casos)) %>% 
  data.frame()

ggplot(tab.happyfam.ent, aes(x = ENT, y = freq)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

tab.hog.ent <- hogares %>% 
  group_by(ENT) %>% 
  summarise(casos.tot = sum(FAC_VIV))

tab.hog.target <- tab.happyfam.ent %>% 
  inner_join(tab.hog.ent) %>% 
  mutate(prop = round((casos/casos.tot)*100,0))

tab.hog.target$id <- as.numeric(tab.hog.target$ENT)
# Info Mapas
edo<-readShapeSpatial("/Users/david/Documents/cad/infografias/familias/data/mex_edos/Mex_Edos")
edo@data$id <- rownames(edo@data)
edo_df<- edo %>%
  fortify() %>%
  mutate(id=as.numeric(id)+1)

edo_subdiag <- left_join(edo_df, tab.hog.target, by = 'id') 


# Mapa proporcion de hogares objetivo
gg <- ggplot(data = edo_subdiag, aes(long, lat, group=group)) + 
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

ggsave(plot = gg, filename = 'graphs/hogares_tradicionales.pdf', 
       width = 5.5, height = 6)

############################################################################
  
# H1: Hogar Nuclear
# H2: Hogar Ampliado
# H3: Hogar Compuesto
# H4: Hogar familiar no especificado
# H5: Hogar Unipersonal
# H6: Hogar de corresidentes
# H9: Hogar no especificado

tab.cls.hog <- hogares %>%
  group_by(ENT, CLS_HOG) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos))

ggplot(tab.cls.hog, aes(x = CLS_HOG)) + 
  geom_bar(aes(weight = casos)) + 
  facet_wrap(~ ENT)

qplot(hogares$SC_JEFE) ### SITUACION CONYUGAL DEL JEFE DE FAMILIA

# 1	Soltera(o)
# 2	Casada(o)
# 3	En unión libre o separada(o) de unión libre
# 4	Separada(o) de un matrimonio, divorciada(o) o viuda(o)
# 9	No especificado

tab.scjefe.hog <- hogares %>%
  group_by(ENT,SC_JEFE) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = round((casos/sum(casos))*100,2)) %>% 
  filter(SC_JEFE==2) %>% 
  mutate(ENTFED = as.numeric(ENT)) %>% 
  left_join(entidades) %>% 
  ungroup() %>% 
  arrange(freq)

ggplot(tab.scjefe.hog, aes(x = ENT_ABV, y = freq)) + 
  geom_bar(stat = "identity") + 
  coord_flip()


### VIVIENDAS
viviendas <- read.dbf("TVivienda.dbf")
viviendas %>% head()

qplot(viviendas$HOGAR_AG) ### HOGARES EN LA VIVIENDA

tab.num.hog <- viviendas %>%
  group_by(ENT,HOGAR_AG) %>% 
  summarise(casos = sum(FAC_VIV))

ggplot(tab.num.hog, aes(x = HOGAR_AG)) + 
  geom_bar(aes(weight = casos)) + 
  facet_wrap(~ ENT)


qplot(hogares$INTHOG_R)

qplot(hogares$TIP_HOG)
qplot(hogares$SEXO_JEFE)
qplot(hogares$EDAD_JEFE)
qplot(hogares$SC_JEFE)

table(hogares$INTHOG_R)/sum(table(hogares$INTHOG_R))


### http://www.conapred.org.mx/index.php?contenido=pagina&id=424&id_opcion=436&op=436

setwd("/Users/david/Documents/cad/infografias/familias/")
enadis <- read.spss("ENADIS-2010 (07-06-2012).sav")
