setwd("/Users/david/Documents/cad/infografias/familias/doc/base_datos_enadid14")

paquetes <- lapply(c('dplyr','tidyr','ggplot2','foreign'), require, character.only=T)

entidades <- read.csv("/Users/david/Documents/cad/infografias/familias/doc/entidades.csv",
                      stringsAsFactors = FALSE)
Encoding(entidades$Entidad) <- "UTF-8"

### HOGARES
hogares <- read.dbf("THogar.dbf")
hogares %>% head()

sum(hogares$FAC_VIV)

qplot(hogares$CLS_HOG) ### CLASE DE HOGAR

round(table(hogares$CLS_HOG)/sum(table(hogares$CLS_HOG))*100,2)

### PRIMER CUADRO
tab.cls.hog <- hogares %>%
  group_by(CLS_HOG) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos))

ggplot(tab.cls.hog, aes(x = CLS_HOG, y = freq)) + 
  geom_bar(stat = "identity")

### SEGUNDO CUADRO
tab.scjefe.nuc.hog <- hogares %>%
  filter(CLS_HOG == 1) %>% 
  group_by(SC_JEFE) %>% 
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos))

ggplot(tab.scjefe.nuc.hog, aes(x = SC_JEFE, y = freq)) + 
  geom_bar(stat = "identity")

### TERCER CUADRO
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

ggplot(tab.happyfam.edad, aes(x = GQE_JEFE, y = freq)) + 
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
  summarise(casos = sum(FAC_VIV)) %>% 
  mutate(freq = casos/sum(casos))


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
