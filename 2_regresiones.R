library(tidyverse)
install.packages("stargazer")

#Modelos trabajo ---------------------
t0_tr<-summary(lm(
  h40.huellaPreWrk ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`+ as.factor(F0), data=bd1hogarDummy))

t1_tr<-summary(lm(
  h40.huellaPstWrk ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd1hogarDummy))


t2_tr<-summary(lm(huellaDicWrk_v2~
             as.factor(F0)+as.factor(GSE)+as.factor(P3_1)+as.factor(`P5[{_1}].Rp`)+as.factor(`P5[{_2}].Rp`)+as.factor(`P5[{_3}].Rp`)+as.factor(`P5[{_4}].Rp`)+as.factor(`P5[{_5}].Rp`)+as.factor(`P5[{_6}].Rp`)+as.factor(`P5[{_7}].Rp`)+as.factor(P5_1)+as.factor(P5_1)+as.factor(P5_2)+as.factor(P5_3)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd2hogarDummy))

#Modelos estudio ---------------------

t0_es<-summary(lm(
  h40.huellaPreStu ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd1hogarDummy))

t1_es<-summary(lm(
  h40.huellaPstStu ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd1hogarDummy))

t2_es<-summary(lm(huellaDicStu_v2~
   as.factor(F0)+as.factor(GSE)+as.factor(P3_1)+as.factor(`P5[{_1}].Rp`)+as.factor(`P5[{_2}].Rp`)+as.factor(`P5[{_3}].Rp`)+as.factor(`P5[{_4}].Rp`)+as.factor(`P5[{_5}].Rp`)+as.factor(`P5[{_6}].Rp`)+as.factor(`P5[{_7}].Rp`)+as.factor(P5_1)+as.factor(P5_1)+as.factor(P5_2)+as.factor(P5_3)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd2hogarDummy))



# Desempeño de los modelos ------------------------------------------------
library(see)
library(performance)

performance::compare_performance(t0_es, t1_es,t2_es) 
performance::compare_performance(t0_tr, t1_tr,t2_tr)


plot(performance::compare_performance(t0_es, t1_es,t2_es))
plot(performance::compare_performance(t0_tr, t1_tr,t2_tr))



# Modelos de trabajo más acotados -----------------------------------------
# Poner tipo de combustible
# Ver una reducción de variables
# Solo trabajo
# Auto Renta Densidad Distancia Compra presencial Cantidad de personas
# Contaminación Compra de alimentos


t0_tr<-lm(
  h40.huellaPreWrk ~  as.factor(P3_1) + 
                      as.factor(`P5[{_3}].Rp`) + 
                      as.factor(`P5[{_6}].Rp`)+ 
                      as.factor(GSE)+
                      as.factor(P5_1)+ 
                      `Densidad del barrio (hab)` + 
                      `Distancia al centro Network`+
                      `P4[{_1}].Rp`, data=bd1hogarDummy[bd1hogarDummy$`P4[{_1}].Rp`!=99,]) %>% summary()

t1_tr<-lm(
  h40.huellaPstWrk ~  as.factor(P3_1) + 
                      as.factor(`P5[{_3}].Rp`) + 
                      as.factor(`P5[{_6}].Rp`)+ 
                      as.factor(GSE)+
                      as.factor(P5_1)+ 
                      `Densidad del barrio (hab)` + 
                      `Distancia al centro Network`+
                      `P4[{_2}].Rp`, data=bd1hogarDummy[bd1hogarDummy$`P4[{_2}].Rp`!=99,]) %>% summary()



t2_tr<-lm(huellaDicWrk_v2~
                    as.factor(GSE)+
                    as.factor(P3_1)+
                    as.factor(`P5[{_3}].Rp`)+
                    as.factor(`P5[{_6}].Rp`)+
                    as.factor(P5_1)+
                    `Densidad del barrio (hab)` + 
                    `Distancia al centro Network`+
                    `P4[{_1}].Rp`, data=bd2hogarDummy[bd2hogarDummy$`P4[{_1}].Rp`!=99,])%>% summary()



# Gráficos ----------------------------------------------------------------
library(tidyverse)

# GSE

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=GSE))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=GSE))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=GSE))+
  geom_point()

# Auto

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=P3_1))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=P3_1))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=P3_1))+
  geom_point()


# + Bicicleta

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=`P5[{_3}].Rp`))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=`P5[{_3}].Rp`))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=`P5[{_3}].Rp`))+
  geom_point()

# Consciencia

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=`P5[{_6}].Rp`))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=`P5[{_6}].Rp`))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=`P5[{_6}].Rp`))+
  geom_point()

# Ha bajado el ingreso

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=P5_1))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=P5_1))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=P5_1))+
  geom_point()

#Densidad

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=`Densidad del barrio (hab)`))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=`Densidad del barrio (hab)`))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=`Densidad del barrio (hab)`))+
  geom_point()

#Distancia

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPreWrk,x=`Distancia al centro Network`))+
  geom_point()

ggplot(data=bd1hogarDummy,aes(y=h40.huellaPstWrk,x=`Distancia al centro Network`))+
  geom_point()

ggplot(data=bd2hogarDummy, aes(y=huellaDicWrk_v2,x=`Distancia al centro Network`))+
  geom_point()


# Sintetizando bases de datos ---------------------------------------------

t0<-select(bd1hogarDummy,
c("h40.huellaPreWrk",
"F0",
"GSE",
"P3_1",
"P5[{_3}].Rp",
"P5[{_6}].Rp",
"P5_1",
"Densidad del barrio (hab)",
"Distancia al centro Network",
"P4[{_1}].Rp"
)) %>% mutate(., data="T0")

t1<-select(bd1hogarDummy,
c("h40.huellaPstWrk",
"F0",
"GSE",
"P3_1",
"P5[{_3}].Rp",
"P5[{_6}].Rp",
"P5_1",
"Densidad del barrio (hab)",
"Distancia al centro Network",
"P4[{_2}].Rp"))%>% mutate(., data="T1")

t2<-select(bd2hogarDummy,
c("huellaDicWrk_v2",
"F0",
"GSE",
"P3_1",
"P5[{_3}].Rp",
"P5[{_6}].Rp",
"P5_1",
"Densidad del barrio (hab)",
"Distancia al centro Network",
"P4[{_1}].Rp"))%>% mutate(., data="T2")

colnames(t0)[1]<-"Huella"
colnames(t1)[1]<-"Huella"
colnames(t2)[1]<-"Huella"

colnames(t0)[10]<-"tripsFood"
colnames(t1)[10]<-"tripsFood"
colnames(t2)[10]<-"tripsFood"


transporte<-rbind(t0,t1,t2)


# Gráficos comparados -----------------------------------------------------

# GSE

g1<-ggplot(data=transporte,aes(y=Huella,x=GSE, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de Carbono & GSE")+
  xlab("GSE")


g2<-ggplot(data=transporte,aes(y=Huella,x=P3_1, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Tenencia de auto")+
  xlab("Tiene auto")

g3<-ggplot(data=transporte,aes(y=Huella,x=F0, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+ theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(title = "Huella de carbono & Ciudad")+
  xlab("Ciudad")

g4<-ggplot(data=transporte,aes(y=Huella,x=`P5[{_3}].Rp`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Camina y anda más en bicicleta")+
  xlab("Camina y anda más en bicicleta")

g5<-ggplot(data=transporte,aes(y=Huella,x=`P5[{_6}].Rp`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Conoce la calidad del aire de su ciudad")+
  xlab("Conoce la calidad del aire de su ciudad")


g6<-ggplot(data=transporte,aes(y=Huella,x=P5_1, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & ha bajado el ingreso promedio del hogar")+
  xlab("Ha bajado el ingreso promedio del hogar")


g7<-ggplot(data=transporte,aes(y=Huella,x=`Densidad del barrio (hab)`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Densidad del barrio")+
  xlab("Densidad del barrio")


g8<-ggplot(data=transporte,aes(y=Huella,x=`Distancia al centro Network`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Distancia (network) al centro")+
  xlab("Distancia (network) al centro")

g9<-ggplot(data=transporte[transporte$tripsFood!=99,],aes(y=Huella,x=tripsFood, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Viajes a comprar alimentos")+
  xlab("Viajes a comprar alimentos")


ggplot(data=transporte, aes(x=Huella, col=factor(data),group=factor(data)))+
  geom_histogram(position = "dodge")

# Nuevo desempeño ---------------------------------------------------------

p1<-performance::compare_performance(t0_tr, t1_tr,t2_tr)

p2<-plot(performance::compare_performance(t0_tr, t1_tr,t2_tr))


# Divide and coquer (pendiente) -------------------------------------------------------

pivot_wider(transporte, id_cols = colnames(transporte)[1])

# Reducción de variables --------------------------------------------------

p1
# Guardando figuras y datos -----------------------------------------------

save(g1,g2,g3,g4,g5,g6,g7,g8,g9,t0_tr,t1_tr,t2_tr,p1,p2, st1,st1b,file="output/graphic.RData")

# Appendix ----------------------------------------------------------------

summary(lm(
  h40.huellaPreStu ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`+ as.factor(P1_3), data=bd1hogarDummy))


summary(lm(
  h40.huellaPreStu ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd1hogarDummy))

test1<-lm(
  h40.huellaPreStu ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd1hogarDummy)

test2<-lm(
  h40.huellaPreStu ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`, data=bd1hogarDummy)

#Modelos - creando una fórmula 

as.formula(paste("h40.huellaPreStu~ ",paste(ifelse(grepl(" ",colnames(bd1hogarDummy)[22:97])|grepl("\\{",colnames(bd1hogarDummy)[22:97]),paste0("`",colnames(bd1hogarDummy)[22:97],"`"),colnames(bd1hogarDummy)[22:97]),collapse = "+")))


st1<-stargazer::stargazer(t0_tr,t1_tr, t2_tr, type="text")
st1b<-stargazer::stargazer(t0_tr,t1_tr, t2_tr, type="text")

car::vif()
car::vif(t0_tr)
car::vif(t1_tr)
car::vif(t2_tr)