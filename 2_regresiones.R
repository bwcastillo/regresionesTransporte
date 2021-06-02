library(tidyverse)

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


t0_tr<-summary(lm(
  h40.huellaPreWrk ~  as.factor(P3_1) + 
                      as.factor(`P5[{_3}].Rp`) + 
                      as.factor(`P5[{_6}].Rp`)+ 
                      as.factor(GSE)+
                      as.factor(P5_1)+ 
                      `Densidad del barrio (hab)` + 
                      `Distancia al centro Network`, data=bd1hogarDummy))

t1_tr<-summary(lm(
  h40.huellaPstWrk ~  as.factor(P3_1) + 
                      as.factor(`P5[{_3}].Rp`) + 
                      as.factor(`P5[{_6}].Rp`)+ 
                      as.factor(GSE)+
                      as.factor(P5_1)+ 
                      `Densidad del barrio (hab)` + 
                      `Distancia al centro Network`, data=bd1hogarDummy))



t2_tr<-summary(lm(huellaDicWrk_v2~
                    as.factor(GSE)+
                    as.factor(P3_1)+
                    as.factor(`P5[{_3}].Rp`)+
                    as.factor(`P5[{_6}].Rp`)+
                    as.factor(P5_1)+
                    `Densidad del barrio (hab)` + 
                    `Distancia al centro Network`, data=bd2hogarDummy))



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
"Distancia al centro Network")) %>% mutate(., data="T0")

t1<-select(bd1hogarDummy,
c("h40.huellaPstWrk",
"F0",
"GSE",
"P3_1",
"P5[{_3}].Rp",
"P5[{_6}].Rp",
"P5_1",
"Densidad del barrio (hab)",
"Distancia al centro Network"))%>% mutate(., data="T1")

t2<-select(bd2hogarDummy,
c("huellaDicWrk_v2",
"F0",
"GSE",
"P3_1",
"P5[{_3}].Rp",
"P5[{_6}].Rp",
"P5_1",
"Densidad del barrio (hab)",
"Distancia al centro Network"))%>% mutate(., data="T2")

colnames(t0)[1]<-"Huella"
colnames(t1)[1]<-"Huella"
colnames(t2)[1]<-"Huella"


transporte<-rbind(t0,t1,t2)


# Gráficos comparados -----------------------------------------------------

# GSE

ggplot(data=transporte,aes(y=Huella,x=GSE, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=P3_1, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=F0, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()+ theme(axis.text.x = element_text(angle=90, hjust = 1))

ggplot(data=transporte,aes(y=Huella,x=P3_1,col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=`P5[{_3}].Rp`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=`P5[{_6}].Rp`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=P5_1, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=`Densidad del barrio (hab)`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()

ggplot(data=transporte,aes(y=Huella,x=`Distancia al centro Network`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d()



# Nuevo desempeño ---------------------------------------------------------

performance::compare_performance(t0_tr, t1_tr,t2_tr)

plot(performance::compare_performance(t0_tr, t1_tr,t2_tr))
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




