library(tidyverse)


# Usa auto como dependiente y distancia independiente ---------------------


test<-lm(P3_1=="No"~
            `Distancia al centro Network`
            , data=bd2hogarDummy[bd2hogarDummy$`P4[{_1}].Rp`!=99,])%>% summary()



# Dep Distancia recorrida y explicativa constante y Distancia al c --------



test2<-split.data.frame(bd2personas,bd2personas$Respondent_ID) %>% 
  map(., ~as.data.frame(sum(.$disTtalTrbj_km))) %>% 
  bind_rows(., .id="Respondent_ID")


test<-bd2hogarDummy

test2$Respondent_ID<-as.double(test2$Respondent_ID)

test<-left_join(test, test2, by= "Respondent_ID")

colnames(test)[120]<-"distWrk"

test<-lm(distWrk~
           `Distancia al centro Network`, 
         data=test[test$P4.._1...Rp!=99,])%>% summary()


# New regressions with Carbon footprint divided by work persons -----------


# The case of bd1 ---------------------------------------------------------

View(bd1personas)

peopleWork<-split.data.frame(bd1personas,bd1personas$id_respondent) %>% 
  map(., ~as.data.frame(.) %>% 
           nrow(.)) %>% 
  bind_rows(.id="id_respond") %>% 
  t(.) %>% 
  cbind(id_respondent=rownames(.),.) %>% 
  as.data.frame(.)


# bd1personas$V2<-as.numeric(bd1personas$V2)
# 
# bd1personas<-left_join(bd1personas, peopleWork,  by="id_respondent")
# 
# bd1personas$h40wrkT0perCap<-round(bd1personas$h40.hue_TrbjaAntes/bd1personas$V2, digits=3)
# 
# bd1personas$h40wrkT1perCap<-round(bd1personas$h40.hue_TrbjaDspus/bd1personas$V2, digits=3)


bd1hogar<-left_join(bd1hogarDummy, peopleWork,  by="id_respondent")

bd1hogar$V2<-as.numeric(bd1hogar$V2)

bd1hogar$footPrintPerT0<-round(bd1hogar$h40.huellaPreWrk/bd1hogar$V2, digits=3)
bd1hogar$footPrintPerT1<-round(bd1hogar$h40.huellaPstWrk/bd1hogar$V2, digits=3)

bd1hogar$footPrintPerT0<-case_when(is.na(bd1hogar$footPrintPerT0)~0,
                        !is.na(bd1hogar$footPrintPerT1)~bd1hogar$footPrintPerT0)

bd1hogar$footPrintPerT1<-case_when(is.na(bd1hogar$footPrintPerT1)~0,
                        !is.na(bd1hogar$footPrintPerT1)~bd1hogar$footPrintPerT1)



# BD2 ---------------------------------------------------------------------


peopleWork2<-split.data.frame(bd2personas,bd2personas$Respondent_ID) %>% 
  map(., ~as.data.frame(.) %>% 
        nrow(.)) %>% 
  bind_rows(.id="Respondent_ID") %>% 
  t(.) %>% 
  cbind(Respondent_ID=rownames(.),.) %>% 
  as.data.frame(.)

peopleWork2$Respondent_ID<-as.numeric(peopleWork2$Respondent_ID)

bd2hogar<-left_join(bd2hogarDummy, peopleWork2,  by="Respondent_ID")

bd2hogar$V2<-as.numeric(bd2hogar$V2)

bd2hogar$footPrintPerT2<-round(bd2hogar$huellaDicWrk_v2/bd2hogar$V2,digits=3)


bd2hogar$footPrintPerT2<-case_when(is.na(bd2hogar$footPrintPerT2)~0,
                                   !is.na(bd2hogar$footPrintPerT2)~bd2hogar$footPrintPerT2)


# 
# bd1hogar-left_join(bd1hogar,tripfood1,by=c("id_respondent"="Respondent_ID...2"))
# bd2hogar<-left_join(bd2hogar,tripfood2,by=c("Respondent_ID"="Respondent_ID...1"))

# Haciendo regresiones ----------------------------------------------------

t0_tr_v2<-summary(lm(
  footPrintPerT0 ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`+ as.factor(F0), data=bd1hogar))

t1_tr_v2<-summary(lm(
  footPrintPerT1 ~  as.factor(P3_1) + as.factor(`P5[{_1}].Rp`) + as.factor(`P5[{_2}].Rp`) + as.factor(`P5[{_3}].Rp`) + as.factor(F0)+ as.factor(GSE)+
    as.factor(`P5[{_4}].Rp`) + as.factor(`P5[{_5}].Rp`) + as.factor(`P5[{_6}].Rp`) + as.factor(P5_1) + as.factor(P5_2)+`Densidad del barrio (hab)` + `Distancia al centro Network`+V2, data=bd1hogar))

t2_tr_v2<-summary(lm(footPrintPerT2~
                    as.factor(F0)+as.factor(GSE)+as.factor(P3_1)+as.factor(`P5[{_1}].Rp`)+as.factor(`P5[{_2}].Rp`)+as.factor(`P5[{_3}].Rp`)+as.factor(`P5[{_4}].Rp`)+as.factor(`P5[{_5}].Rp`)+as.factor(`P5[{_6}].Rp`)+as.factor(`P5[{_7}].Rp`)+as.factor(P5_1)+as.factor(P5_1)+as.factor(P5_2)+as.factor(P5_3)+`Densidad del barrio (hab)` + `Distancia al centro Network`+V2, data=bd2hogar))

# Cambiando nombre de variables B1 y B2 ----------------------------------------------

colnames(bd1hogar)[c(5,33:38,39)]<-c("tieneAuto","aislaVivienda","cambiaCalefactores","masBiciMasCamina","alimentacionSaludable","hogarRecicla","conoceContaminacion","bajaIngreso")
colnames(bd2hogar)[c(10,48:53,55)]<-c("tieneAuto","aislaVivienda","cambiaCalefactores","masBiciMasCamina","alimentacionSaludable","hogarRecicla","conoceContaminacion","bajaIngreso")
colnames(bd1hogar)[c(31)]<-c("F0_Temuco/PadreLasCasas")
colnames(bd2hogar)[c(39)]<-c("F0_Temuco/PadreLasCasas")

colnames(bd1hogar)[98:100]<-c("desplazaAlimentoT0","desplazaAlimentoT1","cantidadPersonas")
colnames(bd2hogar)[c(56,57,120)]<-c("rebajaPlanGobierno","empleadorBonoGasto","cantidadPersonas")
colnames(bd2hogar)[119]<-c("desplazaAlimentoT2")

# Regresiones V2
t0_trV2<-summary(lm(
  footPrintPerT0 ~  
    as.factor(tieneAuto) +
    as.factor(aislaVivienda) + 
    as.factor(cambiaCalefactores) + 
    as.factor(masBiciMasCamina) + 
    as.factor(alimentacionSaludable) +
    as.factor(hogarRecicla) + 
    as.factor(conoceContaminacion) + 
    as.factor(`F0_Coronel (Bíobío)`)+
    as.factor(`F0_Osorno (Los Lagos)`)+
    as.factor(`F0_Temuco/PadreLasCasas`)+
    #as.factor(`F0_Valdivia (Los Ríos)`)+
    as.factor(GSE_C1)+
    as.factor(GSE_C2)+
    as.factor(GSE_C3)+
    as.factor(GSE_D)+
    #as.factor(GSE_E)+
    as.factor(bajaIngreso) + 
    #as.factor(P5_2), +
    `Densidad del barrio (hab)` + 
    `Distancia al centro Network`+
    desplazaAlimentoT0+
    cantidadPersonas,
  data=bd1hogar))

t1_trV2<-summary(lm(
  footPrintPerT1 ~  
    as.factor(tieneAuto) +
    as.factor(aislaVivienda) + 
    as.factor(cambiaCalefactores) + 
    as.factor(masBiciMasCamina) + 
    as.factor(alimentacionSaludable) +
    as.factor(hogarRecicla) + 
    as.factor(conoceContaminacion) + 
    as.factor(`F0_Coronel (Bíobío)`)+
    as.factor(`F0_Osorno (Los Lagos)`)+
    as.factor(`F0_Temuco/PadreLasCasas`)+
    #as.factor(`F0_Valdivia (Los Ríos)`)+
    as.factor(GSE_C1)+
    as.factor(GSE_C2)+
    as.factor(GSE_C3)+
    as.factor(GSE_D)+
    #as.factor(GSE_E)+
    as.factor(bajaIngreso) + 
    #as.factor(P5_2), +
    `Densidad del barrio (hab)` + 
    `Distancia al centro Network`+
    desplazaAlimentoT1+
    cantidadPersonas,
  data=bd1hogar))


t2_trV2<-summary(lm(
  footPrintPerT2 ~  
    as.factor(tieneAuto) +
    as.factor(aislaVivienda) + 
    as.factor(cambiaCalefactores) + 
    as.factor(masBiciMasCamina) + 
    as.factor(alimentacionSaludable) +
    as.factor(hogarRecicla) + 
    as.factor(conoceContaminacion) + 
    as.factor(`F0_Coronel (Bíobío)`)+
    as.factor(`F0_Osorno (Los Lagos)`)+
    as.factor(`F0_Temuco/PadreLasCasas`)+
    #as.factor(`F0_Valdivia (Los Ríos)`)+
    as.factor(GSE_C1)+
    as.factor(GSE_C2)+
    as.factor(GSE_C3)+
    as.factor(GSE_D)+
    #as.factor(GSE_E)+
    as.factor(bajaIngreso) + 
    #as.factor(P5_2), +
    `Densidad del barrio (hab)` + 
    `Distancia al centro Network`+
    desplazaAlimentoT2+
    cantidadPersonas,
  data=bd2hogar))



# Cálculos 2021 -----------------------------------------------------------

mean(bd1personas$distKMEstudioAntes, na.rm = T)
round(mean(bd1personas$distKMEstudioDsps, na.rm = T),digits=2)
mean(bd2personas$disTtalEstd_km, na.rm = T)

mean(bd1personas$distKMTrabajoAntes, na.rm = T)
mean(bd1personas$distKMTrabajoDsps, na.rm = T)
mean(bd2personas$disTtalTrbj_km, na.rm = T)



# Regresiones v1 ----------------------------------------------------------


# Regresiones v1: Modelos trabajo -----------------------------------------
t0_tr<-
  summary(lm(
    h40.huellaPreWrk ~  
      as.factor(tieneAuto) +
      as.factor(aislaVivienda) + 
      as.factor(cambiaCalefactores) + 
      as.factor(masBiciMasCamina) + 
      as.factor(alimentacionSaludable) +
      as.factor(hogarRecicla) + 
      as.factor(conoceContaminacion) + 
      as.factor(`F0_Coronel (Bíobío)`)+
      as.factor(`F0_Osorno (Los Lagos)`)+
      as.factor(`F0_Temuco / Padre las Casas (La Araucanía)`)+
      #as.factor(`F0_Valdivia (Los Ríos)`)+
      as.factor(GSE_C1)+
      as.factor(GSE_C2)+
      as.factor(GSE_C3)+
      as.factor(GSE_D)+
      #as.factor(GSE_E)+
      as.factor(bajaIngreso) + 
      #as.factor(P5_2), +
      `Densidad del barrio (hab)` + 
      `Distancia al centro Network`, 
    data=bd1hogar))


t1_tr<-summary(lm(
  h40.huellaPstWrk ~  
    as.factor(tieneAuto) +
    as.factor(aislaVivienda) + 
    as.factor(cambiaCalefactores) + 
    as.factor(masBiciMasCamina) + 
    as.factor(alimentacionSaludable) +
    as.factor(hogarRecicla) + 
    as.factor(conoceContaminacion) + 
    as.factor(`F0_Coronel (Bíobío)`)+
    as.factor(`F0_Osorno (Los Lagos)`)+
    as.factor(`F0_Temuco / Padre las Casas (La Araucanía)`)+
    #as.factor(`F0_Valdivia (Los Ríos)`)+
    as.factor(GSE_C1)+
    as.factor(GSE_C2)+
    as.factor(GSE_C3)+
    as.factor(GSE_D)+
    #as.factor(GSE_E)+
    as.factor(bajaIngreso) + 
    #as.factor(P5_2), +
    `Densidad del barrio (hab)` + 
    `Distancia al centro Network`, 
  data=bd1hogar))


t2_tr<-summary(lm(huellaDicWrk_v2~
                    as.factor(tieneAuto) +
                    as.factor(aislaVivienda) + 
                    as.factor(cambiaCalefactores) + 
                    as.factor(masBiciMasCamina) + 
                    as.factor(alimentacionSaludable) +
                    as.factor(hogarRecicla) + 
                    as.factor(conoceContaminacion) + 
                    as.factor(`F0_Coronel (Bíobío)`)+
                    as.factor(`F0_Osorno (Los Lagos)`)+
                    as.factor(`F0_Temuco / Padre las Casas (La Araucanía)`)+
                    #as.factor(`F0_Valdivia (Los Ríos)`)+
                    as.factor(GSE_C1)+
                    as.factor(GSE_C2)+
                    as.factor(GSE_C3)+
                    as.factor(GSE_D)+
                    #as.factor(GSE_E)+
                    as.factor(bajaIngreso) + 
                    #as.factor(P5_2), +
                    `Densidad del barrio (hab)` + 
                    `Distancia al centro Network`, data=bd2hogar))


# Regresiones v1: Modelos trabajo -----------------------------------------


#Modelos estudio ---------------------

t0_es<-summary(lm(
  h40.huellaPreStu ~  
    as.factor(tieneAuto) +
    as.factor(aislaVivienda) + 
    as.factor(cambiaCalefactores) + 
    as.factor(masBiciMasCamina) + 
    as.factor(alimentacionSaludable) +
    as.factor(hogarRecicla) + 
    as.factor(conoceContaminacion) + 
    as.factor(`F0_Coronel (Bíobío)`)+
    as.factor(`F0_Osorno (Los Lagos)`)+
    as.factor(`F0_Temuco / Padre las Casas (La Araucanía)`)+
    #as.factor(`F0_Valdivia (Los Ríos)`)+
    as.factor(GSE_C1)+
    as.factor(GSE_C2)+
    as.factor(GSE_C3)+
    as.factor(GSE_D)+
    #as.factor(GSE_E)+
    as.factor(bajaIngreso) + 
    #as.factor(P5_2), +
    `Densidad del barrio (hab)` + 
    `Distancia al centro Network`, 
  data=bd1hogar))

t1_es<-summary(lm(
  h40.huellaPstStu ~  
    as.factor(tieneAuto) +
    as.factor(aislaVivienda) + 
    as.factor(cambiaCalefactores) + 
    as.factor(masBiciMasCamina) + 
    as.factor(alimentacionSaludable) +
    as.factor(hogarRecicla) + 
    as.factor(conoceContaminacion) + 
    as.factor(`F0_Coronel (Bíobío)`)+
    as.factor(`F0_Osorno (Los Lagos)`)+
    as.factor(`F0_Temuco / Padre las Casas (La Araucanía)`)+
    #as.factor(`F0_Valdivia (Los Ríos)`)+
    as.factor(GSE_C1)+
    as.factor(GSE_C2)+
    as.factor(GSE_C3)+
    as.factor(GSE_D)+
    #as.factor(GSE_E)+
    as.factor(bajaIngreso) + 
    #as.factor(P5_2), +
    `Densidad del barrio (hab)` + 
    `Distancia al centro Network`,  
  data=bd1hogar))

t2_es<-summary(lm(huellaDicStu_v2~
                    as.factor(tieneAuto) +
                    as.factor(aislaVivienda) + 
                    as.factor(cambiaCalefactores) + 
                    as.factor(masBiciMasCamina) + 
                    as.factor(alimentacionSaludable) +
                    as.factor(hogarRecicla) + 
                    as.factor(conoceContaminacion) + 
                    as.factor(`F0_Coronel (Bíobío)`)+
                    as.factor(`F0_Osorno (Los Lagos)`)+
                    as.factor(`F0_Temuco / Padre las Casas (La Araucanía)`)+
                    #as.factor(`F0_Valdivia (Los Ríos)`)+
                    as.factor(GSE_C1)+
                    as.factor(GSE_C2)+
                    as.factor(GSE_C3)+
                    as.factor(GSE_D)+
                    #as.factor(GSE_E)+
                    as.factor(bajaIngreso) + 
                    #as.factor(P5_2), +
                    `Densidad del barrio (hab)` + 
                    `Distancia al centro Network`,
                  data=bd2hogar))


# Sintetizando bases de datos V1 ---------------------------------------------

t0_v2<-select(bd1hogar,
           c("footPrintPerT0",
             "F0",
             "GSE",
             "tieneAuto",        #Tiene auto: tieneAuto
             "masBiciMasCamina", #Mas bici: masBiciMasCamina
             "conoceContaminacion", #conoce lvl Contaminacion: conoceContaminacion
             "bajaIngreso",        #Bajo el ingreso: bajaIngreso
             "Densidad del barrio (hab)",
             "Distancia al centro Network",
             "P4[{_1}].Rp"  #Desplazamiento compra de alimentos t0:
           )) %>% mutate(., data="T0")

t1_v2<-select(bd1hogar,
           c("footPrintPerT1",
             "F0",
             "GSE",
             "tieneAuto",
             "masBiciMasCamina",
             "conoceContaminacion",
             "bajaIngreso",
             "Densidad del barrio (hab)",
             "Distancia al centro Network",
             "P4[{_2}].Rp"))%>%  #Desplazamientos ultimo mes Invierno t1: 
  mutate(., data="T1") 

t2_v2<-select(bd2hogar,
           c("footPrintPerT2",
             "F0",
             "GSE",
             "tieneAuto",
             "masBiciMasCamina",
             "conoceContaminacion",
             "bajaIngreso",
             "Densidad del barrio (hab)",
             "Distancia al centro Network",
             "P4[{_1}].Rp"))%>% #Desplazamientos alimentos t2: 
  mutate(., data="T2")

colnames(t0_v2)[1]<-"Huella"
colnames(t1_v2)[1]<-"Huella"
colnames(t2_v2)[1]<-"Huella"

colnames(t0_v2)[10]<-"tripsFood"
colnames(t1_v2)[10]<-"tripsFood"
colnames(t2_v2)[10]<-"tripsFood"


transporte_v2<-rbind(t0_v2,t1_v2,t2_v2)


# Gráficos comparados -----------------------------------------------------

# GSE

g1_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=GSE, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de Carbono & GSE")+
  xlab("GSE")


g2_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=tieneAuto, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Tenencia de auto")+
  xlab("Tiene auto")

g3_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=F0, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+ theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(title = "Huella de carbono & Ciudad")+
  xlab("Ciudad")

g4_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=masBiciMasCamina, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Camina y anda más en bicicleta")+
  xlab("Camina y anda más en bicicleta")

g5_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=conoceContaminacion, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Conoce la calidad del aire de su ciudad")+
  xlab("Conoce la calidad del aire de su ciudad")

g6_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=bajaIngreso, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & ha bajado el ingreso promedio del hogar")+
  xlab("Ha bajado el ingreso promedio del hogar")

g7_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=`Densidad del barrio (hab)`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Densidad del barrio")+
  xlab("Densidad del barrio")


g8_v2<-ggplot(data=transporte_v2,aes(y=Huella,x=`Distancia al centro Network`, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Distancia (network) al centro")+
  xlab("Distancia (network) al centro")

g9_v2<-ggplot(data=transporte_v2[transporte_v2$tripsFood!=99,],aes(y=Huella,x=tripsFood, col=factor(data),group=factor(data)))+
  geom_point(position =  position_dodge(width = .5))+
  scale_color_viridis_d(name="Tiempo")+
  labs(title = "Huella de carbono & Viajes a comprar alimentos")+
  xlab("Viajes a comprar alimentos")


save(g1_v2,g2_v2,g3_v2,g4_v2,g5_v2,g6_v2,g7_v2,g8_v2,g9_v2,t0_trV2,t1_trV2,t2_trV2,file="output/graphicV2.RData")


write.csv(bd1hogar, "output/bd1hogar.csv")
write.csv(bd2hogar, "output/bd2hogar.csv")