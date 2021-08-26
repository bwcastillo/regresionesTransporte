library(tidyverse)

bd1hogarDummy<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/output/huellaHogares_total_v2Dummies.xlsx")
bd1hogar<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/output/bd1hogar.xlsx")
bd2hogar<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/output/bd2Hogares.xlsx")
bd2personas<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/output/bd2personas.xlsx")
bd1personas<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/output/bd1personas.xlsx")
encuesta1<-readxl::read_xlsx("C:/CEDEUS/2020/diciembre6_recalculoHuellaCOVID/BD_Cliente_PUC_Huella_de_Carbon_Urbana (2).xlsx",skip = 9)
encuesta2<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/input/BD_Cliente_PUC_Huella_de_Carbon_Urbana_ENERO_2021_V2/BD_Cliente_PUC_Huella_de_Carbon_Urbana_ENERO_2021_V2.xlsx",skip=8)
#bd2<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/input/BD_Cliente_PUC_Huella_de_Carbon_Urbana_ENERO_2021_V2/BD_Cliente_PUC_Huella_de_Carbon_Urbana_ENERO_2021_V2.xlsx",skip=8)
datat0<-readxl::read_xlsx("C:/CEDEUS/2021/mayo2_modeloRegresionTransporte/input/Datos_T0_T1.xlsx")

rm(encuesta)

colnames(encuesta1)
colnames(encuesta2)

View(encuesta2)
View(encuesta1)


# Dumificando sustentabilidad  --------------------------------------------


encuesta1<-encuesta1[,c(2,263:271)]
colnames(encuesta1)
encuesta1<-encuesta1[,-c(9)]
lapply(encuesta1,function(x){class(x)})
encuesta1<-fastDummies::dummy_columns(encuesta1)

encuesta2<-encuesta2[,c(1,252:264)]
lapply(encuesta2,function(x){class(x)})
colnames(encuesta2)
encuesta2<-encuesta2[,-c(10,13,14)]
encuesta2<-fastDummies::dummy_columns(encuesta2)


View(encuesta1)
encuesta1<-encuesta1[,-c(26)]

View(encuesta2)
encuesta2<-encuesta2[,-c(30,33)]



# Uniendo a dummy 1 ---------------------------------------------------------

encuesta1$Respondent_ID...2<-as.character(encuesta1$Respondent_ID...2)

bd1hogarDummy<-left_join(bd1hogarDummy, encuesta1, by= c("id_respondent"="Respondent_ID...2"))



# Dumificando otras variables y uniendoa a bd2hogar -----------------------

bd2hogarDummy<-fastDummies::dummy_cols(bd2hogar, colnames(bd2hogar)[c(2,5,10)]) 
bd2hogarDummy<-left_join(bd2hogarDummy, encuesta2, by= c("Respondent_ID"="Respondent_ID...1"))



# Agregando densidades y distancias ---------------------------------------
test<-select(datat0,c(4:6)) %>% .[!duplicated(.$`Barrio residencia`),]

bd1hogarDummy<-left_join(bd1hogarDummy,test, by=c("P1_3"="Barrio residencia"))
bd2hogarDummy<-left_join(bd2hogarDummy,test, by=c("P1_3"="Barrio residencia"))



# Dumificando barrios -----------------------------------------------------
bd1hogarDummy<-fastDummies::dummy_cols(bd1hogarDummy, "P1_3") 
bd2hogarDummy<-fastDummies::dummy_cols(bd2hogarDummy, "P1_3") 



# Guardando ---------------------------------------------------------------

writexl::write_xlsx(bd1hogarDummy, "output/bd1hogarDummy.xlsx")
writexl::write_xlsx(bd2hogarDummy, "output/bd2hogarDummy.xlsx")



# Pequeño test para ver si hizo un buen left ------------------------------


table(encuesta1$`P5[{_1}].Rp_No`)
table(bd1hogarDummy$`P5[{_1}].Rp_No`)

table(encuesta2$P5_2_No)
table(bd2hogarDummy$P5_2_No.y)




# Agregando datos de alimentos --------------------------------------------

encuesta1<-readxl::read_xlsx("C:/CEDEUS/2020/diciembre6_recalculoHuellaCOVID/BD_Cliente_PUC_Huella_de_Carbon_Urbana (2).xlsx",skip = 9)
encuesta2<-readxl::read_xlsx("C:/CEDEUS/2021/marzo1_huellaCarbono2/input/BD_Cliente_PUC_Huella_de_Carbon_Urbana_ENERO_2021_V2/BD_Cliente_PUC_Huella_de_Carbon_Urbana_ENERO_2021_V2.xlsx",skip=8)

View(encuesta1)
colnames(encuesta1)
View(encuesta2)
colnames(encuesta2)

tripfood1<-encuesta1[,c(2,248:249)]
tripfood2<-encuesta2[,c(1,232)]

rm(encuesta1)
rm(encuesta2)



View(bd1hogarDummy)
View(bd2hogarDummy)

tripfood1$Respondent_ID...2<-as.character(tripfood1$Respondent_ID...2)

bd1hogarDummy<-left_join(bd1hogarDummy,tripfood1,by=c("id_respondent"="Respondent_ID...2"))
bd2hogarDummy<-left_join(bd2hogarDummy,tripfood2,by=c("Respondent_ID"="Respondent_ID...1"))