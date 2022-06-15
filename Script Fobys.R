rm(list=ls())

## levanto tabla recursos huamanos que me pasa DEN
## luego la vamos armar toda en r

library(pacman)
p_load(readxl,tidyverse,lubridate,stringi,stringr,data.table,tidytable,stringi)

archivos<-list.files("../FOBYS/")
archivos

## traigo la tabla en csv
tabla_fobys<- read.csv("../FOBYS/fobys.csv",sep = ";") 
tabla_fobys<-tabla_fobys[-c(1,2),] 
## levantamos actas 2021 y 2022 de fecha de generación de documento
actas_2021 <- read.csv("../FOBYS/actas_2021.csv",sep = ";") ## este no cambia
actas_2022 <- read.csv("../FOBYS/actas_2022.csv",sep = ";") ## este cambia en cada actcualización

## uno ambas actas 2021 y 2022
actas_2021_22<-rbind(actas_2021, actas_2022) %>% 
  select(1,2) %>% 
  rename("Número.documento.GEDO"="Número")

## uno tablas contra fobys
tabla_fobys_actas<- tabla_fobys %>% 
  left_join(actas_2021_22, by=c("Número.documento.GEDO"))

## separo hora de fecha
tabla_fobys_actas$`Fecha de creación del documento` <- word(tabla_fobys_actas$Fecha.de.creación,1, sep = fixed(" "))
tabla_fobys_actas$`Fecha de creación del documento`<-  dmy(tabla_fobys_actas$`Fecha de creación del documento`)
tabla_fobys_actas$Fecha.de.recepción<-  dmy(tabla_fobys_actas$Fecha.de.recepción)

## armo las variables de tiempo días desde fecha creación y generación acta
tabla_fobys_actas$`Días hasta la generación del acta`<-tabla_fobys_actas$`Fecha de creación del documento` - 
  as.Date(tabla_fobys_actas$Fecha.de.recepción, format="%d/%m/%Y")

#negativos <-tabla_fobys_actas[tabla_fobys_actas$`Días hasta la generación del acta`<0,]

## paso los negativos de la viarable dias desde a NA
tabla_fobys_actas <- tabla_fobys_actas %>% 
mutate(`Días hasta la generación del acta`=ifelse(`Días hasta la generación del acta`<0,NA,`Días hasta la generación del acta`))

#agrupador de promedio de dias por dependencia reeceptora y n actas - Resumen
#actas por mes año, cantidad de actas por año y mediana de días total
#dias hhasta la generación del acta.

## modifico variable remito para que se visualice alfanumérica

# options(scipen = 999)
# tabla_fobys_actas <- tabla_fobys_actas %>%
#   mutate.(variable.1 = ifelse.(stri_detect_fixed(Remito.n., "E+"), Remito.n., NA)) %>%
#   mutate.(variable.2 = ifelse.(is.na(variable.1), Remito.n., NA)) %>% 
#   mutate(variable.3=as.numeric(str_replace_all(variable.1, 
#                                                c(','='.')))) %>% 
#   mutate(variable.4=ifelse(is.na(variable.3),variable.2,variable.3))
# 
# tabla_fobys_actas <- tabla_fobys_actas %>%
#   mutate.(variable.1 = ifelse.(stri_detect_fixed(Remito.n., "E+"), Remito.n., NA)) %>%
#   mutate.(variable.2 = ifelse.(is.na(variable.1), Remito.n., NA)) %>% 
#   mutate(variable.3=as.numeric(str_replace_all(variable.1, 
#                                                c(','='.')))) %>% 
#   mutate(variable.3=format(variable.3,scientific = FALSE)) %>% 
#   mutate(variable.3=as.character(variable.3)) %>% 
#   mutate(variable.4=ifelse(variable.3=="          NA",variable.2,variable.3)) %>% 
#   mutate(variable.4=as.factor(variable.4)) 
# 
# tabla_fobys_actas$var.3<-(!str_detect(tabla_fobys_actas$Remito.n.,"\\E\\+"))
# tabla_fobys_actas$var.4<-(str_detect(tabla_fobys_actas$Remito.n.,"\\E\\+"))
# 
# tabla_fobys_actas<-tabla_fobys_actas %>%
#   mutate(var4=ifelse (var.3==FALSE,Remito.n.,NA))
# 
# tabla_fobys_actas<-tabla_fobys_actas %>% 
# mutate(var4=as.numeric(str_replace_all(var4, 
#                                              c(','='.'))))
# 
# tabla_fobys_actas$var4<-as.character(tabla_fobys_actas$var4) 
# 
# tabla_fobys_actas<-tabla_fobys_actas %>%
#   mutate(var5=ifelse (var.3==TRUE ,Remito.n.,NA))
# 
# tabla_fobys_actas$var.6<-ifelse(is.na(tabla_fobys_actas$var4),tabla_fobys_actas$var5,tabla_fobys_actas$var4)
# names(tabla_fobys_actas)


## seleccionamos variables para listado
tabla_fobys_actas <-tabla_fobys_actas %>% 
  select(1,2,3,4,5,6,7,8,9,12,13,14,15,16,11,18,19)

## noombro las varialbes
names(tabla_fobys_actas)<-c("Número doc GEDO",	
                            "Dependencia receptora"	,
                            "Orden de compra",	
                            "N Contratación - Licitación"	,
                            "Resolución Aprobatoria",
                            "Expediente",
                            "Entidad proveedora",
                            "Cuit Proveedor",
                            "Remito",
                            "Renglon",
                            "Cantidad",
                            "Detalle",
                            "Unidad",
                            "Observaciones",
                            "Fecha de recepción",
                            "Fecha creación del documento",
                            "Días hasta la generación del acta")
#view(tabla_fobys_actas)
## mormalizo dependencia receptora

tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Coberturas de Alto Precio"]<-"Dirección de Coberturas de Alto Precio"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE COBERTURAS DE ALTO PRECIO"]<-"Dirección de Coberturas de Alto Precio"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Coberturas de Medicamentos Especiales y de Alto Precio"]<-"Dirección de Coberturas de Alto Precio"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE COBERTURAS DE MEDICAMENTOS ESPECIALES Y DE ALTO PRECIO"]<-"Dirección de Coberturas de Alto Precio"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Comunicación Institucional y Prensa"]<-"Dirección de Comunicación Institucional y Prensa"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE COMUNICACIÓN INSTITUCIONAL Y PRENSA"]<-"Dirección de Comunicación Institucional y Prensa"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Control de Enfermedades Inmunoprevenibles"]<-"Dirección de Control de Enfermedades Inmunoprevenibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE CONTROL DE ENFERMEDADES INMUNOPREVENIBLES"]<-"Dirección de Control de Enfermedades Inmunoprevenibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Control de Enfermedades Transmitidas por Vectores"]<-"Dirección de Control de Enfermedades Transmisibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE CONTROL DE ENFERMEDADES TRANSMITIDAS POR VECTORES"]<-"Dirección de Control de Enfermedades Transmisibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Géneros y Diversidad"]<-"Dirección de Géneros y Diversidad"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE GÉNEROS Y DIVERSIDAD"]<-"Dirección de Géneros y Diversidad"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Gestión Operativa"]<-"Dirección de Gestión Operativa"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE GESTIÓN OPERATIVA"]<-"Dirección de Gestión Operativa"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Medicina Transfusional"]<-"Dirección de Medicina Transfusional"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Respuesta al VIH, ITS, Hepatitis Virales y Tuberculosis"]<-"Dirección de Respuesta al VIH, ITS, Hepatitis Virales y Tuberculosis"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE RESPUESTA AL VIH, ITS, HEPATITIS VIRALES Y TUBERCULOSIS"]<-"Dirección de Respuesta al VIH, ITS, Hepatitis Virales y Tuberculosis"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE RESPUESTA AL VIH, ITS, HEPATITIS VIRALES Y TUBERCULOSIS "]<-"Dirección de Respuesta al VIH, ITS, Hepatitis Virales y Tuberculosis"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE RESPUESTA AL VIH, ITS, HEPATITIS VIRALES Y TUBERCULOSIS\t\t\t\t"]<-"Dirección de Respuesta al VIH, ITS, Hepatitis Virales y Tuberculosis"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE RESPUESTA AL VIH, ITS, HEPATITIS VIRALES Y TUBERCULOSIS\t\t\t\t\t"]<-"Dirección de Respuesta al VIH, ITS, Hepatitis Virales y Tuberculosis"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE SALUD BUCODENTAL"]<-"Dirección de Salud Bucodental"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Salud Perinatal y Niñez"]<-"Dirección de Salud Perinatal y Niñez"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE SALUD PERINATAL Y NIÑEZ"]<-"Dirección de Salud Perinatal y Niñez"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Salud Sexual y Reproductiva"]<-"Dirección Nacional de Salud Sexual y Reproductiva"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE TECNOLOGÍA Y SOPORTE"]<-"Dirección de Tecnología y soporte"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección de Tecnologías de la Información y de las Comunicaciones"]<-"Dirección de Tecnologías de la Información y de las Comunicaciones"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN DE TECNOLOGÍAS DE LA INFORMACIÓN Y DE LAS COMUNICACIONES"]<-"Dirección de Tecnologías de la Información y de las Comunicaciones"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección General de Recursos Humanos"]<-"Dirección General de Recursos Humanos"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN GENERAL DE RECURSOS HUMANOS"]<-"Dirección General de Recursos Humanos"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Abordaje Integral de Enfermedades No Transmisibles"]<-"Dirección Nacional de Abordaje Integral de Enfermedades No Transmisibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE ABORDAJE INTEGRAL DE ENFERMEDADES NO TRANSMISIBLES"]<-"Dirección Nacional de Abordaje Integral de Enfermedades No Transmisibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Abordaje por Curso de Vida"]<-"Dirección Nacional de Abordaje por Curso de Vida"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Control de Enfermedades Transmisibles"]<-"Dirección de Control de Enfermedades Transmisibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE CONTROL DE ENFERMEDADES TRANSMISIBLES"]<-"Dirección de Control de Enfermedades Transmisibles"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Emergencias Sanitarias"]<-"DINESA"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE EMERGENCIAS SANITARIAS"]<-"DINESA"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Epidemiología e Información Estratégica"]<-"Dirección Nacional de Epidemiología e Información Estratégica"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Gobernanza e Integración de los Sistemas de Salud"]<-"Dirección Nacional de Gobernanza e Integración de los Sistemas de Salud"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Medicamentos y Tecnología Sanitaria"]<-"Dirección Nacional de Medicamentos y Tecnología Sanitaria"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE MEDICAMENTOS Y TECNOLOGÍA SANITARIA"]<-"Dirección Nacional de Medicamentos y Tecnología Sanitaria"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Dirección Nacional de Salud Sexual y Reproductiva"]<-"Dirección Nacional de Salud Sexual y Reproductiva"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE SALUD SEXUAL Y REPRODUCTIVA"]<-"Dirección Nacional de Salud Sexual y Reproductiva"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE SALUD SEXUAL Y REPRODUCTIVA\t\t\t\t"]<-"Dirección Nacional de Salud Sexual y Reproductiva"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="DIRECCIÓN NACIONAL DE TALENTO HUMANO Y CONOCIMIENTO"]<-"Dirección Nacional de Talento Humano y Conocimiento"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Instituto Nacional Central Único Coordinador de Ablación e Implante"]<-"INCUCAI"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="INSTITUTO NACIONAL CENTRAL ÚNICO COORDINADOR DE ABLACIÓN E IMPLANTE"]<-"INCUCAI"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Instituto Nacional del Cáncer"]<-"Instituto Nacional del Cáncer"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="INSTITUTO NACIONAL DEL CÁNCER"]<-"Instituto Nacional del Cáncer"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Secretaría de Acceso a la Salud"]<-"Secretaría de Acceso a la Salud"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="SECRETARÍA DE ACCESO A LA SALUD"]<-"Secretaría de Acceso a la Salud"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Subsecretaría de Estrategias Sanitarias"]<-"Subsecretaría de Estrategias Sanitarias"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="SUBSECRETARÍA DE ESTRATEGIAS SANITARIAS"]<-"Subsecretaría de Estrategias Sanitarias"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="Subsecretaría de Medicamentos e Información Estratégica"]<-"Subsecretaría de Medicamentos e Información Estratégica"
tabla_fobys_actas$`Dependencia receptora`[tabla_fobys_actas$`Dependencia receptora`=="SUBSECRETARÍA DE MEDICAMENTOS E INFORMACIÓN ESTRATÉGICA"]<-"Subsecretaría de Medicamentos e Información Estratégica"

### agrupadores para tabla resumen

tabla.resumen_depe.rec <- tabla_fobys_actas %>%
  dplyr::group_by(`Dependencia receptora`) %>% 
  dplyr::summarise(`Cantidad de actas`=n(),  
                   `Promedio de días desde la creación documento`=(round(mean(`Días hasta la generación del acta`,na.rm = TRUE)))) %>% 
  ungroup()

## paso a factores las character
tabla.resumen_depe.rec <- tabla.resumen_depe.rec %>% 
  mutate_if(is.character,as.factor)

