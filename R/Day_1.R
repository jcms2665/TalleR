
#######################################################################################
#           CURSO DE CAPACITACIÓN DEL SOFTWARE R                                      #
#           Subdirección de Diseño Conceptual de Encuestas en Empleo                  #
#           Juan Trejo Magos                                                          #
#           Julio César Martínez Sánchez                                              #
#######################################################################################

#           Temario
#
# 0. Consideraciones iniciales
# 1. Frecuencias
# 2. Tabulados
# 3. Etiquetar variables
# 4. Recodificar variables
# 5. Subconjunto de datos


#######################################################################################
#0. Consideraciones iniciales

      #0.1  Instalar paquetes 
            install.packages("readxl")
            install.packages("sas7bdat")
            install.packages(c("foreign","data.table","questionr","base","car"))

      #0.2  Cargar las librerias
            library(data.table)
            library(foreign)
            library(questionr)
            library(base)
            library(car)
            library(readxl)      
            library(sas7bdat)
            
      #0.3  Limpiar el entorno de trabajo
            rm(list = ls())
            # Ctrl + L  Limpia la consola  
      
      #0.4  Definir el directorio 
            # Hay que utilizar \\ o / 
            setwd("F:/TallerR_COLMEX-master/TallerR_COLMEX-master/Datos")
      
      #0.5  Importar datos:     excel, dbf,spss, stata, csv, sas   ->   R 

            excel<-data.frame(read_excel("EXCEL_enoe.xlsx"))
            dbf<-data.frame(read.dbf("DBF_enoe.dbf"))
            spss<-data.frame(read.spss("SPSS_enoe.sav"))
            stata<-data.frame(read.dta("STATA_enoe.dta"))
            cvs<- data.frame(read.table("CVS_enoe.csv", header=TRUE, sep=","))
            sas<-data.frame(read.sas7bdat("SAS_enoe.sas7bdat",debug = FALSE))

      
      #0.6  Exportar datos:      R    ->    excel, dbf,spss, stata, csv, sas

            #0.6.1 Limpiamos y solo nos quedamos  con la base de spss
                    rm("excel","dbf","stata","cvs","sas") 
          
            #0.6.2 Guardamos en DTA (stata)
                    write.dta(spss, "Julio_R.dta")
            
            #0.6.3 Guardamos en DBF (foxpro)
                    write.dbf(spss, "Julio_R.dbf")
            
            #0.6.4 Guardamos en CSV (Excel)
                    write.table(spss, "Julio_R.txt", sep=",")

                          
      #0.7  Descripción de la base
            
            #0.7.1  Descripción de la base
                    names(spss)
                    head(spss,2)
      
            #0.7.2  Columnas (como en EXCEL)
                    spss[1,]
                    spss[1:3,]
            
            #0.7.3  Filas (como en EXCEL)
                    spss[,22]
                    spss[,c("SEX")]
      
            #0.7.4  Columnas y filas 
                    spss[1:3,9]
      
#######################################################################################
  
#1. Frecuencias
                    
      #1.0  Ayuda
            ?wtd.table

      #1.1  Frecuencias con datos muestrales (sin ponderar)
            wtd.table(spss$POS_OCU)

      #1.2  Frecuencias con datos ponderados
            wtd.table(spss$POS_OCU, weights=spss$FAC)


#######################################################################################
#2. Tabulados

      #2.1  Tabulados con datos muestrales (sin ponderar)
            wtd.table(spss$SEX,spss$POS_OCU)
            
      #2.2  Tabulados con datos ponderados
            wtd.table(spss$SEX, spss$POS_OCU, weights=spss$FAC)
      

#######################################################################################
#3. Etiquetar variables

      #3.1  Generar etiquetas (variable SEXO)
            spss$SEX <- factor(spss$SEX,levels = c(1,2),labels = c("Hombre", "Mujer"))
            
      #3.2  Tabular variable etiquetada
            wtd.table(spss$SEX, weights=spss$FAC)
            
      #3.3  Generar etiquetas (variable POS_OCU)
            spss$POS_OCU <- factor(spss$POS_OCU,levels = c(1,2,3,4,5),labels = c("Subordinados", "Empleadores","Cuenta Propia", "Sin Pago","NE"))
            
      #3.4  Tabulado 2x2 (SEXO x POS_OCU)
            wtd.table(spss$POS_OCU, spss$SEX, weights=spss$FAC)
            

#######################################################################################
#4. Recodificar variables
      
      #4.1  Recodificion de variables numericas (rangos)
            
            #4.1.1  Convertir a numéricas
            spss$EDA <-as.numeric(as.character(spss$EDA))
            
            #4.1.2  Crear nueva variable
            spss$EDA_5categ<-0
            
            #4.1.3  Establecer los rangos
            spss$EDA_5categ[spss$EDA >= 0 & spss$EDA <=10] <- 1
            spss$EDA_5categ[spss$EDA >= 11 & spss$EDA <=20] <- 2
            spss$EDA_5categ[spss$EDA >= 21 & spss$EDA <=30] <- 3
            spss$EDA_5categ[spss$EDA >= 31 & spss$EDA <=40] <- 4
            spss$EDA_5categ[spss$EDA >= 41] <- 5          

            #4.1.4 Validar
            wtd.table(spss$EDA_5categ)

      #4.2  Recodificion de variables tipo caracter
            
            #4.2.1  Convertir a caracter
            spss$R_DEF <- as.character(spss$R_DEF)
            
            #4.2.2  Crear nueva variable
            spss$R_DEF_VALIDO <-" "
            
            #4.2.3  Establecer los rangos
            
            spss$R_DEF_VALIDO[spss$R_DEF!="00"] <- "Entrevista incompleta"
            spss$R_DEF_VALIDO[spss$R_DEF=="00"] <- "Entrevista completa"

            #4.2.4 Validar
            wtd.table(spss$R_DEF_VALIDO)
                 
#######################################################################################
#5. Subconjunto de datos

      #5.1  Seleccionar variables
            
            #5.1.1 Definimos las variables
                   var<-c("SEX", "POS_OCU")
            
            #5.1.2 Seleccionamos SÓLO esas variables
                   nueva_spss_1 <- spss[,var]
            

      #5.2  Seleccionar casos          
            nueva_spss_2 <- spss[ which(as.numeric(spss$EDA)<18), ]


      #5.3  Seleccionar variables y casos
            nueva_spss_1 <- spss[which(as.numeric(spss$EDA)<18), c("SEX", "POS_OCU")]


