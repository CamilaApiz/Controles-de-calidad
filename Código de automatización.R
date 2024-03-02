## Librerias
library(readr)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)
library(stringr)
library(robustbase)
library(data.table)

## Eliminar notacion cientifica
options(scipen=999)

####---- Lectura de datos ----#####

mes_ant <- read_csv("DATOS_DIC22.csv")
mes_act <- read_csv("DATOS_ENERO23.csv")

## Columna que identifica el mes. Por ahora es "manual".
## De manera general podriamos usar un: "Mes actual" - "Mes ant", o bien que sea cargado antes
mes_ant$Mes <- "Mes Ant"
mes_act$Mes <- "Mes Act"

## Nombre de columnas. De esta forma siempre se trabajaran con los mismos nombres.
## IMPORTANTE: los archivos imput siempre deben tener el mismo orden de las columnas.
columnas <- c("Ean_Cod", "Unidades", "Facturacion", "Neen", "Celda", "Desc_Celda",
              "Area_Scentia", "Area_Scentia2", "FactorAJ", "Ean_Desc", "Categoria", "Mes")

colnames(mes_ant) <- columnas
colnames(mes_act) <- columnas

#### ---- Funciones ----####

## Funcion para ir guardando las medidas y los impactos de los controles - General
medidas_control <- function(df) {
  medidas <- df %>%
    summarize(cantidad_registros = n(),
              suma_unidades = sum(UNI_PROY),
              suma_facturacion = sum(FACT_PROY),
              cantidad_eans_distintos = n_distinct(Ean_Cod))
  return(medidas)
}

porcentaje_perdida <- function(df1, df2) {
  porc = 1 - (df1 / df2)*100
  return(porc)
}

## Funcion para ir guardando las medidas y los impactos de los controles - Agrupados
medidas_control_agrup <- function(df, variables_agrupacion) {
  medidas <- df %>%
    group_by(across(all_of(variables_agrupacion))) %>%
    summarize(suma_unidades = sum(UNI_PROY),
              suma_facturacion = sum(FACT_PROY),
              cantidad_eans_distintos = n_distinct(Ean_Cod)) 
  return(medidas)
}

porcentaje_perdida_agrup <- function(df1, df2, variables_agrupacion) {
  df <- merge(df1, df2, by = variables_agrupacion)
  df <- df %>%
    group_by(across(all_of(variables_agrupacion))) %>%
    summarise('%_perdida_uni' = (suma_unidades.x/suma_unidades.y)*100,
              '%_perdida_fact' = (suma_facturacion.x/suma_facturacion.y)*100,
              '%_perdida_eans' = (cantidad_eans_distintos.x/cantidad_eans_distintos.y)*100)
  return(df)
}

calcular_medcouple <- function(datos) {
  mc_resultado <- tryCatch(
    mc(datos),
    error = function(e) NA
  )
  
  if (is.na(mc_resultado)) {
    resultado <- FALSE
    return(resultado)
  } else {
    return(TRUE)
  }
}

#### ---- Archivo inicial ----####

datos_template <- rbind(mes_ant, mes_act)
rm(mes_act, mes_ant)

## Calculo de Unidades y Facturacion proyectadas
datos_template$UNI_PROY <- datos_template$Unidades * datos_template$FactorAJ
datos_template$FACT_PROY <- datos_template$Facturacion * datos_template$FactorAJ

## Probar la existencia de duplicados en la base
datos_template$Codigo <- paste(datos_template$Neen, datos_template$Ean_Cod, datos_template$Mes, sep = "-")
duplicados <- datos_template %>%
  filter(duplicated(Codigo) | duplicated(Codigo, fromLast = TRUE))
## Se dejan las filas con mayor volumen vendido - Solo en el caso de que existan filas duplicadas
if (nrow(duplicados) > 0){
  codigos_duplicados <- duplicados$Codigo
  datos_template <- subset(datos_template, !(Codigo %in% codigos_duplicados))
  duplicados <- duplicados %>%
    group_by(Codigo) %>%
    slice(which.max(Unidades))
  datos_template <- rbind(datos_template, duplicados)
}

## Lectura IM
IM <- read.xlsx("IM.xlsx") 
IM <- rename (IM, Ean_Cod = EAN_TEXTO)

## Agregar a la base las variables del IM para cada EanCod
datos_template <- merge(datos_template, IM, by = "Ean_Cod")

## Calcular Volumen y PrxVol 
## El calculo del Volumen se hace a partir de la variable "UnidadMedida" del IM, para tener en cuenta
## en que unidad se reporta cada categoria
datos_template  <- mutate(datos_template, "Volumen" = case_when(datos_template$UnidadMedida == "IN" ~ Unidades*Contenido*PesoVolumen,
                                                                datos_template$UnidadMedida == "PAÑOS" ~ Unidades*Contenido*PesoVolumen,
                                                                datos_template$UnidadMedida == "PC" ~ Unidades*Contenido*PesoVolumen,
                                                                datos_template$UnidadMedida == "UN" ~ Unidades*Contenido*PesoVolumen,
                                                                datos_template$UnidadMedida == "UNIDADES" ~ Unidades*Contenido*PesoVolumen,
                                                                TRUE ~ (Unidades*Contenido*PesoVolumen)/1000))
datos_template$PrVol <- datos_template$Facturacion / datos_template$Volumen 
datos_template$VOL_PROY <- datos_template$Volumen * datos_template$FactorAJ
datos_template <- select(datos_template, -`Categoria.x`)
datos_template <- rename(datos_template, Categoria = `Categoria.y`)

### Convertimos en data.table 
datos_template <- as.data.table(datos_template)

#### ---- CONTROL 1: EANs inexistentes en el mercado ----####

## Si un Ean viene solo en una tienda -> se elimina la linea
## Objetivo: Contar cuantas veces al mes aparece el Ean
## Metodo: Agrupamos por Ean_Cod y mes, contando la cantidad de veces que aparece cada registro.
## Si esa cantidad es 1 -> el ean_cod solo se encuentra en un Neen (basado en el supuesto de que
## cada fila del archivo es unica, es decir, representa un registro del tipo NEEN-EAN)
C1 <- datos_template %>%
  group_by(Ean_Cod, Mes) %>%
  summarise(cantidad = n(), Neen, Ean_Desc, Codigo, Categoria, Mes, Celda, Area_Scentia2, UNI_PROY, FACT_PROY) %>%
  filter(cantidad == 1)

## Miramos el porcentaje de perdida de unidades de las categorias por mes
## Accion: si el %perdida < 5% -> eliminamos las filas de la categoria
########## si el %perdida > 5% -> se mira en detalle
## Obtenemos las medidas de los registros detectados en el primer control
medidas_C1_cat <- medidas_control_agrup(C1, c("Categoria", "Mes")) 
## Unidades, facturacion y cantidad de registros por categoria en el inicio
cat <- medidas_control_agrup(datos_template, c("Categoria", "Mes"))
medidas_C1_cat <- merge(medidas_C1_cat, cat, by = c("Categoria", "Mes"), all.y = TRUE)
medidas_C1_cat <- medidas_C1_cat[,c("Categoria", "Mes", "suma_unidades.x", "suma_facturacion.x", "cantidad_eans_distintos.x")]
colnames(medidas_C1_cat) <- c("Categoria", "Mes", "suma_unidades", "suma_facturacion", "cantidad_eans_distintos")
medidas_C1_cat[is.na(medidas_C1_cat)] <- 0 # Medidas de los registros detectados en C1 (categoria-mes)
perdida_C1_cat <- porcentaje_perdida_agrup(medidas_C1_cat, cat, c("Categoria", "Mes")) # Porcentaje de registros detectados en C1 (categoria-mes)

## %perdida > 5% 
perdida_C1_cat <- perdida_C1_cat[,c("Categoria", "Mes", "%_perdida_uni")]
C1 <- merge(C1, perdida_C1_cat, by = c("Categoria", "Mes"))
C1_mayor5 <- C1[C1$`%_perdida_uni` > 5, ]
C1_mayor5 <- C1_mayor5[,c("Categoria", "Mes", "Ean_Cod", "Ean_Desc", "UNI_PROY", "FACT_PROY", "%_perdida_uni")]

## %perdida < 5%
## Se eliminan del archivo:
C1_menor5 <- C1[C1$`%_perdida_uni` < 5, ]
C1_menor5 <- C1_menor5[,c("Categoria", "Mes", "Ean_Cod", "Ean_Desc", "UNI_PROY", "FACT_PROY", "%_perdida_uni")]
#write.xlsx(C1_menor5, "C1.xlsx")
codigos_C1 <- C1$Codigo[C1$`%_perdida_uni` < 5 & C1$Mes == "Mes Act"] #Guardamos los codigos que se eliminarian en el control 1
# Se crea una variable dicotomica: 0 si el registro se debe eliminar, 1 si no
datos_template$C1 <- ifelse(datos_template$Codigo %in% codigos_C1, 0, 1)
# Se eliminan los registros
## Este sera el data que usaremos para el siguiente control
datos_template_C1 <- subset(datos_template, datos_template$C1 == 1)

#### ---- CONTROL 2: Datos en 0, incompletos o negativos ----####

## Si un Ean tiene UNI o FACT menor o igual 0 -> se elimina el registro
C2 <- datos_template_C1[datos_template_C1$UNI_PROY <= 0 | datos_template_C1$FACT_PROY <= 0,]

## Eliminamos los registros
codigos_C2 <- C2$Codigo
datos_template_C1$C2 <- ifelse(datos_template_C1$Codigo %in% codigos_C2, 0, 1)
## Creamos la variable C2 en datos_template para llevar el registro en el archivo general
## C2 = O si el registro es detectado y eliminado en C2
## C2 = 1 si el registro no es detectado en C2
datos_template$C2 <- ifelse(datos_template$Codigo %in% codigos_C2, 0, 1)
## Este sera el data que usaremos para el siguiente control
datos_template_C2 <- subset(datos_template_C1, datos_template_C1$C2 == 1)

rm(datos_template_C1, codigos_C2, codigos_C1, C1, C1_mayor5, C1_menor5, cat, C2, IM, duplicados,
   medidas_C1_cat, perdida_C1_cat)

#### ---- CONTROL 3: Control de conversion a volumen ----####

## Calculamos el precio por volumen proyectado
datos_template_C2$PrVol_PROY <- datos_template_C2$FACT_PROY / datos_template_C2$VOL_PROY 

## Calcular para cada Ean la mediana del PrxVol a nivel nacional para el mes en curso
mediana <- datos_template_C2 %>%
  group_by(Ean_Cod) %>%
  filter(Mes == "Mes Act") %>%
  summarise(Pr_Mediano = median(PrVol_PROY), Lim_Sup_C3 = 4*Pr_Mediano, Lim_Inf_C3 = 0.25*Pr_Mediano, cuenta = n())

## Para cada Ean se evalua si su precio esta dentro del intervalo:
## (0.25*Pr_Mediano; 4*Pr_Mediano)
## -> si esta por fuera del intervalo y tiene 11 o mas registros lo identificamos como un precio con error de conversion 1 ("PEC1")
## -> si esta por fuera del intervalo y tiene menos de 11 registros lo identificamos como un precio con error de conversion 2 ("PEC2")
## -> si esta dentro del intervalo lo identificamos como un precio sin error de conversion ("PSEC")
datos_template_C2 <- merge(datos_template_C2, mediana, by = "Ean_Cod", all.x = TRUE)
condicion_mes <- datos_template_C2$Mes == "Mes Act"
datos_template_C2$dentro_limites <- ifelse(condicion_mes & (datos_template_C2$PrVol_PROY >= datos_template_C2$Lim_Inf_C3 & datos_template_C2$PrVol_PROY <= datos_template_C2$Lim_Sup_C3), 
                                           TRUE, FALSE)
datos_template_C2$dentro_limites[!condicion_mes] <- TRUE
datos_template_C2 <- mutate(datos_template_C2, "C3" = case_when(datos_template_C2$dentro_limites == FALSE & datos_template_C2$cuenta >= 11 ~ "PEC1",
                                                                datos_template_C2$dentro_limites == FALSE & datos_template_C2$cuenta < 11 ~ "PEC2",
                                                                TRUE ~ "PSEC"))

## Seleccionamos los registros identificados como PEC1
C3_PEC1 <- subset(datos_template_C2, datos_template_C2$C3 == "PEC1")

## Para los eans identificados como PEC1
## Recalculamos las UNI_PROY como:
## UNI_PROY / (PR_VOL_PROY / PR_MEDIANO)^(-1)
C3_PEC1$UNI_PROY2 <- C3_PEC1$UNI_PROY / ((C3_PEC1$PrVol_PROY / C3_PEC1$Pr_Mediano)^(-1))  
## Para estos productos volvemos a calcular el VOL y Pr_Vol proyectados
C3_PEC1  <- mutate(C3_PEC1, "VOL_PROY2" = case_when(UnidadMedida == "IN" ~ UNI_PROY2*Contenido*PesoVolumen,
                                                    UnidadMedida == "PAÑOS" ~ UNI_PROY2*Contenido*PesoVolumen,
                                                    UnidadMedida == "PC" ~ UNI_PROY2*Contenido*PesoVolumen,
                                                    UnidadMedida == "UN" ~ UNI_PROY2*Contenido*PesoVolumen,
                                                    UnidadMedida == "UNIDADES" ~ UNI_PROY2*Contenido*PesoVolumen,
                                                    TRUE ~ (UNI_PROY2*Contenido*PesoVolumen)/1000))
C3_PEC1$VOL_PROY2 <- as.numeric(C3_PEC1$VOL_PROY2)
C3_PEC1$PrVol_PROY2 <- C3_PEC1$FACT_PROY / C3_PEC1$VOL_PROY2

## Los eans detectados en el control 3 como PEC1 tienen nuevos valores de UNI, VOL, y PrVol proyectados
## Agregamos en datos_template 3 nuevas columnas con estos valores para los eans que se detectan
## y los valores iniciales para los que no. 
datos_template_C3 <- subset(datos_template_C2, datos_template_C2$C3 == "PSEC" | datos_template_C2$C3 == "PEC2")
datos_template_C3$VOL_PROY2  <- datos_template_C3$VOL_PROY 
datos_template_C3$UNI_PROY2  <- datos_template_C3$UNI_PROY
datos_template_C3$PrVol_PROY2  <- datos_template_C3$PrVol_PROY
datos_template_C3 <- rbind(datos_template_C3, C3_PEC1) ## Archivo para el proximo control
## Recalculamos Unidades y Volumen sin proyectar
datos_template_C3$Unidades2 <- datos_template_C3$UNI_PROY2 / datos_template_C3$FactorAJ
datos_template_C3$Volumen2 <- datos_template_C3$VOL_PROY2 / datos_template_C3$FactorAJ

## Los EAN señalados con PEC2 son aquellos que tienen un precio por fuera de los limites pero
## que tienen 11 o menos observaciones para calcular el intervalo, entonces no son corregidos
## Vamos a marcar con PEC2 a todos los registros de esos ean, no solo a los que tienen el precio
## por fuera del intervalo
## De esta forma, la persona a cargo de corregir los PEC2 puede ver todos los registros del EAN
codigos_PEC2 <- subset(datos_template_C3, datos_template_C3$C3 == "PEC2")
codigos_PEC2 <- codigos_PEC2[, c("Ean_Cod")]
datos_template_C3$C3_aux <- ifelse(datos_template_C3$Ean_Cod %in% codigos_PEC2, "PEC2", datos_template_C3$C3)
## Reemplazamos la columna C3 por la que creamos recien
datos_template_C3 <- select(datos_template_C3, -C3)
datos_template_C3 <- rename(datos_template_C3, C3 = C3_aux)

### Agregar columnas en el archivo inicial Datos_template para tener un registro
C3 <- datos_template_C2[, c("Codigo", "C3")]
datos_template <- merge(datos_template, C3, by = "Codigo", all = TRUE) 
rm(C3, datos_template_C2, C3_PEC1, mediana, condicion_mes, codigos_PEC2, codigos_duplicados, columnas)

#### ---- CONTROL 4: Control de precios ----####

### PARTE 1
### Nos quedamos con los Neen-EAN que tienen presencia en los dos meses, es decir, que tenemos
### precios para comparar del mes anterior y el actual
datos_template_C3$Codigo_Neen_Ean <- paste(datos_template_C3$Ean_Cod, datos_template_C3$Neen, sep = "-")
C4_comunes <- datos_template_C3 %>%
  group_by(Codigo_Neen_Ean) %>%
  filter(all(c("Mes Ant", "Mes Act") %in% Mes))
## Crear variable que distinga GBA o INTERIOR
C4_comunes$`GBA/INT`  <- ifelse(C4_comunes$Area_Scentia == "GBA", "GBA", "INTERIOR")
## Creacion de codigo para J3 y J4
C4_comunes$Codigo_C4_J3 <- paste(C4_comunes$Ean_Cod, C4_comunes$`GBA/INT`, C4_comunes$Mes, sep = "-")
C4_comunes$Codigo_C4_J4 <- paste(C4_comunes$Ean_Cod, C4_comunes$Mes, sep = "-")

### Jerarquia 1: Area Scentia 2
### Se buscan 8 observaciones como minimo dentro de cada Area_Scentia2 para cada Ean por mes (1)
### Luego, se calcula a nivel Ean-Tienda la variacion del precio por volumen
### Para cada Ean, se calcula un intervalo

## Detectar que registros van a ser evaluados en esta primer jerarquia
C4_J1 <- C4_comunes %>%
  group_by(Ean_Cod, Area_Scentia2, Mes) %>%
  summarise(n_area_2 = n()) %>%
  filter(n_area_2 >= 8)

C4_J1$Codigo_C4_J1 <- paste(C4_J1$Ean_Cod, C4_J1$Area_Scentia2, C4_J1$Mes, sep = "-")
C4_comunes$Codigo_C4_J1 <- paste(C4_comunes$Ean_Cod, C4_comunes$Area_Scentia2, C4_comunes$Mes, sep = "-")

## Los registros encontrados en esta primer jerarquia los indicaremos con el valor 1 en la variable C4_J1
C4_comunes$C4_J1 <- ifelse(C4_comunes$Codigo_C4_J1 %in% C4_J1$Codigo_C4_J1, 1, NA)
## Vamos a trabajar con los registros que marcamos con 1
C4_J1_datos <- subset(C4_comunes, C4_comunes$C4_J1 == 1)

## Calculo de la variacion
Var_C4_J1 <- C4_J1_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen", "Area_Scentia2")) %>%
  group_by(Ean_Cod, Neen) %>%
  summarise(Area_Scentia2, Var = (`Mes Act` - `Mes Ant`)/ `Mes Ant`)

Int_C4_J1 <- Var_C4_J1 %>%
  group_by(Ean_Cod, Area_Scentia2) %>%
  summarise(medcouple = ifelse(calcular_medcouple(Var) == FALSE, 0, mc(Var)),
            Lim_Inf_C4 = max(ifelse(medcouple < 0, quantile(Var, 0.25) - (1.5 * exp(-3*medcouple) * IQR(Var)),
                                    quantile(Var, 0.25) - (1.5 * exp(-4*medcouple) * IQR(Var))), 
                             quantile(Var, 0.25) - (1.5 * IQR(Var))),
            Lim_Sup_C4 = min(ifelse(medcouple < 0, quantile(Var, 0.75) + (1.5 * exp(4*medcouple) * IQR(Var)),
                                    quantile(Var, 0.75) + (1.5 * exp(3*medcouple) * IQR(Var))),
                             quantile(Var, 0.75) + (1.5*IQR(Var))))

## Unimos la informacion
Var_C4_J1 <- merge(Var_C4_J1, Int_C4_J1, by = c("Ean_Cod", "Area_Scentia2"), all.x = TRUE)
C4_J1_datos <- merge(C4_J1_datos, Var_C4_J1, by = c("Ean_Cod", "Neen"), all.x = TRUE)

## Detectamos los Ean que quedan fuera del intervalo de variacion 
condicion_mes <- C4_J1_datos$Mes == "Mes Act"
C4_J1_datos$eval_C4_J1 <- ifelse(condicion_mes & (C4_J1_datos$Var >= C4_J1_datos$Lim_Inf_C4 & C4_J1_datos$Var <= C4_J1_datos$Lim_Sup_C4), 
                                 TRUE, FALSE)
C4_J1_datos$eval_C4_J1[!condicion_mes] <- TRUE

## A los registros detectados como FALSE en eval_C4_J1 se le deben modificar las unidades 
## de forma de corregir la variacion
aux <- C4_J1_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen"))
aux1 <- subset(C4_J1_datos, C4_J1_datos$eval_C4_J1 == FALSE)
aux2 <- merge(aux1, aux, by = c("Ean_Cod", "Neen"))
## El volumen proyectado sera calculado como: 
## FACT_PROY / (LI + 1)*Pr_Vol_Ant - si la variacion es menor al limite inferior
## FACT_PROY / (LS + 1)*Pr_Vol_Ant - si la variacion es mayor al limite superior
aux2  <- mutate(aux2, "VOL_PROY3" = case_when(aux2$Var < aux2$Lim_Inf_C4 ~ aux2$FACT_PROY / ((aux2$Lim_Inf_C4 + 1) * aux2$`Mes Ant`),
                                              TRUE ~ aux2$FACT_PROY / ((aux2$Lim_Sup_C4 + 1) * aux2$`Mes Ant`)))
aux2$PrVol_PROY3 <- aux2$FACT_PROY / aux2$VOL_PROY3
aux2  <- mutate(aux2, "UNI_PROY3" = case_when(UnidadMedida == "IN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PAÑOS" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PC" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UNIDADES" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              TRUE ~ (VOL_PROY3*1000) / (Contenido*PesoVolumen)))
## Creamos una variable FLAG que indique si las unidades se ajustan en +-30% para corregir la variacion
aux2$Var_UNI_C4 <- aux2$UNI_PROY3 / aux2$UNI_PROY2 - 1
aux2$FLAG_C4[aux2$Var_UNI_C4 > 0.3] <- "MAYOR 30"
aux2$FLAG_C4[aux2$Var_UNI_C4 < -0.3] <- "MENOR 30"

## Unimos la informacion trabajada en la Jerarquia 1 del control 4:
C4_J1_datos <- subset(C4_J1_datos, C4_J1_datos$eval_C4_J1 == TRUE)
C4_J1_datos$VOL_PROY3 <- C4_J1_datos$VOL_PROY2
C4_J1_datos$UNI_PROY3 <- C4_J1_datos$UNI_PROY2
C4_J1_datos$PrVol_PROY3 <- C4_J1_datos$PrVol_PROY2
C4_J1_datos$Var_UNI_C4 <- NA
C4_J1_datos$FLAG_C4 <- NA
aux2 <- select(aux2, -`Mes Ant`, -`Mes Act`)
C4_J1_datos <- rbind(C4_J1_datos, aux2) ## Datos analizados en el C4 - J1

rm(Int_C4_J1, aux, aux1, aux2, condicion_mes)

### Jerarquia 2: Area Scentia
### Para los eans que no entraron en la Jerarquia 1
### Se buscan 8 observaciones como minimo dentro de cada Area_Scentia para cada Ean por mes (1)
### Luego, se calcula a nivel Ean-Tienda la variacion del precio por volumen
### Para cada Ean, se calcula el Q1, Q3 y el IQR

## Creacion de codigo
C4_comunes$Codigo_C4_J2 <- paste(C4_comunes$Ean_Cod, C4_comunes$Area_Scentia, C4_comunes$Mes, sep = "-")
## Registros que no entraron en J1
C4_comunes_J2 <- subset(C4_comunes, is.na(C4_comunes$C4_J1) == TRUE)

## Detectar que registros van a ser evaluados en la segunda jerarquia
C4_J2 <- C4_comunes_J2 %>%
  group_by(Ean_Cod, Area_Scentia, Mes) %>%
  summarise(n_area = n()) %>%
  filter(n_area >= 8)

C4_J2$Codigo_C4_J2 <- paste(C4_J2$Ean_Cod, C4_J2$Area_Scentia, C4_J2$Mes, sep = "-")

## Los registros encontrados en la segunda jerarquia los indicaremos con el valor 1 en la variable C4_J2
C4_comunes_J2$C4_J2 <- ifelse(C4_comunes_J2$Codigo_C4_J2 %in% C4_J2$Codigo_C4_J2, 1, NA)
## Vamos a trabajar con los registros que marcamos con 1
C4_J2_datos <- subset(C4_comunes_J2, C4_comunes_J2$C4_J2 == 1)

## Calculo de la variacion
Var_C4_J2 <- C4_J2_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen", "Area_Scentia")) %>%
  group_by(Ean_Cod, Neen) %>%
  summarise(Area_Scentia, Var = (`Mes Act` - `Mes Ant`)/ `Mes Ant`)

Int_C4_J2 <- Var_C4_J2 %>%
  group_by(Ean_Cod, Area_Scentia) %>%
  summarise(medcouple = ifelse(calcular_medcouple(Var) == FALSE, 0, mc(Var)),
            Lim_Inf_C4 = max(ifelse(medcouple < 0, quantile(Var, 0.25) - (1.5 * exp(-3*medcouple) * IQR(Var)),
                                    quantile(Var, 0.25) - (1.5 * exp(-4*medcouple) * IQR(Var))), 
                             quantile(Var, 0.25) - (1.5 * IQR(Var))),
            Lim_Sup_C4 = min(ifelse(medcouple < 0, quantile(Var, 0.75) + (1.5 * exp(4*medcouple) * IQR(Var)),
                                    quantile(Var, 0.75) + (1.5 * exp(3*medcouple) * IQR(Var))),
                             quantile(Var, 0.75) + (1.5*IQR(Var))))

## Unimos la informacion
Var_C4_J2 <- merge(Var_C4_J2, Int_C4_J2, by = c("Ean_Cod", "Area_Scentia"), all.x = TRUE)
C4_J2_datos <- merge(C4_J2_datos, Var_C4_J2, by = c("Ean_Cod", "Neen"), all.x = TRUE)

## Detectamos los Ean que quedan fuera del intervalo de variacion
condicion_mes <- C4_J2_datos$Mes == "Mes Act"
C4_J2_datos$eval_C4_J2 <- ifelse(condicion_mes & (C4_J2_datos$Var >= C4_J2_datos$Lim_Inf_C4 & C4_J2_datos$Var <= C4_J2_datos$Lim_Sup_C4), 
                                 TRUE, FALSE)
C4_J2_datos$eval_C4_J2[!condicion_mes] <- TRUE

## A los registros detectados como FALSE en eval_C4_J2 se le deben modificar las unidades 
## de forma de corregir la variacion
aux <- C4_J2_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen"))
aux1 <- subset(C4_J2_datos, C4_J2_datos$eval_C4_J2 == FALSE)
aux2 <- merge(aux1, aux, by = c("Ean_Cod", "Neen"))
## El volumen proyectado sera calculado como: 
## FACT_PROY / (LI + 1)*Pr_Vol_Ant - si la variacion es menor al limite inferior
## FACT_PROY / (LS + 1)*Pr_Vol_Ant - si la variacion es mayor al limite superior
aux2  <- mutate(aux2, "VOL_PROY3" = case_when(aux2$Var < aux2$Lim_Inf_C4 ~ aux2$FACT_PROY / ((aux2$Lim_Inf_C4 + 1) * aux2$`Mes Ant`),
                                              TRUE ~ aux2$FACT_PROY / ((aux2$Lim_Sup_C4 + 1) * aux2$`Mes Ant`)))
aux2$PrVol_PROY3 <- aux2$FACT_PROY / aux2$VOL_PROY3
aux2  <- mutate(aux2, "UNI_PROY3" = case_when(UnidadMedida == "IN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PAÑOS" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PC" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UNIDADES" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              TRUE ~ (VOL_PROY3*1000) / (Contenido*PesoVolumen)))
## Creamos una variable FLAG que indique si las unidades se ajustan en +-30% para corregir la variacion
aux2$Var_UNI_C4 <- aux2$UNI_PROY3 / aux2$UNI_PROY2 - 1
aux2$FLAG_C4[aux2$Var_UNI_C4 > 0.3] <- "MAYOR 30"
aux2$FLAG_C4[aux2$Var_UNI_C4 < -0.3] <- "MENOR 30"

## Unimos la informaciOn trabajada en la Jerarquia 2 del control 4:
C4_J2_datos <- subset(C4_J2_datos, C4_J2_datos$eval_C4_J2 == TRUE)
C4_J2_datos$VOL_PROY3 <- C4_J2_datos$VOL_PROY2
C4_J2_datos$UNI_PROY3 <- C4_J2_datos$UNI_PROY2
C4_J2_datos$PrVol_PROY3 <- C4_J2_datos$PrVol_PROY2
C4_J2_datos$Var_UNI_C4 <- NA
C4_J2_datos$FLAG_C4 <- NA
aux2 <- select(aux2, -`Mes Ant`, -`Mes Act`)
C4_J2_datos <- rbind(C4_J2_datos, aux2) ## Datos analizados en el C4 - J2

rm(Int_C4_J2, aux, aux1, aux2, condicion_mes)

### Jerarquia 3: GBA / INTERIOR
### Para los eans que no entraron en la Jerarquia 2
### Se buscan 8 observaciones como minimo dentro de GBA o INTERIOR para cada Ean por mes (1)
### Luego, se calcula a nivel Ean-Tienda la variacion del precio por volumen
### Para cada Ean, se calcula el Q1, Q3 y el IQR

## Registros que no entraron en J1 ni J2
C4_comunes_J3 <- subset(C4_comunes_J2, is.na(C4_comunes_J2$C4_J2) == TRUE)

## Detectar que registros van a ser evaluados en la tercer jerarquia
C4_J3 <- C4_comunes_J3 %>%
  group_by(Ean_Cod, `GBA/INT`, Mes) %>%
  summarise(n_area = n()) %>%
  filter(n_area >= 8)

C4_J3$Codigo_C4_J3 <- paste(C4_J3$Ean_Cod, C4_J3$`GBA/INT`, C4_J3$Mes, sep = "-")

## Los registros encontrados en la tercer jerarquia los indicaremos con el valor 1 en la variable C4_J3
C4_comunes_J3$C4_J3 <- ifelse(C4_comunes_J3$Codigo_C4_J3 %in% C4_J3$Codigo_C4_J3, 1, NA)
## Vamos a trabajar con los registros que marcamos con 1
C4_J3_datos <- subset(C4_comunes_J3, C4_comunes_J3$C4_J3 == 1)

## Calculo de la variacion
Var_C4_J3 <- C4_J3_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen", "GBA/INT")) %>%
  group_by(Ean_Cod, Neen) %>%
  summarise(`GBA/INT`, Var = (`Mes Act` - `Mes Ant`)/ `Mes Ant`)

Int_C4_J3 <- Var_C4_J3 %>%
  group_by(Ean_Cod, `GBA/INT`) %>%
  summarise(medcouple = ifelse(calcular_medcouple(Var) == FALSE, 0, mc(Var)),
            Lim_Inf_C4 = max(ifelse(medcouple < 0, quantile(Var, 0.25) - (1.5 * exp(-3*medcouple) * IQR(Var)),
                                    quantile(Var, 0.25) - (1.5 * exp(-4*medcouple) * IQR(Var))), 
                             quantile(Var, 0.25) - (1.5 * IQR(Var))),
            Lim_Sup_C4 = min(ifelse(medcouple < 0, quantile(Var, 0.75) + (1.5 * exp(4*medcouple) * IQR(Var)),
                                    quantile(Var, 0.75) + (1.5 * exp(3*medcouple) * IQR(Var))),
                             quantile(Var, 0.75) + (1.5*IQR(Var))))

## Unimos la informacion
Var_C4_J3 <- merge(Var_C4_J3, Int_C4_J3, by = c("Ean_Cod", "GBA/INT"), all.x = TRUE)
C4_J3_datos <- merge(C4_J3_datos, Var_C4_J3, by = c("Ean_Cod", "Neen"), all.x = TRUE)

## Detectamos los Ean que quedan fuera del intervalo de variacion 
condicion_mes <- C4_J3_datos$Mes == "Mes Act"
C4_J3_datos$eval_C4_J3 <- ifelse(condicion_mes & (C4_J3_datos$Var >= C4_J3_datos$Lim_Inf_C4 & C4_J3_datos$Var <= C4_J3_datos$Lim_Sup_C4), 
                                 TRUE, FALSE)
C4_J3_datos$eval_C4_J3[!condicion_mes] <- TRUE

## A los registros detectados como FALSE en eval_C4_J3 se le deben modificar las unidades 
## de forma de corregir la variacion
aux <- C4_J3_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen"))
aux1 <- subset(C4_J3_datos, C4_J3_datos$eval_C4_J3 == FALSE)
aux2 <- merge(aux1, aux, by = c("Ean_Cod", "Neen"))
## El volumen proyectado sera calculado como: 
## FACT_PROY / (LI + 1)*Pr_Vol_Ant - si la variacion es menor al limite inferior
## FACT_PROY / (LS + 1)*Pr_Vol_Ant - si la variacion es mayor al limite superior
aux2  <- mutate(aux2, "VOL_PROY3" = case_when(aux2$Var < aux2$Lim_Inf_C4 ~ aux2$FACT_PROY / ((aux2$Lim_Inf_C4 + 1) * aux2$`Mes Ant`),
                                              TRUE ~ aux2$FACT_PROY / ((aux2$Lim_Sup_C4 + 1) * aux2$`Mes Ant`)))
aux2$PrVol_PROY3 <- aux2$FACT_PROY / aux2$VOL_PROY3
aux2  <- mutate(aux2, "UNI_PROY3" = case_when(UnidadMedida == "IN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PAÑOS" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PC" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UNIDADES" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              TRUE ~ (VOL_PROY3*1000) / (Contenido*PesoVolumen)))
## Creamos una variable FLAG que indique si las unidades se ajustan en +-30% para corregir la variacion
aux2$Var_UNI_C4 <- aux2$UNI_PROY3 / aux2$UNI_PROY2 - 1
aux2$FLAG_C4[aux2$Var_UNI_C4 > 0.3] <- "MAYOR 30"
aux2$FLAG_C4[aux2$Var_UNI_C4 < -0.3] <- "MENOR 30"

## Unimos la informacion trabajada en la Jerarquia 3 del control 4:
C4_J3_datos <- subset(C4_J3_datos, C4_J3_datos$eval_C4_J3 == TRUE)
C4_J3_datos$VOL_PROY3 <- C4_J3_datos$VOL_PROY2
C4_J3_datos$UNI_PROY3 <- C4_J3_datos$UNI_PROY2
C4_J3_datos$PrVol_PROY3 <- C4_J3_datos$PrVol_PROY2
C4_J3_datos$Var_UNI_C4 <- NA
C4_J3_datos$FLAG_C4 <- NA
aux2 <- select(aux2, -`Mes Ant`, -`Mes Act`)
C4_J3_datos <- rbind(C4_J3_datos, aux2) ## Datos analizados en el C4 - J3

rm(Int_C4_J3, aux, aux1, aux2, condicion_mes)

### Jerarquia 4: Total Pais
### Para los eans que no entraron en la Jerarquia 3
### Se buscan 8 observaciones como minimo a nivel Total Pais para cada Ean por mes (1)
### Luego, se calcula a nivel Ean-Tienda la variacion del precio por volumen
### Para cada Ean, se calcula el Q1, Q3 y el IQR

## Registros que no entraron en J1, J2 ni J3
C4_comunes_J4 <- subset(C4_comunes_J3, is.na(C4_comunes_J3$C4_J3) == TRUE)

## Detectar que registros van a ser evaluados en la cuarta jerarquia
C4_J4 <- C4_comunes_J4 %>%
  group_by(Ean_Cod, Mes) %>%
  summarise(n_total = n()) %>%
  filter(n_total >= 8)

C4_J4$Codigo_C4_J4 <- paste(C4_J4$Ean_Cod, C4_J4$Mes, sep = "-")

## Los registros encontrados en la cuarta jerarquia los indicaremos con el valor 1 en la variable C4_J4
C4_comunes_J4$C4_J4 <- ifelse(C4_comunes_J4$Codigo_C4_J4 %in% C4_J4$Codigo_C4_J4, 1, NA)
## Vamos a trabajar con los registros que marcamos con 1
C4_J4_datos <- subset(C4_comunes_J4, C4_comunes_J4$C4_J4 == 1)

## Calculo de la variacion
Var_C4_J4 <- C4_J4_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen")) %>%
  group_by(Ean_Cod, Neen) %>%
  summarise(Var = (`Mes Act` - `Mes Ant`)/ `Mes Ant`)

Int_C4_J4 <- Var_C4_J4 %>%
  group_by(Ean_Cod) %>%
  summarise(medcouple = ifelse(calcular_medcouple(Var) == FALSE, 0, mc(Var)),
            Lim_Inf_C4 = max(ifelse(medcouple < 0, quantile(Var, 0.25) - (1.5 * exp(-3*medcouple) * IQR(Var)),
                                    quantile(Var, 0.25) - (1.5 * exp(-4*medcouple) * IQR(Var))), 
                             quantile(Var, 0.25) - (1.5 * IQR(Var))),
            Lim_Sup_C4 = min(ifelse(medcouple < 0, quantile(Var, 0.75) + (1.5 * exp(4*medcouple) * IQR(Var)),
                                    quantile(Var, 0.75) + (1.5 * exp(3*medcouple) * IQR(Var))),
                             quantile(Var, 0.75) + (1.5*IQR(Var))))

## Unimos la informacion
Var_C4_J4 <- merge(Var_C4_J4, Int_C4_J4, by = c("Ean_Cod"), all.x = TRUE)
C4_J4_datos <- merge(C4_J4_datos, Var_C4_J4, by = c("Ean_Cod", "Neen"), all.x = TRUE)

## Detectamos los Ean que quedan fuera del intervalo de variacion 
condicion_mes <- C4_J4_datos$Mes == "Mes Act"
C4_J4_datos$eval_C4_J4 <- ifelse(condicion_mes & (C4_J4_datos$Var >= C4_J4_datos$Lim_Inf_C4 & C4_J4_datos$Var <= C4_J4_datos$Lim_Sup_C4), 
                                 TRUE, FALSE)
C4_J4_datos$eval_C4_J4[!condicion_mes] <- TRUE

## A los registros detectados como FALSE en eval_C4_J4 se le deben modificar las unidades 
## de forma de corregir la variacion
aux <- C4_J4_datos %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen"))
aux1 <- subset(C4_J4_datos, C4_J4_datos$eval_C4_J4 == FALSE)
aux2 <- merge(aux1, aux, by = c("Ean_Cod", "Neen"))
## El volumen proyectado sera calculado como: 
## FACT_PROY / (LI + 1)*Pr_Vol_Ant - si la variacion es menor al limite inferior
## FACT_PROY / (LS + 1)*Pr_Vol_Ant - si la variacion es mayor al limite superior
aux2  <- mutate(aux2, "VOL_PROY3" = case_when(aux2$Var < aux2$Lim_Inf_C4 ~ aux2$FACT_PROY / ((aux2$Lim_Inf_C4 + 1) * aux2$`Mes Ant`),
                                              TRUE ~ aux2$FACT_PROY / ((aux2$Lim_Sup_C4 + 1) * aux2$`Mes Ant`)))
aux2$PrVol_PROY3 <- aux2$FACT_PROY / aux2$VOL_PROY3
aux2  <- mutate(aux2, "UNI_PROY3" = case_when(UnidadMedida == "IN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PAÑOS" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PC" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UNIDADES" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              TRUE ~ (VOL_PROY3*1000) / (Contenido*PesoVolumen)))
## Creamos una variable FLAG que indique si las unidades se ajustan en +-30% para corregir la variacion
aux2$Var_UNI_C4 <- aux2$UNI_PROY3 / aux2$UNI_PROY2 - 1
aux2$FLAG_C4[aux2$Var_UNI_C4 > 0.3] <- "MAYOR 30"
aux2$FLAG_C4[aux2$Var_UNI_C4 < -0.3] <- "MENOR 30"

## Unimos la informacion trabajada en la Jerarquia 4 del control 4:
C4_J4_datos <- subset(C4_J4_datos, C4_J4_datos$eval_C4_J4 == TRUE)
C4_J4_datos$VOL_PROY3 <- C4_J4_datos$VOL_PROY2
C4_J4_datos$UNI_PROY3 <- C4_J4_datos$UNI_PROY2
C4_J4_datos$PrVol_PROY3 <- C4_J4_datos$PrVol_PROY2
C4_J4_datos$Var_UNI_C4 <- NA
C4_J4_datos$FLAG_C4 <- NA
aux2 <- select(aux2, -`Mes Ant`, -`Mes Act`)
C4_J4_datos <- rbind(C4_J4_datos, aux2) ## Datos analizados en el C4 - J3

rm(Int_C4_J4, aux, aux1, aux2, condicion_mes)

### Jerarquia 5: Eans que no entraron en las jerarquias anteriores
### Se calcula a nivel Ean-Tienda la variacion del precio por volumen
### Para cada Ean, se calcula un intervalo
### Se corrigen los valores que sean necesarios, pero dejando una marca

## Registros que no entraron en J1, J2, J3 ni J4
C4_comunes_J5 <- subset(C4_comunes_J4, is.na(C4_comunes_J4$C4_J4) == TRUE)

## Los registros encontrados en la quinta jerarquia los indicaremos con el valor 1 en la variable C4_J4
C4_comunes_J5$C4_J5 <- 1

## Calculo de la variacion
Var_C4_J5 <- C4_comunes_J5 %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen")) %>%
  group_by(Ean_Cod, Neen) %>%
  summarise(Var = (`Mes Act` - `Mes Ant`)/ `Mes Ant`)

Int_C4_J5 <- Var_C4_J5 %>%
  group_by(Ean_Cod) %>%
  summarise(medcouple = ifelse(calcular_medcouple(Var) == FALSE, 0, mc(Var)),
            Lim_Inf_C4 = max(ifelse(medcouple < 0, quantile(Var, 0.25) - (1.5 * exp(-3*medcouple) * IQR(Var)),
                                    quantile(Var, 0.25) - (1.5 * exp(-4*medcouple) * IQR(Var))), 
                             quantile(Var, 0.25) - (1.5 * IQR(Var))),
            Lim_Sup_C4 = min(ifelse(medcouple < 0, quantile(Var, 0.75) + (1.5 * exp(4*medcouple) * IQR(Var)),
                                    quantile(Var, 0.75) + (1.5 * exp(3*medcouple) * IQR(Var))),
                             quantile(Var, 0.75) + (1.5*IQR(Var))))

## Unimos la informacion
Var_C4_J5 <- merge(Var_C4_J5, Int_C4_J5, by = c("Ean_Cod"), all.x = TRUE)
C4_comunes_J5 <- merge(C4_comunes_J5, Var_C4_J5, by = c("Ean_Cod", "Neen"), all.x = TRUE)

## Detectamos los Ean que quedan fuera del intervalo de variacion 
condicion_mes <- C4_comunes_J5$Mes == "Mes Act"
C4_comunes_J5$eval_C4_J5 <- ifelse(condicion_mes & (C4_comunes_J5$Var >= C4_comunes_J5$Lim_Inf_C4 & C4_comunes_J5$Var <= C4_comunes_J5$Lim_Sup_C4), 
                                   TRUE, FALSE)
C4_comunes_J5$eval_C4_J5[!condicion_mes] <- TRUE

## A los registros detectados como FALSE en eval_C4_J5 se le deben modificar las unidades 
## de forma de corregir la variacion
aux <- C4_comunes_J5 %>%
  pivot_wider(names_from = Mes, values_from = PrVol_PROY2, 
              id_cols = c("Ean_Cod","Neen"))
aux1 <- subset(C4_comunes_J5, C4_comunes_J5$eval_C4_J5 == FALSE)
aux2 <- merge(aux1, aux, by = c("Ean_Cod", "Neen"))
## El volumen proyectado sera calculado como: 
## FACT_PROY / (Q1 - 1.5*RI + 1)*Pr_Vol_Ant - si la variacion es menor al limite inferior
## FACT_PROY / (Q3 + 1.5*RI + 1)*Pr_Vol_Ant - si la variacion es mayor al limite superior
aux2  <- mutate(aux2, "VOL_PROY3" = case_when(aux2$Var < aux2$Lim_Inf_C4 ~ aux2$FACT_PROY / ((aux2$Lim_Inf_C4 + 1) * aux2$`Mes Ant`),
                                              TRUE ~ aux2$FACT_PROY / ((aux2$Lim_Sup_C4 + 1) * aux2$`Mes Ant`)))
aux2$PrVol_PROY3 <- aux2$FACT_PROY / aux2$VOL_PROY3
aux2  <- mutate(aux2, "UNI_PROY3" = case_when(UnidadMedida == "IN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PAÑOS" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "PC" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              UnidadMedida == "UNIDADES" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                              TRUE ~ (VOL_PROY3*1000) / (Contenido*PesoVolumen)))
## Creamos una variable FLAG que indique si las unidades se ajustan en +-30% para corregir la variacion
aux2$Var_UNI_C4 <- aux2$UNI_PROY3 / aux2$UNI_PROY2 - 1
aux2$FLAG_C4[aux2$Var_UNI_C4 > 0.3] <- "MAYOR 30"
aux2$FLAG_C4[aux2$Var_UNI_C4 < -0.3] <- "MENOR 30"

## Unimos la informacion trabajada en la Jerarquia 5 del control 4:
C4_comunes_J5 <- subset(C4_comunes_J5, C4_comunes_J5$eval_C4_J5 == TRUE)
C4_comunes_J5$VOL_PROY3 <- C4_comunes_J5$VOL_PROY2
C4_comunes_J5$UNI_PROY3 <- C4_comunes_J5$UNI_PROY2
C4_comunes_J5$PrVol_PROY3 <- C4_comunes_J5$PrVol_PROY2
C4_comunes_J5$Var_UNI_C4 <- NA
C4_comunes_J5$FLAG_C4 <- NA
aux2 <- select(aux2, -`Mes Ant`, -`Mes Act`)
C4_comunes_J5 <- rbind(C4_comunes_J5, aux2) ## Datos analizados en el C4 - J3

rm(Int_C4_J5, aux, aux1, aux2, condicion_mes)

### Juntar la data analizada en el control 4 (J1, J2, J3, J4, J5) que tiene informacion en comun
### en ambos meses
## Jerarquia 1
C4_J1_datos$C4 <- ifelse(C4_J1_datos$eval_C4_J1 == FALSE, "J1", 0)
C4_J1_datos <- select(C4_J1_datos, -Codigo_C4_J1, -Codigo_C4_J3, -Codigo_C4_J4, -Area_Scentia2.y, -eval_C4_J1)
C4_J1_datos <- rename(C4_J1_datos, Jerarquia = C4_J1, Area_Scentia2 = Area_Scentia2.x)
C4_J1_datos$C4_revisar <- NA

## Jerarquia 2
C4_J2_datos$C4 <- ifelse(C4_J2_datos$eval_C4_J2 == FALSE, "J2", 0)
C4_J2_datos <- select(C4_J2_datos, -Codigo_C4_J1, -Codigo_C4_J3, -Codigo_C4_J4, -Area_Scentia.y, -eval_C4_J2,
                      -Codigo_C4_J2, -C4_J1)
C4_J2_datos$C4_J2 <- 2
C4_J2_datos <- rename(C4_J2_datos, Jerarquia = C4_J2, Area_Scentia = Area_Scentia.x)
C4_J2_datos$C4_revisar <- NA

## Jerarquia 3
C4_J3_datos$C4 <- ifelse(C4_J3_datos$eval_C4_J3 == FALSE, "J3", 0)
C4_J3_datos <- select(C4_J3_datos, -Codigo_C4_J1, -Codigo_C4_J3, -Codigo_C4_J4, -`GBA/INT.y`, -eval_C4_J3,
                      -Codigo_C4_J2, -C4_J1, -C4_J2)
C4_J3_datos$C4_J3 <- 3
C4_J3_datos <- rename(C4_J3_datos, Jerarquia = C4_J3, `GBA/INT` = `GBA/INT.x`)
C4_J3_datos$C4_revisar <- NA

## Jerarquia 4
C4_J4_datos$C4 <- ifelse(C4_J4_datos$eval_C4_J4 == FALSE, "J4", 0)
C4_J4_datos <- select(C4_J4_datos, -Codigo_C4_J1, -Codigo_C4_J3, -Codigo_C4_J4, -`C4_J3`, -eval_C4_J4,
                      -Codigo_C4_J2, -C4_J1, -C4_J2)
C4_J4_datos$C4_J4 <- 4
C4_J4_datos <- rename(C4_J4_datos, Jerarquia = C4_J4)
C4_J4_datos$C4_revisar <- NA

## Jerarquia 5
C4_comunes_J5$C4 <- ifelse(C4_comunes_J5$eval_C4_J5 == FALSE, "J5", 0)
C4_comunes_J5 <- select(C4_comunes_J5, -Codigo_C4_J1, -Codigo_C4_J3, -Codigo_C4_J4, -`C4_J3`, -eval_C4_J5,
                        -Codigo_C4_J2, -C4_J1, -C4_J2, -C4_J4)
C4_comunes_J5$C4_J5 <- 5
C4_comunes_J5 <- rename(C4_comunes_J5, Jerarquia = C4_J5)
C4_comunes_J5$C4_revisar <- ifelse(C4_comunes_J5$C4 == "J5", "REVISAR", NA)

C4_comunes_post <- rbind(C4_J1_datos, C4_J2_datos, C4_J3_datos, C4_J4_datos, C4_comunes_J5)

rm(C4_comunes, C4_comunes_J2, C4_comunes_J3, C4_comunes_J4, C4_comunes_J5, C4_J1, C4_J1_datos,
   C4_J2, C4_J2_datos, C4_J3, C4_J3_datos, C4_J4, C4_J4_datos, Var_C4_J1, Var_C4_J2,
   Var_C4_J3, Var_C4_J4, Var_C4_J5)

### PARTE 2
### Buscamos los Neen-EAN que no tienen presencia en los dos meses, es decir, que no tenemos
### precios para comparar del mes anterior y el actual
C4_comunes_post <- C4_comunes_post[, c("Ean_Cod", "Neen", "Unidades", "Facturacion", "Volumen", "PrVol", "Celda", "Desc_Celda",
                                       "Area_Scentia", "Area_Scentia2", "FactorAJ", "EAN_DESC", "Categoria",
                                       "Mes", "UNI_PROY", "FACT_PROY", "VOL_PROY", "PrVol_PROY", "Manuf", 
                                       "Brand", "PesoVolumen", "Contenido", "UnidadMedida", "Codigo", "C1", "C2",
                                       "Lim_Sup_C3", "Lim_Inf_C3","C3", "Unidades2", "Volumen2", "UNI_PROY2", "VOL_PROY2",
                                       "PrVol_PROY2", "Codigo_Neen_Ean", "GBA/INT", "Jerarquia", "Var",
                                       "Lim_Inf_C4", "Lim_Sup_C4", "VOL_PROY3", "UNI_PROY3", "PrVol_PROY3",
                                       "Var_UNI_C4", "FLAG_C4", "C4", "C4_revisar")]
datos_template_C3 <- datos_template_C3[, c("Ean_Cod", "Neen", "Unidades", "Facturacion", "Volumen", "PrVol", "Celda", "Desc_Celda",
                                           "Area_Scentia", "Area_Scentia2", "FactorAJ", "EAN_DESC", "Categoria",
                                           "Mes", "UNI_PROY", "FACT_PROY", "VOL_PROY", "PrVol_PROY", "Manuf", 
                                           "Brand", "PesoVolumen", "Contenido", "UnidadMedida", "Codigo", "C1", "C2",
                                           "Lim_Sup_C3", "Lim_Inf_C3","C3", "Unidades2", "Volumen2", "UNI_PROY2", "VOL_PROY2",
                                           "PrVol_PROY2", "Codigo_Neen_Ean")]
## Seleccionamos de datos_template_c3 lo que no entro en las jerarquias anteriores
codigos <- C4_comunes_post$Codigo_Neen_Ean
datos_template_C3$C4_aux <- ifelse(datos_template_C3$Codigo_Neen_Ean %in% codigos, 0, 1)
C4_no_comunes <- subset(datos_template_C3, datos_template_C3$C4_aux == 1)
## Creamos un nuevo datos_template con las variables que se agregaron en las jerarquias anteriores
C4_comunes_post$C4_aux <- 0
datos_template_C4 <- bind_rows(C4_comunes_post, C4_no_comunes)
rm(datos_template_C3)
## Los neen-ean que no tienen informacion en ambos meses tienen las columnas de valores del C5 vacias
## agregamos el valor del precio que vienen manteniendo para poder calcular la mediana
datos_template_C4$PrVol_PROY3[is.na(datos_template_C4$PrVol_PROY3) == TRUE] <- datos_template_C4$PrVol_PROY2[is.na(datos_template_C4$PrVol_PROY3) == TRUE]

### Para estos productos se busca un precio a nivel nacional para usar como base de comparacion
### Este precio nacional sera la mediana del precio por volumen del ean en cada tienda para 
### el mes actual
Pr_mediano_nac <- datos_template_C4 %>%
  filter(Mes == "Mes Act") %>%
  group_by(Ean_Cod) %>%
  summarise(Pr_Mediano_C4 = median(PrVol_PROY3))

C4_no_comunes <- merge(C4_no_comunes, Pr_mediano_nac, by = "Ean_Cod", all.x = TRUE)

### Creacion de los limites que se permite variar al precio (no mas de un 150%, y no menos que un 70%)
C4_no_comunes$Lim_Inf_C4 <- C4_no_comunes$Pr_Mediano_C4 * 0.7
C4_no_comunes$Lim_Sup_C4 <- C4_no_comunes$Pr_Mediano_C4 * 1.5

### Los registros que queden fuera de esos limites tomaran el valor J6 en la variable C5
condicion_mes <- C4_no_comunes$Mes == "Mes Act"
C4_no_comunes$C4 <- ifelse(condicion_mes & (C4_no_comunes$PrVol_PROY2 >= C4_no_comunes$Lim_Inf_C4 & C4_no_comunes$PrVol_PROY2 <= C4_no_comunes$Lim_Sup_C4), 
                           0, "J6")
C4_no_comunes$C4[!condicion_mes] <- 0

### A estos valores se le debe corregir las unidades
C4_J6 <- subset(C4_no_comunes, C4_no_comunes$C4 == "J6")
C4_J6$VOL_PROY3 <- ifelse(C4_J6$PrVol_PROY2 < C4_J6$Lim_Inf_C4, C4_J6$FACT_PROY / C4_J6$Lim_Inf_C4,
                          C4_J6$FACT_PROY / C4_J6$Lim_Sup_C4)
C4_J6$PrVol_PROY3 <- C4_J6$FACT_PROY / C4_J6$VOL_PROY3
C4_J6  <- mutate(C4_J6, "UNI_PROY3" = case_when(UnidadMedida == "IN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                                UnidadMedida == "PAÑOS" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                                UnidadMedida == "PC" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                                UnidadMedida == "UN" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                                UnidadMedida == "UNIDADES" ~ VOL_PROY3 / (Contenido*PesoVolumen),
                                                TRUE ~ (VOL_PROY3*1000) / (Contenido*PesoVolumen)))
## Creamos una variable FLAG que indique si las unidades se ajustan en +-30% para corregir la variacion
C4_J6$Var_UNI_C4 <- C4_J6$UNI_PROY3 / C4_J6$UNI_PROY2 - 1
C4_J6$FLAG_C4[C4_J6$Var_UNI_C4 > 0.3] <- "MAYOR 30"
C4_J6$FLAG_C4[C4_J6$Var_UNI_C4 < -0.3] <- "MENOR 30"

## Unimos la informacion trabajada en la Jerarquia 6 del control 4:
C4_no_comunes <- subset(C4_no_comunes, C4_no_comunes$C4 == 0)
C4_no_comunes$VOL_PROY3 <- C4_no_comunes$VOL_PROY2
C4_no_comunes$UNI_PROY3 <- C4_no_comunes$UNI_PROY2
C4_no_comunes$PrVol_PROY3 <- C4_no_comunes$PrVol_PROY2
C4_no_comunes$Var_UNI_C4 <- NA
C4_no_comunes$FLAG_C4 <- NA
C4_no_comunes <- rbind(C4_no_comunes, C4_J6) ## Datos analizados en el C4 - J6

rm(codigos, C4_J6, condicion_mes, Pr_mediano_nac)

## Unimos toda la informacion 
datos_template_C4 <- subset(datos_template_C4, datos_template_C4$C4_aux == 0)
C4_no_comunes$`GBA/INT`  <- ifelse(C4_no_comunes$Area_Scentia == "GBA", "GBA", "INTERIOR")
C4_no_comunes$Jerarquia <- 6
datos_template_C4 <- bind_rows(datos_template_C4, C4_no_comunes)
datos_template_C4 <- select(datos_template_C4, -C4_aux)

## Creamos una variable que indique si la diferencia entre las unidades proyectadas antes de este
## control y las unidades proyectadas luego del control representan mas de un 1% del total de unidades
## de la categoria-area a la que pertenecen
## Calculo de la diferencia
datos_template_C4$Dif_Uni <- NA
datos_template_C4$Dif_Uni[datos_template_C4$C4 != 0] <- abs(datos_template_C4$UNI_PROY3[datos_template_C4$C4 != 0] - datos_template_C4$UNI_PROY2[datos_template_C4$C4 != 0])
## Calculo del total por Categoria-Area
cat_area <- datos_template_C4 %>%
  group_by(Categoria, Area_Scentia) %>%
  summarise(suma_uni_proy = sum(UNI_PROY3))
datos_template_C4 <- merge(datos_template_C4, cat_area, by = c("Categoria", "Area_Scentia"))
## Calculo del porcentaje
datos_template_C4$Porcentaje_C4 <- NA
datos_template_C4$Porcentaje_C4[datos_template_C4$C4 != 0] <- datos_template_C4$Dif_Uni[datos_template_C4$C4 != 0]/datos_template_C4$suma_uni_proy[datos_template_C4$C4 != 0]
## Creacion de la variable
datos_template_C4$Porc_Mayor_Uno <- ifelse(datos_template_C4$Porcentaje_C4 > 0.01, "Afecta", "No Afecta")

## Creamos una variable qe indique si las unidades a total categoria - area_scentia2 se modifican
## en +1% o -1%
## Calculo del total por Categoria-Area
cat_area2 <- datos_template_C4 %>%
  group_by(Categoria, Area_Scentia2) %>%
  summarise(porc_cat_area = (sum(UNI_PROY3)/sum(UNI_PROY2) - 1)*100)
datos_template_C4 <- merge(datos_template_C4, cat_area2, by = c("Categoria", "Area_Scentia2"))
datos_template_C4$Flag_Uni_Cat_Area2 <- ifelse(datos_template_C4$porc_cat_area >= 1 | datos_template_C4$porc_cat_area <= -1, "Modifica +-1%", NA)

## Creamos una variable que indique si el precio del producto aumenta mas de un 100% o cae en menos de un 50%
## luego del control
datos_template_C4$Dif_Pr <- datos_template_C4$PrVol_PROY3 / datos_template_C4$PrVol_PROY2 - 1
datos_template_C4$Flag_Dif_Pr <- ifelse(datos_template_C4$Dif_Pr >= 1 | datos_template_C4$Dif_Pr <= -0.5, "Revisar Precio", NA)

datos_template_C4 <- datos_template_C4[, c("Ean_Cod", "Neen", "Unidades", "Facturacion", "Volumen", "PrVol", "Celda", "Desc_Celda",
                                           "Area_Scentia", "Area_Scentia2", "FactorAJ", "EAN_DESC", "Categoria",
                                           "Mes", "UNI_PROY", "FACT_PROY", "VOL_PROY", "PrVol_PROY", "Manuf", 
                                           "Brand", "PesoVolumen", "Contenido", "UnidadMedida", "Codigo", "C1", "C2",
                                           "Lim_Sup_C3", "Lim_Inf_C3","C3", "Unidades2", "Volumen2", "UNI_PROY2", "VOL_PROY2",
                                           "PrVol_PROY2", "Codigo_Neen_Ean", "GBA/INT", "Jerarquia", "Var",
                                           "Lim_Inf_C4", "Lim_Sup_C4", "VOL_PROY3", "UNI_PROY3", "PrVol_PROY3",
                                           "Var_UNI_C4", "FLAG_C4", "C4", "C4_revisar", "Porc_Mayor_Uno", "Flag_Dif_Pr",
                                           "Flag_Uni_Cat_Area2")]

## Calculamos Unidades y Volumen sin proyectar
datos_template_C4$Unidades3 <- datos_template_C4$UNI_PROY3 / datos_template_C4$FactorAJ
datos_template_C4$Volumen3 <- datos_template_C4$VOL_PROY3 / datos_template_C4$FactorAJ

## Agregamos a datos_template la variable C4 que identifica a los registros que son modificados en este control
aux <- datos_template_C4[,c("Codigo", "C4")]
datos_template <- merge(datos_template, aux, by = "Codigo", all = TRUE) 
rm(aux, C4_no_comunes, C4_comunes_post, cat_area, cat_area2)

#### ---- CONTROL 5: Control de volumen ----####  

## Este control se realiza para las tiendas que tienen informacion en los dos meses
## Seleccionamos las tiendas en comun
datos_template_C4$Codigo_Neen_Cat_Area <- paste(datos_template_C4$Neen, datos_template_C4$Categoria, datos_template_C4$Area_Scentia, sep = "-")
datos_template_C4$Precio_Uni <- datos_template_C4$Facturacion / datos_template_C4$Unidades3
C5_comunes <- datos_template_C4 %>%
  group_by(Codigo_Neen_Cat_Area) %>%
  filter(all(c("Mes Ant", "Mes Act") %in% Mes))

## Para cada combinacion de categoria-area se debe calcular la variacion promedio de unidades
## y el desvio
## RESTRICCION: debe haber mas de 30 tiendas en la categoria-area
## Contamos la cantidad de tiendas
tiendas <- C5_comunes %>%
  group_by(Categoria, Area_Scentia) %>%
  summarise(cantidad_tiendas = n_distinct(Neen))

C5_comunes <- merge(C5_comunes, tiendas, by = c("Categoria", "Area_Scentia"))
## Seleccionamos las combinaciones que tienen mas de 30 tiendas
C5_comunes <- subset(C5_comunes, C5_comunes$cantidad_tiendas > 30)

## Calculamos las variaciones de las tiendas
Var_C5 <- C5_comunes %>%
  group_by(Neen, Mes, Categoria, Area_Scentia) %>%
  summarise(suma_uni = sum(Unidades3)) %>%
  pivot_wider(names_from = Mes, values_from = suma_uni, 
              id_cols = c("Neen", "Categoria", "Area_Scentia")) %>%
  group_by(Neen, Categoria, Area_Scentia) %>%
  summarise(var_uni = (`Mes Act` - `Mes Ant`)/ `Mes Ant`)

### Agregamos las variaciones a la base
C5_comunes <- merge(C5_comunes, Var_C5, by = c("Neen", "Categoria", "Area_Scentia"))

### Detectamos los outliers segun categoría-area
### LI : max(LI medcouple, LI boxplot) 
### LS : min(LS medcouple, LS boxplot)
### Los limites del medcouple se calculan segun si esta medida es mayor o menor a 0
### Las variaciones mayores a 500% no seran tenidas en cuenta para calcular estos limites
Limites_C5 <- Var_C5 %>%
  filter(var_uni <= 5) %>%
  group_by(Categoria, Area_Scentia) %>%
  summarise(medcouple = mc(var_uni, c.huberize = Inf),
            LI_uni5 = max(ifelse(medcouple < 0, quantile(var_uni, 0.25) - (1.5 * exp(-3*medcouple) * IQR(var_uni)),
                                 quantile(var_uni, 0.25) - (1.5 * exp(-4*medcouple) * IQR(var_uni))), 
                          quantile(var_uni, 0.25) - (1.5 * IQR(var_uni))),
            LS_uni5 = min(ifelse(medcouple < 0, quantile(var_uni, 0.75) + (1.5 * exp(4*medcouple) * IQR(var_uni)),
                                 quantile(var_uni, 0.75) + (1.5 * exp(3*medcouple) * IQR(var_uni))),
                          quantile(var_uni, 0.75) + (1.5*IQR(var_uni))))

C5_comunes <- merge(C5_comunes, Limites_C5, by = c("Categoria", "Area_Scentia"))
### Creamos la variable que distingue aquellos registros que caen fuera o dentro del intervalo
### Si Eval_C5 = 1 -> dentro del intervalo
### Si Eval_C5 = 0 -> fuera del intervalo
C5_comunes$Eval_C5 <- ifelse(C5_comunes$var_uni >= C5_comunes$LI_uni5 & C5_comunes$var_uni <= C5_comunes$LS_uni5,1,0)

## Seleccionamos los registros que caen por fuera del intervalo
C5_detectados <- subset(C5_comunes, C5_comunes$Eval_C5 == 0)
## Identificamos: neg -> si la variacion es menor al LI
################: pos -> si la variacion es mayor al LS
C5_detectados$Signo <- ifelse(C5_detectados$var_uni <= C5_detectados$LI_uni5,"neg","pos")

### Trabajamos a los clasificados como "neg"
C5_neg <- subset(C5_detectados, C5_detectados$Signo == "neg")
## Calculamos la cantidad de unidades que deberia tener cada tienda para caer dentro del intervalo de la 
## cat-area como: (LI + 1)*sum_uni_ant
C5_neg_uni <- C5_neg %>%
  pivot_wider(names_from = Mes, values_from = c("Unidades3"), 
              id_cols = c("Ean_Cod","Neen", "Categoria", "Area_Scentia")) %>%
  group_by(Neen, Categoria, Area_Scentia) %>%
  summarise(uni_ant = sum(`Mes Ant`, na.rm = TRUE),
            uni_act = sum(`Mes Act`, na.rm = TRUE))
C5_neg_uni <- merge(C5_neg_uni, Limites_C5, by = c("Categoria", "Area_Scentia"), all.x = TRUE)
C5_neg_uni$Uni_ideales <- (C5_neg_uni$LI_uni5 + 1)*C5_neg_uni$uni_ant
## Cada registro lo vamos a multiplicar por un proporcional, de forma de subir las unidades para 
## que la variacion quede dentro del intervalo. Lo calculamos como: uni_ideales/uni_actuales
C5_neg_uni$Prop <- C5_neg_uni$Uni_ideales / C5_neg_uni$uni_act
C5_neg <- merge(C5_neg, C5_neg_uni, by = c("Neen", "Categoria", "Area_Scentia"))
C5_neg$peso[C5_neg$Mes == "Mes Act"] <- C5_neg$Unidades3[C5_neg$Mes == "Mes Act"] / C5_neg$uni_act[C5_neg$Mes == "Mes Act"]
C5_neg$peso[C5_neg$Mes == "Mes Ant"] <- C5_neg$Unidades3[C5_neg$Mes == "Mes Ant"] / C5_neg$uni_act[C5_neg$Mes == "Mes Ant"]
C5_neg$Condicion_C5[C5_neg$Mes == "Mes Act"] <- 1
C5_neg$Condicion_C5[C5_neg$Mes == "Mes Ant"] <- 0
C5_neg$Unidades4[C5_neg$Mes == "Mes Act"] <- C5_neg$Unidades3[C5_neg$Mes == "Mes Act"] * C5_neg$Prop[C5_neg$Mes == "Mes Act"]
C5_neg$Unidades4[C5_neg$Mes == "Mes Ant"] <- C5_neg$Unidades3[C5_neg$Mes == "Mes Ant"]
rm(C5_neg_uni)

### Trabajamos a los clasificados como "pos"
C5_pos <- subset(C5_detectados, C5_detectados$Signo == "pos")
## Calculamos la cantidad de unidades que deberia tener cada tienda para caer dentro del intervalo
## como: (LS + 1)*sum_uni_ant
C5_pos_uni <- C5_pos %>%
  pivot_wider(names_from = Mes, values_from = c("Unidades3"), 
              id_cols = c("Ean_Cod","Neen", "Categoria", "Area_Scentia")) %>%
  group_by(Neen, Categoria, Area_Scentia) %>%
  summarise(uni_ant = sum(`Mes Ant`, na.rm = TRUE),
            uni_act = sum(`Mes Act`, na.rm = TRUE))
C5_pos_uni <- merge(C5_pos_uni, Limites_C5, by = c("Categoria", "Area_Scentia"), all.x = TRUE)
C5_pos_uni$Uni_ideales <- (C5_pos_uni$LS_uni5 + 1)*C5_pos_uni$uni_ant
## Cada registro lo vamos a multiplicar por un proporcional, de forma de bajar las unidades para 
## que la variacion quede dentro del intervalo. Lo calculamos como: uni_ideales/uni_actuales
C5_pos_uni$Prop <- C5_pos_uni$Uni_ideales / C5_pos_uni$uni_act
C5_pos <- merge(C5_pos, C5_pos_uni, by = c("Neen", "Categoria", "Area_Scentia"))

## El tratamiento de las unidades para los registros que se van por encima del LS depende de la
## cantidad de registros que tenemos por tienda. Llamemos n a la cantidad de eans vendidos en la tienda
## en el mes actual. 
eans <- C5_pos %>%
  filter(Mes == "Mes Act") %>%
  group_by(Categoria, Area_Scentia, Neen) %>%
  summarise(n = n_distinct(Ean_Cod))
C5_pos <- merge(C5_pos, eans, by = c("Categoria", "Area_Scentia", "Neen"))

## n < 10:
C5_pos_9 <- subset(C5_pos, C5_pos$n < 10)
## El valor esperado del peso de cada ean en una tienda particular es 1/n
## Calculamos el peso de cada ean dentro de la tienda, como uni/suma uni
C5_pos_9$peso[C5_pos_9$Mes == "Mes Act"] <- C5_pos_9$Unidades3[C5_pos_9$Mes == "Mes Act"] / C5_pos_9$uni_act[C5_pos_9$Mes == "Mes Act"]
C5_pos_9$peso[C5_pos_9$Mes == "Mes Ant"] <- C5_pos_9$Unidades3[C5_pos_9$Mes == "Mes Ant"] / C5_pos_9$uni_ant[C5_pos_9$Mes == "Mes Ant"]
## Vamos dentro de la tienda, en cada categoria y area. Ordenamos los pesos de los eans en el  mes actual
## Si el mayor peso es menor a (1/n)*2 -> multiplicamos las unidades por el proporcional
## Si el mayor peso es mayor a (1/n)*2 -> lo marcamos, pero no lo arreglamos
C5_pos_9_eval <- C5_pos_9 %>%
  filter(Mes == "Mes Act") %>%
  group_by(Neen, Categoria, Area_Scentia) %>%
  summarise(cant_eans = n_distinct(Ean_Cod),
            peso_max = max(peso))
C5_pos_9_eval$Cota <- 0.5
C5_pos_9_eval$Condicion_C5 <- ifelse(C5_pos_9_eval$peso_max < C5_pos_9_eval$Cota, 1, "REVISAR")
C5_pos_9 <- merge(C5_pos_9, C5_pos_9_eval, by = c("Neen", "Categoria", "Area_Scentia"))
C5_pos_9$Condicion_C5[C5_pos_9$Mes == "Mes Ant"] <- 0
rm(C5_pos_9_eval)

## n >= 10:
C5_pos_10 <- subset(C5_pos, C5_pos$n >= 10)
## El valor esperado del peso de cada ean en una tienda particular es 1/n
## Calculamos el peso de cada ean dentro de la tienda, como uni/suma uni
C5_pos_10$peso[C5_pos_10$Mes == "Mes Act"] <- C5_pos_10$Unidades3[C5_pos_10$Mes == "Mes Act"] / C5_pos_10$uni_act[C5_pos_10$Mes == "Mes Act"]
C5_pos_10$peso[C5_pos_10$Mes == "Mes Ant"] <- C5_pos_10$Unidades3[C5_pos_10$Mes == "Mes Ant"] / C5_pos_10$uni_ant[C5_pos_10$Mes == "Mes Ant"]
## Vamos dentro de la tienda, en cada categoria y area. Ordenamos los pesos de los eans en el  mes actual
## Si el mayor peso es menor a (1/n)*3 -> multiplicamos las unidades por el proporcional
## Si el mayor peso es mayor a (1/n)*3 -> lo marcamos, pero no lo arreglamos
C5_pos_10_eval <- C5_pos_10 %>%
  filter(Mes == "Mes Act") %>%
  group_by(Neen, Categoria, Area_Scentia) %>%
  summarise(cant_eans = n_distinct(Ean_Cod),
            peso_max = max(peso))
C5_pos_10_eval$Cota <- (1/C5_pos_10_eval$cant_eans) * 3 
C5_pos_10_eval$Condicion_C5 <- ifelse(C5_pos_10_eval$peso_max < C5_pos_10_eval$Cota, 1, "REVISAR")
C5_pos_10 <- merge(C5_pos_10, C5_pos_10_eval, by = c("Neen", "Categoria", "Area_Scentia"))
C5_pos_10$Condicion_C5[C5_pos_10$Mes == "Mes Ant"] <- 0
rm(C5_pos_10_eval, C5_pos, C5_pos_uni)

## Acomodamos y juntamos los dos archivos
C5_pos_9 <- select(C5_pos_9, -`medcouple.x`, -Signo, -uni_ant, -uni_act, -`medcouple.y`,
                   -`LI_uni5.y`, -`LS_uni5.y`, -n, -peso_max, -Cota, -cant_eans)
C5_pos_10 <- select(C5_pos_10, -`medcouple.x`, -Signo, -uni_ant, -uni_act,-`medcouple.y`,
                    -`LI_uni5.y`, -`LS_uni5.y`, -n, -peso_max, -Cota, -cant_eans)
C5_pos <- rbind(C5_pos_9, C5_pos_10)
rm(C5_pos_9, C5_pos_10)

## A los valores marcados con 1 en la variable Condicion_C5 los multiplicamos por el proporcional
## Los que tienen valor "REVISAR" o 0 le ponemos las unidades anteriores
C5_pos$Unidades4 <- ifelse(C5_pos$Condicion_C5 == 1, C5_pos$Unidades3*C5_pos$Prop, C5_pos$Unidades3)

## Acomodamos y juntamos los negativos y positivos 
C5_neg <- select(C5_neg, -`medcouple.x`, -Signo, -uni_ant, -uni_act, -`medcouple.y`,
                 -`LI_uni5.y`, -`LS_uni5.y`)
C5_detectados2 <- rbind(C5_neg, C5_pos)
rm(C5_detectados, C5_neg, C5_pos)

## Juntamos los detectados con los no detectados
C5_comunes <- subset(C5_comunes, C5_comunes$Eval_C5 == 1)
C5_comunes$Uni_ideales <- NA
C5_comunes$Prop <- NA
C5_comunes$peso <- NA
C5_comunes$Condicion_C5 <- 0
C5_comunes$Unidades4 <- C5_comunes$Unidades3
C5_comunes <- select(C5_comunes, -medcouple)
C5_detectados2 <- rename(C5_detectados2, "LI_uni5" = `LI_uni5.x`, "LS_uni5" = `LS_uni5.x`)
C5_comunes_post <- rbind(C5_comunes, C5_detectados2)
rm(C5_comunes)

## Juntamos todo lo que usamos en este control con lo que no entro, para formar la base final
codigos <- C5_comunes_post$Codigo
datos_template_C4$C5_aux <- ifelse(datos_template_C4$Codigo %in% codigos, 0, 1)
C5_no_comunes <- subset(datos_template_C4, datos_template_C4$C5_aux == 1)
C5_no_comunes <- select(C5_no_comunes, -C5_aux)
C5_comunes_post <- select(C5_comunes_post, -Eval_C5)
C5_comunes_post <- rename(C5_comunes_post, C5 = Condicion_C5)
C5_no_comunes$cantidad_tiendas <- NA
C5_no_comunes$var_uni <- NA
C5_no_comunes$Unidades4 <- C5_no_comunes$Unidades3
C5_no_comunes$LI_uni5 <- NA
C5_no_comunes$LS_uni5 <- NA
C5_no_comunes$Uni_ideales <- NA
C5_no_comunes$Prop <- NA
C5_no_comunes$peso <- NA
C5_no_comunes$C5 <- 0
datos_template_C5 <- rbind(C5_no_comunes, C5_comunes_post)
rm(codigos)

## Recalculamos la facturación como: Unidades5 * Precio
datos_template_C5$Facturacion2 <- datos_template_C5$Unidades4 * datos_template_C5$Precio_Uni
## Recalculamos el volumen
datos_template_C5  <- mutate(datos_template_C5, "Volumen4" = case_when(datos_template_C5$UnidadMedida == "IN" ~ Unidades4*Contenido*PesoVolumen,
                                                                       datos_template_C5$UnidadMedida == "PAÑOS" ~ Unidades4*Contenido*PesoVolumen,
                                                                       datos_template_C5$UnidadMedida == "PC" ~ Unidades4*Contenido*PesoVolumen,
                                                                       datos_template_C5$UnidadMedida == "UN" ~ Unidades4*Contenido*PesoVolumen,
                                                                       datos_template_C5$UnidadMedida == "UNIDADES" ~ Unidades4*Contenido*PesoVolumen,
                                                                       TRUE ~ (Unidades4*Contenido*PesoVolumen)/1000))
## Recalculamos los proyectados
datos_template_C5$UNI_PROY4 <- datos_template_C5$Unidades4 * datos_template_C5$FactorAJ
datos_template_C5$FACT_PROY2 <- datos_template_C5$Facturacion2 * datos_template_C5$FactorAJ
datos_template_C5$VOL_PROY4 <- datos_template_C5$Volumen4 * datos_template_C5$FactorAJ

## Agregamos a datos_template la variable C5 que identifica a los registros que son modificados en este control
aux <- datos_template_C5[,c("Codigo", "C5")]
datos_template <- merge(datos_template, aux, by = "Codigo", all = TRUE) 
rm(aux, datos_template_C4, C5_comunes_post, C5_detectados2, C5_no_comunes, eans, Limites_C5, tiendas, Var_C5)

datos_template_C5 <- select(datos_template_C5, -Codigo_Neen_Cat_Area, -cantidad_tiendas, -Uni_ideales,
                            -Prop, -peso)

