rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)


set.seed(1)

#limit.rows <- 2000000

WIN <- TRUE
if (WIN) {setwd("c:/repos/repo/santander/code/")}



little = function() {
  
  df <- fread("../input/train2.csv",nrows=-1)
  unique.id <- unique(df$ncodpers)
  limit.people <- 1.25e5
  unique.id <- unique.id[sample(length(unique.id),limit.people)]
  df <- df[df$ncodpers %in% unique.id,]
  # df <- df[sample(nrow(df),limit.rows),]
  # df <- fread("train_ver2.csv",nrows=limit.rows)
  
  str(df)
  
  
  df$fecha_dato <- as.POSIXct(strptime(df$fecha_dato,format="%Y-%m-%d"))
  df$fecha_alta <- as.POSIXct(strptime(df$fecha_alta,format="%Y-%m-%d"))
  df$month <- month(df$fecha_dato)
  unique(df$fecha_dato)
  
  
  write.csv(df, file="../input/little.csv")
}

featureEnginering = function() {
  
  # age
  df$month <- month(df$fecha_dato)
  df$age[(df$age < 18)] <- mean(df$age[(df$age >= 18) & (df$age <=30)],na.rm=TRUE)
  df$age[(df$age > 100)] <- mean(df$age[(df$age >= 30) & (df$age <=100)],na.rm=TRUE)
  df$age[is.na(df$age)] <- median(df$age,na.rm=TRUE)
  df$age <- round(df$age)
  
  # ind_nuevo
   df$ind_nuevo[is.na(df$ind_nuevo)] <- 1 
  
  #antiguedad
  df$antiguedad[is.na(df$antiguedad)] <- min(df$antiguedad,na.rm=TRUE)
  df$antiguedad[df$antiguedad<0] <- 0
  
  #fecha_alta
  df$fecha_alta[is.na(df$fecha_alta)] <- median(df$fecha_alta,na.rm=TRUE)
  
  #indrel
  df$indrel[is.na(df$indrel)] <- 1
  
  #tipodom # nomprov
  df <- df %>% select(-tipodom,-cod_prov)
  
  #ind_actividad_cliente
  #probablemente los clientes nuevos que son los que tienen el resto de los
  #datos vacios ... son los que primero se le dan unos determinados tipos de produtos
  df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- median(df$ind_actividad_cliente,na.rm=TRUE)
  
  #nomprov
  
  df$nomprov[df$nomprov==""] <- "UNKNOWN"
  
  #renta
  ##por provincia no esta mal, pero tambien se tiene que hacer por segmento.
  new.incomes <-df %>%
    select(nomprov) %>%
    merge(df %>%
            group_by(nomprov) %>%
            summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
    select(nomprov,med.income) %>%
    arrange(nomprov)
  df <- arrange(df,nomprov)
  df$renta[is.na(df$renta)] <- new.incomes$med.income[is.na(df$renta)]
  rm(new.incomes)
  
  df$renta[is.na(df$renta)] <- median(df$renta,na.rm=TRUE)
  
  #features
  df[is.na(df)] <- 0
  
  #empty strings
  df$indfall[df$indfall==""]               <- "N"
  df$tiprel_1mes[df$tiprel_1mes==""]       <- "A"
  df$indrel_1mes[df$indrel_1mes==""] <- "1"
  df$indrel_1mes[df$indrel_1mes=="P"] <- "5"
  df$indrel_1mes <- as.factor(as.integer(df$indrel_1mes))
  
  df$pais_residencia[df$pais_residencia==""] <- "UNKNOWN"
  df$sexo[df$sexo==""]                       <- "UNKNOWN"
  df$ult_fec_cli_1t[df$ult_fec_cli_1t==""]   <- "UNKNOWN"
  df$ind_empleado[df$ind_empleado==""]       <- "UNKNOWN"
  df$indext[df$indext==""]                   <- "UNKNOWN"
  df$indresi[df$indresi==""]                 <- "UNKNOWN"
  df$conyuemp[df$conyuemp==""]               <- "UNKNOWN"
  df$segmento[df$segmento==""]               <- "UNKNOWN"
  
  
  #Convert all the features to numeric dummy indicators
  features <- grepl("ind_+.*ult.*",names(df))
  df[,features] <- lapply(df[,features],function(x)as.integer(round(x)))
  df$total.services <- rowSums(df[,features],na.rm=TRUE)
  }





little()



df <- fread("../input/little.csv",nrows=-1)
df <- readLines("../input/train.csv")