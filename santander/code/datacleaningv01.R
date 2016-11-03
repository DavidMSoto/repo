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

  write.csv(df, file="../input/little.csv")
}


# read --------------------------------------------------------------------
#fecha_dato 				The table is partitioned for this column
#ncodpers 				  Customer code
#ind_empleado 			Employee index: A active, B ex employed, F filial, N not employee, P pasive
#pais_residencia 		Customer's Country residence
#sexo 				    	Customer's sex
#age 					      Age
#fecha_alta 				The date in which the customer became as the first holder of a contract in the bank
#ind_nuevo 			  	New customer Index. 1 if the customer registered in the last 6 months.
#antiguedad 				Customer seniority (in months)
#indrel 				  	1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)
#ult_fec_cli_1t 			Last date as primary customer (if he isn't at the end of the month)
#indrel_1mes 			Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
#tiprel_1mes 			Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
#indresi 				  Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
#indext 					Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
#conyuemp 				Spouse index. 1 if the customer is spouse of an employee
#canal_entrada 		channel used by the customer to join
#indfall 			  	Deceased index. N/S
#tipodom 				  Addres type. 1, primary address
#cod_prov 				Province code (customer's address)
#nomprov 			  	Province name
#ind_actividad_cliente 	Activity index (1, active customer; 0, inactive customer)
#renta 					  Gross income of the household
#segmento 				segmentation: 01 - VIP, 02 - Individuals 03 - college graduated

#exposure         
#holdings         

featureEnginering = function(df) {
  
  #dates
  df$fecha_dato <- as.POSIXct(strptime(df$fecha_dato,format="%Y-%m-%d"))
  df$fecha_alta <- as.POSIXct(strptime(df$fecha_alta,format="%Y-%m-%d"))
  df$month <- month(df$fecha_dato)
  
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
  df$total_services <- rowSums(df[,features],na.rm=TRUE)
  
  return(df)
  
  }



##*****************************************

rm(list = ls())

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(sqldf) # sql



set.seed(1)

#limit.rows <- 2000000

WIN <- TRUE
if (WIN) {setwd("c:/repos/repo/santander/code/")}


little()



df <- fread("../input/little.csv",nrows=-1)

df_e <- featureEnginering(df)

names(df_e)
unique(f$ncodpers)

  single_old <-  sqldf("SELECT 
                       *
                   FROM df_e
                   where 
                    nomprov = 'ALBACETE'
                      and ncodpers = '36280'
                   " )
  
  
  single_new <-  sqldf("SELECT 
                           *
                   FROM df_e
                   where 
                    nomprov = 'ALBACETE'
                  and ncodpers = '745700'
                   " )
  
  multi_uni <-  sqldf("SELECT 
                         ncodpers,  segmento, renta,  count(ncodpers), total_services
                   FROM df_e
                   where 
                    nomprov = 'ALBACETE'  
                  -- and count(ncodpers) ='17' 
                  -- and  segmento = '03 - UNIVERSITARIO'
                  -- and total_services != '2'
                    group by ncodpers, segmento
                    order by 4
                  
                   " )
  
  
  
  # read --------------------------------------------------------------------
  #ind_ahor_fin_ult1 	Saving Account
  #ind_aval_fin_ult1 	Guarantees
  #ind_cco_fin_ult1 	Current Accounts
  #ind_cder_fin_ult1 	Derivada Account
  #ind_cno_fin_ult1 	Payroll Account
  #ind_ctju_fin_ult1 	Junior Account
  #ind_ctma_fin_ult1 	Más particular Account
  #ind_ctop_fin_ult1 	particular Account
  #ind_ctpp_fin_ult1 	particular Plus Account
  #ind_deco_fin_ult1 	Short-term deposits
  #ind_deme_fin_ult1 	Medium-term deposits
  #ind_dela_fin_ult1 	Long-term deposits
  #ind_ecue_fin_ult1 	e-account
  #ind_fond_fin_ult1 	Funds
  #ind_hip_fin_ult1 	Mortgage
  #ind_plan_fin_ult1 	Pensions
  #ind_pres_fin_ult1 	Loans
  #ind_reca_fin_ult1 	Taxes
  #ind_tjcr_fin_ult1 	Credit Card
  #ind_valo_fin_ult1 	Securities
  #ind_viv_fin_ult1 	Home Account
  #ind_nomina_ult1 	  Payroll
  #ind_nom_pens_ult1 	Pensions
  #ind_recibo_ult1 	  Direct Debit
  
         
  
  
