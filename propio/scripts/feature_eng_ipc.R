#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
rm(list=ls())
dataset  <- fread("/Users/achain/Downloads/competencia2_2022.csv.gz")  #donde entreno
ipc<- fread('/Users/achain/Documents/github/labo/propio/scripts/ipc_nac.csv')

ipc_solo<-ipc[foto_mes>=201901 & foto_mes<=202105 ,]
ipc_solo$foto_mes<- as.integer(ipc_solo$foto_mes)
#dataset[ipc,on='foto_mes']
dataset_con_ipc<-left_join(dataset,ipc_solo,by='foto_mes')
## COlumnas monetarias
col_num<-c(1,
  8,
  9,
  10,
  11,
  12,
  16,
  17,
  19,
  20,
  21,
  23,
  26,
  29,
  32,
  34,
  36,
  38,
  40,
  41,
  43,
  44,
  46,
  53,
  54,
  57,
  59,
  61,
  63,
  65,
  67,
  69,
  71,
  73,
  75,
  78,
  80,
  82,
  84,
  86,
  88,
  90,
  92,
  94,
  105,
  107,
  113,
  116,
  117,
  118,
  119,
  120,
  121,
  122,
  123,
  125,
  126,
  127,
  129,
  132,
  135,
  138,
  139,
  140,
  141,
  142,
  143,
  144,
  145,
  147,
  148,
  149,
  151,
  154)


nombres<-c()
for (j in col_num) {
  nombres<-append(nombres,names(dataset_con_ipc[,..j]))
}
c<-0
for (i in col_num) {
  col<-dataset_con_ipc[, ..i]/dataset_con_ipc[, 156] 
  c<-c+1
  dataset_con_ipc[,paste0(nombres[c],'_defla'):=col]
}


c<-0
for (i in col_num) {
  c<-c+1
  dataset_con_ipc[,nombres[c]:=NULL]
}


dataset_con_ipc[,mean(mplazo_fijo_pesos_defla)]
dataset[,mean(mplazo_fijo_pesos)]


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  
  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]
  
  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]
  
  
  
  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite_defla,  Visa_mfinanciacion_limite_defla) , na.rm=TRUE ) ]
  
  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal_defla,  Visa_msaldototal_defla) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos_defla,  Visa_mconsumospesos_defla) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares_defla,  Visa_mconsumosdolares_defla) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra_defla,  Visa_mlimitecompra_defla) , na.rm=TRUE ) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado_defla,  Visa_mpagado_defla) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo_defla,  Visa_mpagominimo_defla) , na.rm=TRUE ) ]
  dataset[ , ratio_mrent          := ifelse(!is.na(mrentabilidad_defla/mrentabilidad_annual_defla),mrentabilidad_defla/mrentabilidad_annual_defla,0) ]
  dataset[ , ratio_mrent          := ifelse(!is.na(mrentabilidad_defla/mrentabilidad_annual_defla ),mrentabilidad_defla/mrentabilidad_annual_defla ,0)]
  dataset[ , margen_tasa          := mactivos_margen_defla-mpasivos_margen_defla ]
  dataset[ , spread          := mactivos_margen_defla-mpasivos_margen_defla ]
  dataset[ , ratio_tran_traj          := ifelse(!is.na(ctarjeta_debito_transacciones/ctarjeta_debito),ctarjeta_debito_transacciones/ctarjeta_debito,0) ]
  dataset[ , ratio_pres_pers          := ifelse(!is.na(mprestamos_personales_defla/cprestamos_personales),mprestamos_personales_defla/cprestamos_personales,0 )]
  dataset[ , ratio_pres_prend          := ifelse(!is.na(mprestamos_prendarios_defla/cprestamos_prendarios),mprestamos_prendarios_defla/cprestamos_prendarios,0 )]
  dataset[ , ratio_pres_hipo        := ifelse(!is.na(mprestamos_hipotecarios_defla/cprestamos_hipotecarios),mprestamos_hipotecarios_defla/cprestamos_hipotecarios,0 )]
  dataset[ , ratio_plazo_fijo       := ifelse(!is.na(rowSums( cbind( mplazo_fijo_dolares_defla,  mplazo_fijo_pesos_defla) , na.rm=TRUE )/cplazo_fijo), (rowSums( cbind( mplazo_fijo_dolares_defla,  mplazo_fijo_pesos_defla) , na.rm=TRUE )/cplazo_fijo),0)]
  dataset[ , ratio_inv_1       := ifelse(!is.na(rowSums( cbind( minversion1_pesos_defla,  minversion1_dolares_defla) , na.rm=TRUE )/cinversion1), (rowSums( cbind( minversion1_pesos_defla,  minversion1_dolares_defla) , na.rm=TRUE )/cinversion1),0)]
  dataset[ , ratio_inv_2       := ifelse(!is.na(minversion2_defla/cinversion2),minversion2_defla/cinversion2,0) ]
  dataset[ , seguro_total       := rowSums( cbind( cseguro_vida,  cseguro_auto,cseguro_vivienda,cseguro_accidentes_personales) , na.rm=TRUE ) ]
  dataset[ , payroll_total       := rowSums( cbind( mpayroll_defla,  mpayroll2_defla) , na.rm=TRUE ) ]
  dataset[ , ratio_payroll_total       := ifelse(!is.na(rowSums( cbind( mpayroll_defla,  mpayroll2_defla) , na.rm=TRUE ) /rowSums( cbind( cpayroll_trx,  cpayroll2_trx) , na.rm=TRUE )), (rowSums( cbind( mpayroll_defla,  mpayroll2_defla) , na.rm=TRUE ) /rowSums( cbind( cpayroll_trx,  cpayroll2_trx) , na.rm=TRUE )),0)]
  dataset[ , ratio_payroll_2       :=   ifelse(!is.na(mpayroll2_defla/cpayroll2_trx),(mpayroll2_defla/cpayroll2_trx),0)]
  dataset[ , ratio_payroll       :=   ifelse(!is.na(mpayroll_defla/cpayroll_trx),mpayroll_defla/cpayroll_trx,0)]
  dataset[ , ratio_buy_sel_forex      :=   ifelse(!is.na(mforex_buy_defla/mforex_sell_defla),(mforex_buy_defla/mforex_sell_defla),0)]
  dataset[ , ratio_buy_forex      :=   ifelse(!is.na(mforex_buy_defla/cforex_buy),(mforex_buy_defla/cforex_buy),0)]
  dataset[ , ratio_sel_forex      :=   ifelse(!is.na(mforex_sell_defla/cforex_sell),(mforex_sell_defla/cforex_sell),0)]
  dataset[ , debito_anti      :=   ifelse(!is.na(rowSums( cbind( ccuenta_debitos_automaticos,  ctarjeta_visa_debitos_automaticos,ctarjeta_master_debitos_automaticos) , na.rm=TRUE )*cliente_antiguedad),(rowSums( cbind( ccuenta_debitos_automaticos,  ctarjeta_visa_debitos_automaticos,ctarjeta_master_debitos_automaticos) , na.rm=TRUE )*cliente_antiguedad),0)]
  dataset[ , mv_monto_utilizado := mv_mlimitecompra - mv_mpagado]
  
  
  
  
  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )
  
  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}

dataset_con_ipc[,ipc_nac:=NULL]
EnriquecerDataset(dataset_con_ipc,'/Users/achain/Downloads/competencia2_2022_FE.csv.gz')
