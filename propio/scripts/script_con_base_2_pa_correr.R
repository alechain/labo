#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library("data.table")
library("rlist")
library("yaml")

library("rpart")
library("parallel")

#paquetes necesarios para la Bayesian Optimization
library("DiceKriging")
library("mlrMBO")



setwd("/home/alechain97/buckets/b1/exp/drift_tot")


kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "04.02 - BO Rpart FE"
kBO_iter    <-  200   #cantidad de iteraciones de la Optimizacion Bayesiana


kcarpeta_datasets    <- "/home/alechain97/buckets/b1/datasets/"                          #VM o Ubuntu



#Archivo con datos etiquetados para entrenamiento
karchivo_entrada      <-  paste0(kcarpeta_datasets, "competencia2_2022.csv.gz")


#Ganancia por TP
kTPGain               <-  78000

#Pérdida por FP
kFPGain               <-  -2000


hs  <- makeParamSet(
  makeNumericParam("cp"       , lower= -1   , upper=    0.1),
  makeIntegerParam("minsplit" , lower=  1L  , upper= 8000L),  #la letra L al final significa ENTERO
  makeIntegerParam("minbucket", lower=  1L  , upper= 2000L),
  makeIntegerParam("maxdepth" , lower=  3L  , upper=   20L),
  forbidden = quote( minbucket > 0.5*minsplit ) )


ksemilla_azar  <- 500113





get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
  
  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento
  
  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
  
  return( experimento_actual )
}


particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ . ", 
                   data= data[ fold != fold_test,-c("clase_ternaria") ],
                   xval= 0,
                   control= param )
  
  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, -c("clase_ternaria") ], type = "prob")
  
  prob_baja2  <- prediccion[, "SI"]
  
  ganancia_testing  <- sum(  data[ fold==fold_test ][ prob_baja2 >0.025,  ifelse( clase_ternaria=="BAJA+2", kTPGain, kFPGain ) ] )
  
  return( ganancia_testing )
}


ArbolesCrossValidation  <- function( data, param, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )
  
  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 5 )   #se puede subir a 5 si posee Linux o Mac OS
  
  data[ , fold := NULL ]
  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo la ganancia
}


#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia  <- function( x )
{
  GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
  
  xval_folds  <- 5
  ganancia  <-  ArbolesCrossValidation( dtrain, param=x, qfolds= xval_folds, pagrupa="clase_ternaria", semilla=ksemilla_azar )
  
  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if(  ganancia > GLOBAL_ganancia_max )
  {
    GLOBAL_ganancia_max <<-  ganancia  #asigno la nueva maxima ganancia
    
    modelo  <- rpart("clase_binaria ~ . ",
                     data= dtrain[,-c("clase_ternaria","foto_mes") ],
                     xval= 0,
                     control= x )
    
    #genero el vector con la prediccion, la probabilidad de ser positivo
    prediccion  <- predict( modelo, dapply[,-c("clase_ternaria") ])
    
    prob_baja2  <- prediccion[, "SI"]
    Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )
    
    entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )
    
    #genero el archivo para Kaggle
    fwrite( entrega, 
            file= paste0("/home/alechain97/buckets/b1/exp/drift_tot",kkaggle, GLOBAL_iteracion, ".csv" ),
            sep=  "," )
  }
  
  return( ganancia )
}



#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento




#en estos archivos quedan los resultados
kbayesiana  <- paste0("./E",  kexperimento, "_rpart.RDATA" )
kkaggle     <- paste0("./E",kexperimento, "_rpart_kaggle_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0


#cargo los datos
dataset <- fread(karchivo_entrada)

#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ !(foto_mes  %in% (202103)), 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


#Para hacer pruebas rapidas puedo reducir el dataset a una fraccion
#subsample <- sample(1:nrow(dataset), .1 * nrow(dataset))
#dataset <- dataset[subsample,]

dtrain  <- dataset[ !(foto_mes  %in% (202103)) ]  #defino donde voy a entrenar

dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

factor(dapply[,clase_ternaria])
#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr( show.learner.output = FALSE)

funcion_optimizar  <- EstimarGanancia

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar,
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,
  has.simple.signature = FALSE
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 60,  save.file.path= kbayesiana)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else  run  <- mboContinue( kbayesiana )   #retomo en