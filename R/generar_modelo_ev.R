## Funciones para train/test, generar modelo y predicción
## con el df real y los datos de config


#####---- FUNCIÓN SEPARA TRAIN 

# Función para separar train - VERSIÓN COMPLETA
#
# pais_objetivo = config$columnas$pais_objetivo
# year_objetivo = config$columnas$year_objetivo

separa_train <- function(df, config){
  
  print("empieza separar train...")
  
  tryCatch(expr = {
    
    # si todo ok elimino fila objetivo y quito los registros con missings
    row_objetivo <- which((df[ ,1] == config$columnas$pais_objetivo) & df[ ,2] == config$columnas$year_objetivo)
    
    print("row objetivo:")
    print(row_objetivo)
    
    # si existe la fila objetivo, saco df_train
    if ((row_objetivo +1)>=2){
      
      df_train <- df[-row_objetivo, ]
      df_train <- subset(df_train, (!is.na(df_train[,ncol(df_train)])))
    }
    
    # si no existe, paro función
    else{
      print("No existe registro objetivo")
      loginfo("No existe registro objetivo.",
              logger = 'log')
      stop() 
    }
    
  }, error = function(e){
    print("Error al generar train")
    loginfo("No se puede eliminar registro objetivo para crear train. Esa combinación de país y año no está disponible.",
            logger = 'log')
    stop() 
    
  })
  print("train separado ok")
  return(df_train)
  
}





#####---- FUNCIÓN SEPARA TEST 
# Función para separar test (nuestro objetivo) - VERSIÓN COMPLETA 
#
# pais_objetivo = config$columnas$pais_objetivo
# year_objetivo = config$columnas$year_objetivo

separa_test <- function(df, config){
  
  print("empieza separar test...")
  
  tryCatch(expr = {
    
    # compruebo que hay registro objetivo
    row_objetivo <- which((df[ ,1] == config$columnas$pais_objetivo) & df[ ,2] == config$columnas$year_objetivo)
    
    if((row_objetivo+1)>=2){
      
      print("row objetivo:")
      print(row_objetivo)
      
      # me quedo con registro objetivo
      df_test <- df[row_objetivo, ]
      
      if("Murder" %in% colnames(df)){
        print("elimino murder")
        # si hay columna target, la elimino
        df_test$Murder <- NULL
        
      }
      
      # si no hay columna target, hay un error
      else{
        print("El registro objetivo no tiene target Murder")
        loginfo("El registro objetivo no tiene target Murder",logger = 'log')
        stop()
      }
      
    } 
    # si no hay registro objetivo, paro
    else{
      print("No se puede extraer el registro objetivo para predicción (test)")
      loginfo("No se puede extraer el registro para la predicción. Esa combinación de país y año no está disponible",logger = 'log')
      stop()
    }
    
  }, error = function(e){
    
    logerror("Error en la extracción del registro objetivo para predicción",
             logger = 'log')
    stop() 
    
  })
  print("test separado ok")
  return(df_test)
}






#####---- FUNCIÓN ENTRENAR MODELO
# función entrenar modelo - llama a separa_test
# El target es murdered_women
# Las features:  gdp, life_exp, fem_particip

entrenar_modelo <- function(df){
  print("Voy a entrenar modelo, primero...")
  print("... llamo a separar train")
  
  # separo df_train del df completo
  df_train <- separa_train(df,config)  
  
  tryCatch(expr = {
    print("entreno el modelo...")
    
    # entrenar
    fit_modelo <- lm(murdered_women ~ gdp + life_exp + fem_particip, data=df_train)
    print(summary(fit_modelo))
    
  }, error = function(e){
    
    logerror("No se ha podido entrenar el modelo. Comprobar nombres de features que participan",
             logger = 'log')
    stop()
  })
  print("modelo entrenado")
  return(fit_modelo)
  
}

#####---- FUNCIÓN PREDECIR OBJETIVO 
# función predecir objetivo - llama a separa test

predecir_objetivo <- function(df_objetivo,modelo){
  print("Voy a predecir objetivo, primero...")
  print("... llamo a separar test")
  
  # separo el registro objetivo
  df_test <- separa_test(df,"Alaska",365)
  
  
  tryCatch(expr = {
    print("empieza predicción...")
    # predicción
    predictions = predict.lm(modelo, df_test)
    
  }, error = function(e){
    
    logerror("No se ha podido generar la predicción del registro objetivo",
             logger = 'log')
    stop()
  })
  
  print("predicción finalizada")
  return(predictions)
  
}


#LLAMADAS PARA PROBAR LAS FUNCIONES DE ENTRENAMIENTO Y PREDICCIÓN (llaman a las de separar train/test)

modelo_entrenado <- entrenar_modelo(df)
prediccion <- predecir_objetivo(df,modelo_entrenado)

# A su vez, modelo_entrenado y prediccion serán las entradas de las funciones de 
# que generan los ficheros de salida
