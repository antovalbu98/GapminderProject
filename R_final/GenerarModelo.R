separa_train <- function(df, config){
  
  print("empieza separar train...")
  
  
  pais_objetivo <- config$columnas$pais_objetivo
  print( pais_objetivo)
  
  year_objetivo <- config$columnas$year_objetivo
  print(year_objetivo)
  tryCatch(expr = {
    
    # si todo ok elimino fila objetivo y quito los registros con missings
    row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
    
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





separa_test <- function(df,config){
  
  print("empieza separar test...")
  
  
  pais_objetivo <- config$columnas$pais_objetivo
  year_objetivo <- config$columnas$year_objetivo
    
  tryCatch(expr = {
    
    logerror_msg <- "No se puede extraer registro para predicción"
    # compruebo que hay registro objetivo
    row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
    
    if((row_objetivo+1)>=2){
      
      print("row objetivo:")
      print(row_objetivo)
      
      # me quedo con registro objetivo
      df_test <- df[row_objetivo, ]
      
      if("murdered_women" %in% colnames(df)){
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





entrenar_modelo <- function(df,config){
  print("llamo a separar train...")
  
  # separo df_train del df completo
  
  df_train <- separa_train(df, config)  
  
  tryCatch(expr = {
    print("entreno el modelo...")
    
    # entrenar
    fit_modelo <- lm(murdered_women ~ fem_particip + gdp + life_exp, data=df_train)
    print(summary(fit_modelo))
    
  }, error = function(e){
    
    logerror("No se ha podido entrenar el modelo. Comprobar nombres de features que participan",
             logger = 'log')
    stop()
  })
  
  return(fit_modelo)
  
}




predecir_objetivo <- function(df,modelo,config){
  
  print("llamo a separar test...")
  
  # separo el registro objetivo

  df_test <- separa_test(df, config)
  
  tryCatch(expr = {
    print("empieza predecir objetivo...")
    
    # predicción
    predictions = predict.lm(modelo, df_test)
    
    print("predict:")
    print(predictions)
    
  }, error = function(e){
    
    logerror("No se ha podido generar la predicción del registro objetivo",
             logger = 'log')
    stop()
  })
  
  return(predictions)
  
}