#' @title  Función separa_train()
#' @description  Esta función toma el df y busca el registro que combina el dato de 
#' país y anio que se indican en el fichero config. 
#' Se crea un nuevo df (df_train) eliminando este registro que servirá
#' para entrenar el modelo
#' 
#'
#' @param df 
#' @param config 
#'
#' @return df_train
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

    logerror("No se puede eliminar registro objetivo para crear train. Esa combinación de país y año no está disponible.",
            logger = 'log')
    stop() 
    
  })
  print("train separado ok")
  return(df_train)
  
}





#' @title  Función separa_test()
#' 
#' @description  Esta función busca en el df original el registro que combina
#' el país y anio introducidos por el usuario en el fichero de configuración.
#' El registro se extrae y se elimina el campo de target y se guarda como 
#' df_test. Este df se usará en la predicción del target. 
#'
#' @param df 
#' @param config 
#'
#' @return df_test
separa_test <- function(df,config){
  
  print("empieza separar test...")
  
  
  pais_objetivo <- config$columnas$pais_objetivo
  year_objetivo <- config$columnas$year_objetivo
    
  tryCatch(expr = {
    
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





#' @title  Función entrenar_modelo()
#' 
#' @description  Esta función llama a la función "separa_train()" y sobre el df_train
#' que devuelve calcula un modelo de regresión lineal para el target basado en los
#' parámetros fem_particip, gdp y life_ex
#' Devuelve como salida el modelo entrenado
#'
#' @param df 
#' @param config 
#'
#' @return fit_modelo. El modelo entrenado para df_train
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




#' @title  Función predecir_objetivo()
#' 
#' @description  Función que predice el target para el registro objetivo 
#' empleando el modelo entrenado en la función "entrenar_modelo()". 
#' Para ello, la función llama inicialmene a la función 
#' "separa_test()" para obtener el registro objetivo 
#'
#' @param df 
#' @param modelo 
#' @param config 
#'
#' @return predictions La predicción con este modelo para el registro objetivo
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