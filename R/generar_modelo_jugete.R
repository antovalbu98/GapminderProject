## EV notas para generar el modelo y predecir

# para ejemplo cargo este ds de la librería dplyr, pongo país como columna
# Population hace de año y también lo uso como feature en el modelo
# y así me quedo con 4 columnas de datos como en nuestro caso.
# El target es Murder

library(logging)

library(dplyr)
library(data.table)
df <- as.data.frame(state.x77)

setDT(df, keep.rownames = TRUE)[]
colnames(df)[1] <- "Pais"
df[,5] <- NULL
df[,6:8] <- NULL
df



#####---- FUNCIÓN SEPARA TRAIN 
# Función para separar train - VERSIÓN CON df DE PRUEBA
# Usando pais_objetivo y year_objetivo como entradas en la llamada

separa_train <- function(df, pais_objetivo, year_objetivo){
  
  print("empieza separar train...")
  
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



#####---- FUNCIÓN SEPARA TEST 
# Función para separar test (nuestro objetivo) - VERSIÓN CON df DE PRUEBA
# Usando pais_objetivo y year_objetivo como entradas en la llamada
separa_test <- function(df, pais_objetivo, year_objetivo){
  
  print("empieza separar test...")
  
  tryCatch(expr = {
    
    # compruebo que hay registro objetivo
    row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
    
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





# Pruebas de las funciones de separar train test 
# -- NOTA PARA EL COMPLETO --
# en el bueno, no hace falta indicar Alaska y 365 en la llamada, 
# ya lo cogemos el config dentro de la función
# df_train <- separa_train(df)
# df_test <- separa_test(df)

df_train <- separa_train(df,"Alaska",365)  
df_train
df

df_train <- separa_train(df_train,"Alaska",365)  
#df_train


df_test <- separa_test(df,"Alaska",365)
df_test
df_test <- separa_test(df_test,"Alaska",365)


######   FUNCIONES PARA EL MODELO



#####---- FUNCIÓN ENTRENAR MODELO
# función entrenar modelo - llama a separa_test

entrenar_modelo <- function(df){
  print("llamo a separar train...")
  
  # separo df_train del df completo
  df_train <- separa_train(df,"Alaska",365)  
  
  tryCatch(expr = {
    print("entreno el modelo...")
    
    # entrenar      
    fit_modelo <- lm(Murder ~ Population + Income + Illiteracy, data=df_train)
    print(summary(fit_modelo))
    
  }, error = function(e){
    
    logerror("No se ha podido entrenar el modelo. Comprobar nombres de features que participan",
             logger = 'log')
    stop()
  })
  
  return(fit_modelo)
  
}

#####---- FUNCIÓN PREDECIR OBJETIVO 
# función predecir objetivo - llama a separa test

predecir_objetivo <- function(df_objetivo,modelo){
  
  print("llamo a separar test...")
  
  # separo el registro objetivo
  df_test <- separa_test(df,"Alaska",365)
  
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


#LLAMADAS PARA PROBAR LAS FUNCIONES DE ENTRENAMIENTO Y PREDICCIÓN (llaman a las de separar train/test)

modelo_entrenado <- entrenar_modelo(df)
prediccion <- predecir_objetivo(df,modelo_entrenado)

# para probar que captura errores:
df_train <- separa_train(df,"Alaska",365)  
df_train
df

df_train <- separa_train(df_train,"Alaska",365)  
#df_train


df_test <- separa_test(df,"Alaska",365)
df_test
df_test <- separa_test(df_test,"Alaska",365)




