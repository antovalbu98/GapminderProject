## EV notas para generar el modelo y predecir

# para ejemplo cargo este ds de librería, pongo país como columna
# Population hace de año
# y me quedo con 4 columnas de datos como en nuestro caso.
# El target es Murder
library(dplyr)
library(data.table)
df <- as.data.frame(state.x77)
df
setDT(df, keep.rownames = TRUE)[]
colnames(df)[1] <- "Pais"
df[,5] <- NULL
df[,6:8] <- NULL
df

#####----------- pruebas sin meter en función, pruebo directamente
# separo registro objetivo del df completo
#df_obj <- df[df$Pais == "Alaska" & df$Population == "365", ]  Es el bueno, pero no lo puedo meter así en función
#df_obj <- df[df[ ,1] == "Alaska", ]  #da warning
#df_obj <- df_obj[df_obj[ ,2] == "365", ]
row_objetivo <- which((df[ ,1] == "Alaska")&df[ ,2] == 365)
df_obj <- df[row_objetivo, ]
df_obj

# resto del df es df_train
#row_objetivo <- which(grepl("Alaska", df$Pais) & grepl(365, df$Population))
row_objetivo <- which(grepl("Alaska", df[ ,1]) & grepl(365, df[ ,2]))

row_objetivo <- which((df[ ,1] == "Alaska")&df[ ,2] == 365)
df_train <- df[-row_objetivo, ]
df_train
#####----------- 


#####---- FUNCIÓN SEPARA TRAIN 
# Función para separar train - VERSIÓN CON df DE PRUEBA
# Usando pais_objetivo y year_objetivo como entradas en la llamada
separa_train <- function(df, pais_objetivo, year_objetivo){
  
  # parar si falta país o año
#  if((!all(pais_objetivo %in% df)) || (!all(year_objetivo %in% df))) {
#    print("error falta país o año")
#    #logerror("Algún parámetro pedido no está en el df", logger = 'log')
#    stop()
#  }
  
  # si todo ok elimino fila objetivo y quito los registros con missings
  row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
  df_train <- df[-row_objetivo, ]
  df_train <- subset(df_train, (!is.na(df_train[,ncol(df_train)])))
  return(df_train)
  
}

# Función para separar train - VERSIÓN COMPLETA (CUANDO ESTÉ LISTO)
# pais_objetivo = config$pais_objetivo
# year_objetivo = config$year_objetivo
separa_train <- function(df, config){
  
  # parar si falta país o año
  if((!all(config$pais_objetivo %in% df)) || (!all(config$year_objetivo %in% df))){
    print("error falta país o año")
    #logerror("Algún parámetro pedido no está en el df", logger = 'log')
    stop()
  }
  
  # si todo ok
  row_objetivo <- which((df[ ,1] == config$pais_objetivo) & df[ ,2] == config$year_objetivo)
  df_train <- df[-row_objetivo, ]
  df_train <- subset(df_train, (!is.na(df_train[,ncol(df_train)])))
  
  return(df_train)
  
}





#####---- FUNCIÓN SEPARA TEST 
# Función para separar test (nuestro objetivo) - VERSIÓN CON df DE PRUEBA
# Usando pais_objetivo y year_objetivo como entradas en la llamada
separa_test <- function(df, pais_objetivo, year_objetivo){
  
  # me quedo registro objetivo y le quito columna de target
  row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
  df_test <- df[row_objetivo, ]
  df_test[,ncol(df_test)] <- NULL
  
  return(df_test)
}

# Función para separar test (nuestro objetivo) - VERSIÓN COMPLETA (CUANDO ESTÉ TODO)
# pais_objetivo = config$pais_objetivo
# year_objetivo = config$year_objetivo

separa_test <- function(df, config){
  
    # si todo ok me quedo registro objetivo y le quito la columna target
  row_objetivo <- which((df[ ,1] == config$pais_objetivo) & df[ ,2] == config$year_objetivo)
  df_test <- df[row_objetivo, ]
  df_test[,ncol(df_test)] <- NULL
  
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
df_test <- separa_test(df,"Alaska",365)
df_test


######   FUNCIONES PARA EL MODELO
# primero sin funciones

# fit del modelo
#fit_modelo <- lm(Murder ~ Income + Illiteracy + Population, data=df_train)
#summary(fit_modelo)
# predicción
#newdata = data.frame(x1 = x1Vector, x2 = x2Vector, x3 = x3Vector)


#####---- FUNCIÓN ENTRENAR MODELO
# función entrenar modelo - llama a separa_test

entrenar_modelo <- function(df){
  print("separar train")
  # separo df_train del df completo
  df_train <- separa_train(df,"Alaska",365)  
  print("separar train: OK")
  
  # separar target y features
  nombre_columnas <- colnames(df_train)
  nombre_columnas
  features <- nombre_columnas[2:(ncol(df_train)-1)] # la 1 es país, la última el target
  features
  target <- nombre_columnas[ncol(df_train)]
  target
  print("sacar features y target train:OK")
  print(features[1])
  
  # entrenar
  fit_modelo <- lm(target ~ features[1] + features[2] + features[3], data=df_train)
  print(summary(fit_modelo))
  
  return(fit_modelo)
  
}

#####---- FUNCIÓN PREDECIR OBJETIVO 
# función predecir objetivo - llama a separa test
predecir_objetivo <- function(df_objetivo,modelo){
  
  # separo el registro objetivo
  df_test <- separa_test(df,"Alaska",365)
  
  # predicción
  predictions = predict.lm(modelo, df_test)
  return(predictions)
  
}


#LLAMADAS PARA PROBAR LAS FUNCIONES DE ENTRENAMIENTO Y PREDICCIÓN (llaman a las de separar train/test)

modelo_entrenado <- entrenar_modelo(df)
prediccion <- predecir_objetivo(df,modelo_entrenado)

