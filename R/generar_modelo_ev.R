## EV notas para generar el modelo y predecir

# para ejemplo cargo este ds de librería, pongo país como columna
# Population hace de año
# y me quedo con 4 columnas de datos como en nuestro caso.
# El target es Income
library(dplyr)
library(data.table)
df <- as.data.frame(state.x77)

setDT(df, keep.rownames = TRUE)[]
colnames(df)[1] <- "Pais"
df[,7:9] <- NULL
df

#####----------- antes de meter en función, pruebo directamente
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
# Función para separar train - versión prueba
# Usando pais_objetivo y year_objetivo como entradas en la llamada
separa_train <- function(df, pais_objetivo, year_objetivo){
  
  # parar si falta país o año
  if((!all(pais_objetivo %in% df)) || (!all(year_objetivo %in% df))) {
    print("error falta país o año")
    #logerror("Algún parámetro pedido no está en el df", logger = 'log')
    stop()
  }
  
  # si todo ok
  row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
  df_train <- df[-row_objetivo, ]
  
  return(df_train)
  
}

# Función para separar train - versión completa
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
  
  return(df_train)
  
}





#####---- FUNCIÓN SEPARA TEST 
# Función para separar test (nuestro objetivo) - versión prueba
# Usando pais_objetivo y year_objetivo como entradas en la llamada
separa_test <- function(df, pais_objetivo, year_objetivo){
  
  if((!all(pais_objetivo %in% df)) || (!all(year_objetivo %in% df))) {
    print("error falta país o año")
    #logerror("Algún parámetro pedido no está en el df", logger = 'log')
    stop()
  }
  row_objetivo <- which((df[ ,1] == pais_objetivo) & df[ ,2] == year_objetivo)
  df_test <- df[row_objetivo, ]
  
  return(df_test)
}

# Función para separar test (nuestro objetivo) - versión completa
# pais_objetivo = config$pais_objetivo
# year_objetivo = config$year_objetivo
separa_test <- function(df, config){
  
  # parar si falta país o año objetivo
  if((!all(config$pais_objetivo %in% df)) || (!all(config$year_objetivo %in% df))){
    print("error falta país o año")
    #logerror("Algún parámetro pedido no está en el df", logger = 'log')
    stop()
  }
  # si todo ok
  row_objetivo <- which((df[ ,1] == config$pais_objetivo) & df[ ,2] == config$year_objetivo)
  df_test <- df[row_objetivo, ]
  
  return(df_test)
  
}






# compruebo que funcionan -- NOTA PARA EL COMPLETO --
# en el bueno, no hace falta indicar Alaska y 365 en la llamada, 
# ya lo cogemos el config dentro de la función
# df_train <- separa_train(df)
# df_test <- separa_test(df)

df_train <- separa_train(df,"Alaska",365)  
df_train

df_test <- separa_test(df,"Alaska",365)
df_test


######   MODELO

# Multiple Linear Regression Example (y, target, xs las features)
fit <- lm(y ~ x1 + x2 + x3, data=mydata)


# predicción
newdata = data.frame(x1 = x1Vector, x2 = x2Vector, x3 = x3Vector)
predictions = predict.lm(model, newdata)

