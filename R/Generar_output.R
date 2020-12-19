# A la función que genera el output le entran como parámetros dinámicos:
# 1. la salida de la función que genera y entrena el modelo (entrenar_modelo())
# 2. la salida de la función que predice sobre el objetivo (predecir_objetivo())


generarOutput <- function(modelo, prediccion, config, path){
  
  marcaTmp <- Sys.time()
  
  nombreArchivo <- paste0(path, "output/murder_prediction.csv")
  
  tryCatch(expr = {
    
    write.csv(prediccion, file = nombreArchivo, sep = ";",
              row.names = FALSE)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado de la predicción!!", logger = 'log')
    stop()
  })
  
  
  nombreArchivo <- paste0(path, "output/modelo.rds")
  
  tryCatch(expr = {
    
    saveRDS(modelo, file = nombreArchivo)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado del modelo!!", logger = 'log')
    stop()
  })
  
  
}