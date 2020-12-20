
#' @title  Función generarOutput()
#' 
#' @description  Esta función empaqueta en dos ficheros diferentes los resultados de la ejecución
#' del programa. Por un lado, guarda en un .csv los resultados de la predicción.
#' En un fichero rds guarda el modelo entrenado.
#'
#' @param modelo 
#' @param prediccion 
#'
#' @return prediccion.csv    modelo.rds

generarOutput <- function(modelo, prediccion){
  
  marcaTmp <- Sys.time()
  
  nombreArchivo <- "output/prediccion.csv"
  
  tryCatch(expr = {
    
    write.csv(prediccion, file = nombreArchivo, row.names = FALSE)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado de la predicción!!", logger = 'log')
    stop()
  })
  
  
  nombreArchivo <- "output/modelo.rds"
  
  tryCatch(expr = {
    
    saveRDS(modelo, file = nombreArchivo)
    
  }, error = function(e){
    
    logerror("Ha fallado el guardado del modelo!!", logger = 'log')
    stop()
  })
  
  
}