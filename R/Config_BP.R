
#PRUEBA PARA VER SI SE LEE EL CONFIG, FALTARIA METER LA PARTE DE COMPROBACIÓN.
leerConfig <- function(){
  
  library(XML)
  
  
  configPath <- paste0("config/config_BP.xml")
  
  
  tryCatch(expr = {
    
    #Leer el xml y convertirlo a lista
    config <- XML::xmlToList(xmlParse(configPath))
    
    
  }, error = function(e){
    
    logerror("Config no encontrado en su ruta. Verifica que se llame config.xml",
             logger = 'log')
    stop()
  })
  
}
  
 