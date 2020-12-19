
#PRUEBA PARA VER SI SE LEE EL CONFIG, FALTARIA METER LA PARTE DE COMPROBACIÓN.
leerConfig <- function(){
  
  library(XML)
  
  
  configPath <- ("config/Config_BP.xml")
  
  
  tryCatch(expr = {
    
    #Leer el xml y convertirlo a lista
    config <- XML::xmlToList(xmlParse(configPath))
    
    
  }, error = function(e){
    
    logerror("Config no  se ha encontrado en su ruta. Verifica que se llame config.xml",
             logger = 'log')
    stop()
  })
  
}
  
 