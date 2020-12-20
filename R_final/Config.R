
#PRUEBA PARA VER SI SE LEE EL CONFIG, FALTARIA METER LA PARTE DE COMPROBACIÓN.
leerConfig <- function(){
  
  library(XML)
  
  
  configPath <- ("config/Config.xml")
  
  
  tryCatch(expr = {
    
    #Leer el xml y convertirlo a lista
    config <- XML::xmlToList(xmlParse(configPath))
    
    print(config)
  }, error = function(e){
    
    logerror("Config no  se ha encontrado en su ruta. Verifica que se llame config.xml",
             logger = 'log')
    stop()
  })
  #validateConfigNodes(config) # le anado la nueva funcion,
                              # creo que el resto de cosas de la de ander
                              # no aplican
  
  
}
  

####---- Validar config nodes ----

validateConfigNodes <- function(config){
  
  nodoPrincipal <- identical(names(config), c("input", "columnas"))
  nodoInput <- identical(names(config$input), c("name"))
  nodoColumnas <- identical(names(config$columnas), c("pais_objetivo", "year_objetivo"))
  nodos <- c("nodoPrincipal" = nodoPrincipal, "nodoInput" = nodoInput, 
             "nodoColumnas" = nodoColumnas)
  check <- all(nodos)
  if(!check){
    
    nodosMalos <- names(nodos)[!nodos]
    
    logerror(paste0("Los nodos: ", paste(nodosMalos, collapse = ", "),
                    " estan mal estructurados!"), logger = 'log')
    stop()
    
  }
}


 