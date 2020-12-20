library(logging)

imputadorMissing2 <- function() {
  
  tryCatch(expr = {
  
    # Función para leer config y valiudar(script config)
    
    addHandler(writeToFile, logger = 'log', file = "log/logfile.log")
    loginfo("Empezamos la app...", logger = 'log')
    
    
    loginfo("Leyendo el config...", logger = 'log')
    
    config <- leerConfig()
    
    loginfo("Config leido.", logger = 'log')
    
    
    loginfo("Comprobando el config... ", logger = 'log')
    
    validateConfigNodes(config)
    
    loginfo("Config comprobado, todo correcto.", logger = 'log')
    
    #Funciones leer datos y crear df(scrip leer datos)
    
    loginfo("Empezamos la leer los datos...", logger = 'log')
  
    datas <- leerData(config)
    
    loginfo("Se han leido los datos correctamente.", logger = 'log')
    
   
     loginfo("Empezamos la transformaccion de los datos...", logger = 'log')
    
     dftotal <-creacionDataFrame(datas, config)
    
    loginfo("Se ha creado dataframe correctamente.", logger = 'log')
    
    
    #Funcion para limpieza de datos (script limpieza de datos)
    
    loginfo("Empezamos la limpieza  de los datos..", logger = 'log')
    
    dffinal <-limpiar_datos(dftotal)
    
    loginfo("Los datos ya estan limpios.", logger = 'log')
    
    # funciones para Generar modelo y predecir.(Script generar modelo)
    

    loginfo("Empezamos a generar el modelo..", logger = 'log')
    
    modelo <-entrenar_modelo(dffinal,config)
    
    loginfo("El modelo se ha generado.  ", logger = 'log')
    
    
    #Generamso aoutput 
    
    loginfo("Empezamos a hacer la prediccion..", logger = 'log')
    
    prediccion <-predecir_objetivo(dffinal, modelo, config)
    
    #print(prediccion)
      
    loginfo("Prediccion hecha.", logger = 'log')
    
    
    loginfo("Generandose Output...", logger = 'log')
    
    output <-generarOutput(modelo, prediccion)
    
    loginfo("Output hechos.", logger = 'log')
    
    
    
    
  }, error = function(e){
    
    print(e)
    logerror("La aplicacion ha petado...", logger = 'log')
    stop()
    
  },finally = {
    
    loginfo("Fin de la ejecucion.", logger = 'log')
    removeHandler(writeToFile, logger = 'log')
    
  })
}