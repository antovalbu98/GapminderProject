library(logging)

imputadorMissing <- function() {
  
  tryCatch(expr = {
  
    
    
    loginfo("Empezamos la app...", logger = 'log')
  
    datas <- leerData()
    
    loginfo("Se han leido los datos", logger = 'log')
    
   
     
    
    
    
    loginfo("Empezamos la transformaccion de los datos", logger = 'log')
    
     dftotal<-CreacionDataFrame(datas)
    
    loginfo("Se ha creado df completo ", logger = 'log')
    
    
    loginfo("Empezamos la limpieza  de los datos", logger = 'log')
    
    dffinal<-limpar_datos(dftotal)
    
    loginfo("Los datos ya estan limpios ", logger = 'log')
    
    
    
    
    
    
  }, error = function(e){
    
    print(e)
    logerror("La aplicacion ha petado...", logger = 'log')
    stop()
    
  },finally = {
    
    loginfo("Fin de la ejecucion.", logger = 'log')
    removeHandler(writeToFile, logger = 'log')
    
  })
}