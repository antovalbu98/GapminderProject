library(logging)

imputadorMissing <- function() {
  
  tryCatch(expr = {
  
    loginfo("Empezamos la app...", logger = 'log')
  
    datas <- leerData()
    
    loginfo("Se han leido los datos", logger = 'log')
    
   
     
    
    
    loginfo("Empezamos la transformaccion de los datos", logger = 'log')
    
     
     
     nombreArchivo <- list.files("data/Features")
     
     #print(length(datas))
     
     dfTotal <- data.frame()
     
     for (i in 1:length(datas)) {
       dataFrameFiltrado <- filtrarDataFrame(datas[[i]] , nombreArchivo[i])
       if (i == 1) {
         dfTotal <- dataFrameFiltrado
       }else
       {
         dfTotal <- merge(dfTotal, dataFrameFiltrado, by = c("country", "year"))
       }
       
         
     }
     
     return(dfTotal)
    
    loginfo("Se ha creado df completo ", logger = 'log')
    
    
    
    
    
    
    
    
  }, error = function(e){
    
    print(e)
    logerror("La aplicacion ha petado...", logger = 'log')
    stop()
    
  },finally = {
    
    loginfo("Fin de la ejecucion.", logger = 'log')
    removeHandler(writeToFile, logger = 'log')
    
  })
}