
library(readr)
library(reshape2)
require(dplyr)

#Probar lo de config 
leerData <- function(config){
  
 print('Leyendo data')


 tryCatch(expr = {
      
 file_list <- list.files( config$input$name)
 
 #print(file_list) 
 # fem_particip <- read_csv("data/Features/fem_particip.csv")
 # View(fem_particip)
 
 datas <- list()
 
 for (i in 1:length(file_list)) {

   file <- paste0("data/Features/",file_list[i])
   datas[[i]] <- read.csv(file,header = TRUE, check.names = FALSE)

 }

    
 }, error = function(e){
    
    logerror("Datos no encontrado en su ruta. Verifica el directorio de data y el config",
             logger = 'log')
    stop()
 })
 
 if(length(datas) == 0){
    
    logerror("Datos mal leido, verifica que tengan un buen formato. ",
             logger = 'log')
    stop()
    
 }
 
 return(datas)
 
}


filtrarDataFrame <- function(dataFrame, nombreArchivo) {
   
   paises <- list('Austria', 'Belgium' , 'Bulgaria', 'Croatia', 
                  'Czech Republic', 'Denmark', 'Estonia', 'Finland',
                  'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 
                  'Latvia', 'Lithuania', 'Luxemburg', 'Malta', 'Moldova', "Netherlands", 'North Macedonia', 
                  'Norway', 'Poland', 'Portugal', "Romania", 'Serbia', 'Slovak Republic', "Slovenia", 
                  'Spain', 'Sweden', 'Switzerland', 'United Kingdom')
   
   years <- list('country', '1998', '1999', '2000', '2001', '2002',
                 '2003', '2004', '2005', '2006', '2007')
   
   #print(colnames(dataFrame))
  # print(datas)
   #Filtramos csv de Fem.
   
   data_filtrado <- subset(dataFrame, country %in% paises)
   data_filtrado <- select(data_filtrado, 'country', '1998', '1999', '2000', '2001', '2002',
                                   '2003', '2004', '2005', '2006', '2007')
   
   data_filtrado<- reshape2::melt(data = data_filtrado, id.vars = c("country"), measure.vars = c('1998', '1999', '2000', '2001', '2002',
                                                                                                       '2003', '2004', '2005', '2006', '2007'))
 
   
 
                     
   colnames(data_filtrado) <- c("country", "year", nombreArchivo)
   
   #print(data_filtrado)
   
   
   return(data_filtrado)
}

CreacionDataFrame <-function(ListaDataframe){ 

nombreArchivo <- list.files("data/Features")

#print(length(datas))

dfTotal <- data.frame()

for (i in 1:length(ListaDataframe)) {
   dataFrameFiltrado <- filtrarDataFrame(ListaDataframe[[i]] , nombreArchivo[i])
   if (i == 1) {
      dfTotal <- dataFrameFiltrado
   }else
   {
      dfTotal <- merge(dfTotal, dataFrameFiltrado, by = c("country", "year"))
   }
   
   
}

return(dfTotal)
}




