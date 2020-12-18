
library(readr)
library(reshape2)
require(dplyr)

leerData <- function(){
  
 print('Leyendo data')
  
 file_list <- list.files("data/Features")
 
 #print(file_list) 
 # fem_particip <- read_csv("data/Features/fem_particip.csv")
 # View(fem_particip)
 
 datas <- list()
 
 for (i in 1:length(file_list)) {

   file <- paste0("data/Features/",file_list[i])
   datas[[i]] <- read.csv(file,header = TRUE, check.names = FALSE)

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
 
   nom_col <- unlist(strsplit(nombreArchivo, split='_', fixed=TRUE))[2]
 
                     
   colnames(data_filtrado) <- c("country", "year", nom_col)
   
   #print(data_filtrado)
   
   
   return(data_filtrado)
}



