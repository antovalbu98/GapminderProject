#FUNCION LECTURA AUTOMATICA R 

#Directorio relativo, busca en el entorno de trabajo  y no en una carpeta del ordenador.

#directorio <- "~/BOOTCAMPS/18112019/clasificarContactos/"

#setwd(directorio)


 # Pongo como working directory el del proyecto

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)

imputadorMissing()


