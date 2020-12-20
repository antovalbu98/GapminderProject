#FUNCION LECTURA AUTOMATICA R 

#Directorio relativo, busca en el entorno de trabajo  y no en una carpeta del ordenador.

#directorio <- "~/BOOTCAMPS/18112019/clasificarContactos/"

#setwd(directorio)


 # Pongo como working directory el del proyecto

lapply(paste0("R_final/", list.files(path = "R_final/", recursive = TRUE)), source)

imputadorMissing2()


