
library(reshape2)
#Filtramos  CSV

paises <- list('Austria', 'Belgium' , 'Bulgaria', 'Croatia', 
               'Czech Republic', 'Denmark', 'Estonia', 'Finland',
               'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 
               'Latvia', 'Lithuania', 'Luxemburg', 'Malta', 'Moldova', "Netherlands", 'North Macedonia', 
               'Norway', 'Poland', 'Portugal', "Romania", 'Serbia', 'Slovak Republic', "Slovenia", 
               'Spain', 'Sweden', 'Switzerland', 'United Kingdom')

years <- list('country', '1998', '1999', '2000', '2001', '2002',
              '2003', '2004', '2005', '2006', '2007')

#Filtramos csv de Fem.

fem_particip_filtrado <- subset(fem_particip, country %in% paises)
fem_particip_filtrado <- select(fem_particip_filtrado, 'country', '1998', '1999', '2000', '2001', '2002',
       '2003', '2004', '2005', '2006', '2007')

fem_particip_filtrado<- melt(data = fem_particip_filtrado, id.vars = c("country"), measure.vars = c('1998', '1999', '2000', '2001', '2002',
                                                                            '2003', '2004', '2005', '2006', '2007'))

colnames(fem_particip_filtrado) <- c("country", "year", "fem_particip")

#Filtranos csv de GDP.

gdp_filtrado <- subset(gdp, country %in% paises)
gdp_filtrado  <- select(gdp_filtrado , 'country', '1998', '1999', '2000', '2001', '2002',
                                '2003', '2004', '2005', '2006', '2007')
gdp_filtrado<- melt(data = gdp_filtrado, id.vars = c("country"), measure.vars = c('1998', '1999', '2000', '2001', '2002',
  
                                                                                  
colnames(gdp_filtrado) <- c("country", "year", "gdp")                                                                                                                                                                        '2003', '2004', '2005', '2006', '2007'))

#Filtramos CSV de life_exp.

life_exp_filtrado <-subset(life_exp, country %in% paises)
life_exp_filtrado <- select(life_exp_filtrado, 'country', '1998', '1999', '2000', '2001', '2002',
                                '2003', '2004', '2005', '2006', '2007')

life_exp_filtrado <- melt(data = life_exp_filtrado , id.vars = c("country"), measure.vars = c('1998', '1999', '2000', '2001', '2002',
                                                                                  '2003', '2004', '2005', '2006', '2007'))
colnames(life_exp_filtrado) <- c("country", "year", "life_expetancy")    



#Hacemos un melt para unificar lo datos 

prueba2 <- merge(x = fem_particip_filtrado, y = gdp_filtrado, by = c("country", "year"))

df <-merge(x = prueba2, y = life_exp_filtrado, by = c("country", "year"))

df_total <- merge(x = df, y = murdered_filtrado, by = c("country", "year"))



#Guardo el documento 

library(readr)

write_csv(df, "C:\\Users\\BORJA\\OneDrive\\Escritorio\\R\\datos_trabajo_R\\df.csv")

write_csv(df_total, "C:\\Users\\BORJA\\OneDrive\\Escritorio\\R\\datos_trabajo_R\\df_total.csv")