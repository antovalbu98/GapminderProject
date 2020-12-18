# de la linea  4 a la 116 es redundante, pues eso ya lo hace la funcion
#de lectura de datos de Borja. Mi funcion esta en la linea 117


#---- paises y anos que queremos ----


paises <- list('Austria', 'Belgium' , 'Bulgaria', 'Croatia', 
               'Czech Republic', 'Denmark', 'Estonia', 'Finland',
               'France', 'Georgia', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 
               'Latvia', 'Lithuania', 'Luxemburg', 'Malta', 'Moldova', "Netherlands", 'North Macedonia', 
               'Norway', 'Poland', 'Portugal', "Romania", 'Serbia', 'Slovak Republic', "Slovenia", 
               'Spain', 'Sweden', 'Switzerland', 'United Kingdom')



years <- list('country', '1998', '1999', '2000', '2001', '2002',
              '2003', '2004', '2005', '2006', '2007')


###--- Leo gdp ----

gdp <- read.csv("./data/Features/gdp.csv", check.names = F)

# me quedo con paises
gdp_filtrado <- subset(gdp, country %in% paises)

# me quedo con anos
gdp_filtrado <- gdp_filtrado[ ,which((names(gdp_filtrado) %in% years)==TRUE)]

# melt - gdp definitivo

gdp_definitivo <- melt(data = gdp_filtrado, id.vars = c("country"), 
                       measure_vars = c('1998', '1999', '2000', '2001', '2002',
                                        '2003', '2004', '2005', '2006', '2007'),
                       variable.name = "year",
                       value.name = "gdp_capita"
)


###--- Leo life expectancy ----

life <- read.csv("./data/Features/life_exp.csv", check.names = F)

# me quedo con paises
life_filtrado <- subset(life, country %in% paises)

# me quedo con anos
life_filtrado <- life_filtrado[ ,which((names(life_filtrado) %in% years)==TRUE)]

# melt - gdp definitivo

life_definitivo <- melt(data = life_filtrado, id.vars = c("country"), 
                        measure_vars = c('1998', '1999', '2000', '2001', '2002',
                                         '2003', '2004', '2005', '2006', '2007'),
                        variable.name = "year",
                        value.name = "life_exp"
)


###--- Leo laboral ----


laboral <- read.csv("./data/Features/fem_particip.csv", check.names = F)

# me quedo con paises
laboral_filtrado <- subset(laboral, country %in% paises)

# me quedo con anos
laboral_filtrado <- laboral_filtrado[ ,which((names(laboral_filtrado) %in% years)==TRUE)]

# melt - gdp definitivo

laboral_definitivo <- melt(data = laboral_filtrado, id.vars = c("country"), 
                           measure_vars = c('1998', '1999', '2000', '2001', '2002',
                                            '2003', '2004', '2005', '2006', '2007'),
                           variable.name = "year",
                           value.name = "fem_particip"
)



###--- Leo TARGET ----



target <- read.csv("./data/Target/murdered_women.csv", check.names = F)

# me quedo con paises
target_filtrado <- subset(target, country %in% paises)

# me quedo con anos
target_filtrado <- target_filtrado[ ,which((names(target_filtrado) %in% years)==TRUE)]

# melt - TARGET definitivo

target_definitivo <- melt(data = target_filtrado, id.vars = c("country"), 
                          measure_vars = c('1998', '1999', '2000', '2001', '2002',
                                           '2003', '2004', '2005', '2006', '2007'),
                          variable.name = "year",
                          value.name = "fem_murders/100k"
)



####------Hacemos un MERGE para unificar lo datos-----

merge1 <- merge(x = laboral_definitivo, y = gdp_definitivo, 
                by = c("country", "year"))

merge2 <-merge(x = merge1, y = life_definitivo, by = c("country", "year"))

df <- merge(x = merge2, y = target_definitivo, by = c("country", "year"))

###----FUNCION LIMPIEZA DE DATOS----

limpar_datos <- function(dataframe){
  
  df_fem_part_no_missings <- subset(dataframe, (!is.na(dataframe[,3])))
  
  fem_part_mean_country <- aggregate(df_fem_part_no_missings$fem_particip, 
                                     by=list(Country=df_fem_part_no_missings$country), FUN=mean)
  colnames(fem_part_mean_country)[2] <- "mean_fem_part"
  
  df_sin_missings <- dataframe
  
  for (index in 1:length(df_sin_missings$country)) {
    
    if (is.na(df_sin_missings$fem_particip[index]) == T) {
      df_sin_missings$fem_particip[index] <- subset(fem_part_mean_country, 
                                                    Country==df_sin_missings[index,]['country'][[1]])['mean_fem_part'][[1]]
    }
  }
  
  return(df_sin_missings)
  
}

