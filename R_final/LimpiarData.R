
limpiar_datos <- function(dataframe){
  
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