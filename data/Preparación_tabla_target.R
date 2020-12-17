#TARGET

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


murdered<- murdered_women

murdered_filtrado <- subset(murdered, country %in% paises)
murdered_filtrado  <- select(murdered_filtrado , 'country', '1998', '1999', '2000', '2001', '2002',
                        '2003', '2004', '2005', '2006', '2007')
murdered_filtrado<- melt(data = murdered_filtrado, id.vars = c("country"), measure.vars = c('1998', '1999', '2000', '2001', '2002',
                                                                                  '2003', '2004', '2005', '2006', '2007'))
colnames(murdered_filtrado) <- c("country", "year", "murdered women ")    

#