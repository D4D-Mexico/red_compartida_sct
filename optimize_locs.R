library('foreign')
library('stringi')
library('sp')
library('geosphere')
locs <- read.dbf('/Users/iorch/Downloads/ITER_NALDBF10.dbf',as.is = TRUE)
varnames <- c('NOM_ENT','ENTIDAD','NOM_MUN','MUN','NOM_LOC','LOC','LONGITUD','LATITUD','POBTOT')
locs <- locs[,varnames]
pueblos_magicos <- read.table('/Users/iorch/Downloads/pm.csv',sep=',',header=T)
nom_loc <- locs$NOM_LOC
nom_mun <- locs$NOM_MUN
nom_ent <- locs$NOM_ENT
locs$NOM_LOC <- iconv(nom_loc, "latin1", "UTF-8")
locs$NOM_MUN <- iconv(nom_mun, "latin1", "UTF-8")
locs$NOM_ENT <- iconv(nom_ent, "latin1", "UTF-8")
locs$POBTOT <- as.numeric(locs$POBTOT)
pob_tot <- as.numeric(locs[locs$NOM_LOC=="Total nacional","POBTOT"])

pob_cov <- 0
pob_cov_men <- 0

mayores <- locs[as.numeric(locs$POBTOT) >= 1e4 & locs$MUN != '000' & locs$LOC != '0000',]
menores <- locs[as.numeric(locs$POBTOT) <  1e4 & locs$MUN != '000' & locs$LOC != '0000',]
total_mayores <- sum(as.numeric(mayores$POBTOT))
top_mayores <- sort(as.numeric(mayores$POBTOT),decreasing=T)[12]/total_mayores
total_menores <- sum(as.numeric(menores$POBTOT))
top_menores <- sort(as.numeric(menores$POBTOT),decreasing=T)[12]/total_menores

my_list_mayores <- c()
while (pob_cov < 0.30 * 0.85){
  my_loc <- sample(row.names(mayores),1)[1]
  if (is.element(my_loc, my_list_mayores)){
    next
  }
  pob_my_loc <- as.numeric(mayores[my_loc,'POBTOT'])
  prob <- as.numeric(runif(1,min=0,top_mayores)[1])
  if ( pob_my_loc >= total_mayores * prob){
    my_list_mayores <- c(my_list_mayores,my_loc)
    pob_cov = pob_cov + pob_my_loc / pob_tot
    mayores <- mayores[row.names(mayores)!=my_loc,]
  }
}
my_list_pueblos_magicos <- c()
my_list_menores <- c()
pueblos_magicos <- pueblos_magicos[!is.na(pueblos_magicos$Clave.INEGI),]
selected_pm <- pueblos_magicos[sample(row.names(pueblos_magicos),ceiling(0.25*nrow(pueblos_magicos))),]
for (i in row.names(selected_pm)){
  pm <- selected_pm[i,]
  my_pm <- menores[as.numeric(locs$ENTIDAD)==pm$Clave.entidad & 
          as.numeric(locs$MUN)==pm$Clave.municipio &
          as.numeric(locs$LOC)==pm$Clave.localidad,]
  if (is.na(my_pm$POBTOT) || length(my_pm$POBTOT)==0){
    next
  }
  my_list_pueblos_magicos <- c(my_list_pueblos_magicos,row.names(my_pm))
  #if (as.numeric(my_pm$POBTOT) >= 1e4){
  #  my_list_mayores <- c(my_list_mayores,row.names(my_pm))
  #  next
  #}
  #pob_cov_men <- pob_cov_men + as.numeric(my_pm$POBTOT) / pob_tot
  #my_list_menores <- c(my_list_menores,row.names(my_pm))
}
df_mayores <- locs[my_list_mayores,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
pob_order <- order(df_mayores$POBTOT,decreasing = T)
df_mayores <- df_mayores[pob_order,]
df_pueblos_magicos <- locs[my_list_pueblos_magicos,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
menores100 <- menores[as.numeric(menores$POBTOT)> 5000,]
while (pob_cov_men < 0.30 * 0.15){
  my_loc <- sample(row.names(menores100),1)[1]
  if (is.element(my_loc, my_list_menores)){
    next
  }
  pob_my_loc <- as.numeric(menores100[my_loc,'POBTOT'])
  prob <- as.numeric(runif(1,min=0,top_menores)[1])
  if ( pob_my_loc >= total_menores * prob){
    my_list_menores <- c(my_list_menores,my_loc)
    pob_cov_men = pob_cov_men + pob_my_loc / pob_tot
    menores <- menores[row.names(menores)!=my_loc,]
    menores100 <- menores100[row.names(menores100)!=my_loc,]
  }
}
df_menores <- locs[my_list_menores,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
pob_order <- order(df_menores$POBTOT,decreasing = T)
df_menores <- df_menores[pob_order,]

net <- c()
net_length <- 0

for (row in row.names(df_mayores)){
  lat_str <- df_mayores[row,'LATITUD']
  lon_str <- df_mayores[row,'LONGITUD']
  lat_sec <- stri_sub(lat_str,-2,-1)
  lat_min <- stri_sub(lat_str,-4,-3)
  lat_deg <- stri_sub(lat_str,1,-5)
  lon_sec <- stri_sub(lon_str,-2,-1)
  lon_min <- stri_sub(lon_str,-4,-3)
  lon_deg <- stri_sub(lon_str,1,-5)
  lat <- char2dms(paste(lat_deg,'d',lat_min,'\'',lat_sec,'"',sep=''))
  lon <- char2dms(paste(lon_deg,'d',lon_min,'\'',lon_sec,'"',sep=''))
  p <- cbind(-as.numeric.DMS(lon),as.numeric.DMS(lat))
  if ( length(net) > 0) {
    net_length <- net_length +  min(distHaversine(net,p))/1000
  }
  net <- rbind(net,p)
}
for (row in row.names(df_pueblos_magicos)){
  lat_str <- df_pueblos_magicos[row,'LATITUD']
  lon_str <- df_pueblos_magicos[row,'LONGITUD']
  lat_sec <- stri_sub(lat_str,-2,-1)
  lat_min <- stri_sub(lat_str,-4,-3)
  lat_deg <- stri_sub(lat_str,1,-5)
  lon_sec <- stri_sub(lon_str,-2,-1)
  lon_min <- stri_sub(lon_str,-4,-3)
  lon_deg <- stri_sub(lon_str,1,-5)
  lat <- char2dms(paste(lat_deg,'d',lat_min,'\'',lat_sec,'"',sep=''))
  lon <- char2dms(paste(lon_deg,'d',lon_min,'\'',lon_sec,'"',sep=''))
  p <- cbind(-as.numeric.DMS(lon),as.numeric.DMS(lat))
  if ( length(net) > 0) {
    net_length <- net_length +  min(distHaversine(net,p))/1000
  }
  net <- rbind(net,p)
}
for (row in row.names(df_menores)){
  lat_str <- df_menores[row,'LATITUD']
  lon_str <- df_menores[row,'LONGITUD']
  lat_sec <- stri_sub(lat_str,-2,-1)
  lat_min <- stri_sub(lat_str,-4,-3)
  lat_deg <- stri_sub(lat_str,1,-5)
  lon_sec <- stri_sub(lon_str,-2,-1)
  lon_min <- stri_sub(lon_str,-4,-3)
  lon_deg <- stri_sub(lon_str,1,-5)
  lat <- char2dms(paste(lat_deg,'d',lat_min,'\'',lat_sec,'"',sep=''))
  lon <- char2dms(paste(lon_deg,'d',lon_min,'\'',lon_sec,'"',sep=''))
  p <- cbind(-as.numeric.DMS(lon),as.numeric.DMS(lat))
  if ( length(net) > 0) {
    net_length <- net_length +  min(distHaversine(net,p))/1000
  }
  net <- rbind(net,p)
}
print(net_length)