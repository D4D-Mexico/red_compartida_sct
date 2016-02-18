library('foreign')
library('stringi')
library('sp')
library('geosphere')
setwd('~/mxabierto/sct_redc_ompartida/')
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
pob_tot <- as.numeric(locs[locs$NOM_LOC=="Total nacional","POBTOT"])
locs <- locs[locs$MUN != '000' & locs$LOC != '0000',]

locs$POBTOT <- as.numeric(locs$POBTOT)

pob_order <- order(locs$POBTOT,decreasing = T)

locs <- locs[pob_order,]

mayores <- locs[as.numeric(locs$POBTOT) >= 1e4 & locs$MUN != '000' & locs$LOC != '0000',]
menores <- locs[as.numeric(locs$POBTOT) <  1e4 & locs$MUN != '000' & locs$LOC != '0000',]

pob_mayores <- 0
my_list_mayores_30 <- c()
my_list_mayores_50 <- c()
my_list_mayores_70 <- c()
my_list_mayores_85 <- c()
my_list_mayores_90 <- c()

for (row in rownames(mayores)){
  if (pob_mayores > pob_tot * 0.90 * 0.85){
    break
  }
  pob_mayores <- pob_mayores + mayores[row,'POBTOT']
  my_list_mayores_90 <- c(my_list_mayores_90, row)
  mayores <- mayores[row.names(mayores)!=row,]
  if (pob_mayores < pob_tot * 0.85875 * 0.85){
    my_list_mayores_85 <- c(my_list_mayores_85, row)
    if (pob_mayores < pob_tot * 0.70 * 0.85){
      my_list_mayores_70 <- c(my_list_mayores_70, row)
      if (pob_mayores < pob_tot * 0.50 * 0.85){
        my_list_mayores_50 <- c(my_list_mayores_50, row)
        if (pob_mayores < pob_tot * 0.30 * 0.85){
          my_list_mayores_30 <- c(my_list_mayores_30, row)
        }
    }
  }
}
}
pob_menores <- 0
my_list_menores_90 <- c()
my_list_menores_85 <- c()
my_list_menores_70 <- c()
my_list_menores_50 <- c()
my_list_menores_30 <- c()
pueblos_magicos <- pueblos_magicos[!is.na(pueblos_magicos$Clave.INEGI),]
selected_pm_75 <- pueblos_magicos[sample(row.names(pueblos_magicos),ceiling(0.70*nrow(pueblos_magicos))),]
selected_pm_50 <- pueblos_magicos[sample(row.names(selected_pm_75),ceiling(0.50*nrow(selected_pm_75)/0.70)),]
selected_pm_25 <- pueblos_magicos[sample(row.names(selected_pm_50),ceiling(0.25*nrow(selected_pm_50)/0.50)),]
for (i in row.names(pueblos_magicos)){
  pm <- selected_pm_75[i,]
  my_pm <- menores[as.numeric(locs$ENTIDAD)==pm$Clave.entidad & 
                     as.numeric(locs$MUN)==pm$Clave.municipio &
                     as.numeric(locs$LOC)==pm$Clave.localidad,]
  if (is.na(my_pm$POBTOT) || length(my_pm$POBTOT)==0){
    next
  }
  if (as.numeric(my_pm$POBTOT) >= 1e4){
    my_list_mayores_85 <- c(my_list_mayores_85,row.names(my_pm))
    mayores <- mayores[row.names(mayores)!=my_pm,]
      if (is.element(i,selected_pm_75)) {
        my_list_mayores_70 <- c(my_list_mayores_70,row.names(my_pm))
        if (is.element(i,selected_pm_50)) {
          my_list_mayores_50 <- c(my_list_mayores_50,row.names(my_pm))
          if (is.element(i,selected_pm_25)) {
            my_list_mayores_30 <- c(my_list_mayores_30,row.names(my_pm))
          }
        }
      }
    next
  }
  menores <- menores[row.names(menores)!=my_pm,]
  pob_menores <- pob_menores + as.numeric(my_pm$POBTOT)
  my_list_menores_85 <- c(my_list_menores_85,row.names(my_pm))
  if (is.element(i,selected_pm_75)) {
    my_list_menores_70 <- c(my_list_menores_70,row.names(my_pm))
    if (is.element(i,selected_pm_50)) {
      my_list_menores_50 <- c(my_list_menores_50,row.names(my_pm))
      if (is.element(i,selected_pm_25)) {
        my_list_menores_30 <- c(my_list_menores_30,row.names(my_pm))
      }
    }
  }
}
df_mayores_30 <- locs[my_list_mayores_30,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_mayores_50 <- locs[my_list_mayores_50,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_mayores_70 <- locs[my_list_mayores_70,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_mayores_85 <- locs[my_list_mayores_85,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_mayores_90 <- locs[my_list_mayores_90,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
write.csv(locs[my_list_mayores_30,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'mayores0_30.csv',row.names = FALSE)
write.csv(locs[my_list_mayores_50,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'mayores0_50.csv',row.names = FALSE)
write.csv(locs[my_list_mayores_70,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'mayores0_70.csv',row.names = FALSE)
write.csv(locs[my_list_mayores_85,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'mayores0_85.csv',row.names = FALSE)
write.csv(locs[my_list_mayores_90,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'mayores0_90.csv',row.names = FALSE)
for (row in rownames(menores)){
  if (pob_menores > pob_tot * 0.90 * 0.15){
    break
  }
  my_list_menores_90 <- c(my_list_menores_90, row)
  if (pob_menores < pob_tot * 0.85 * 0.15){
    my_list_menores_85 <- c(my_list_menores_85, row)
    if (pob_menores < pob_tot * 0.70 * 0.15){
      my_list_menores_70 <- c(my_list_menores_70, row)
      if (pob_menores < pob_tot * 0.50 * 0.15){
        my_list_menores_50 <- c(my_list_menores_50, row)
        if (pob_menores < pob_tot * 0.30 * 0.15){
          my_list_menores_30 <- c(my_list_menores_30, row)
        }
      }
    }
  }
  pob_menores <- pob_menores + menores[row,'POBTOT']
  menores <- menores[row.names(menores)!=row,]
}

df_menores_30 <- locs[my_list_menores_30,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_menores_50 <- locs[my_list_menores_50,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_menores_70 <- locs[my_list_menores_70,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_menores_85 <- locs[my_list_menores_85,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]
df_menores_90 <- locs[my_list_menores_90,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')]

write.csv(locs[my_list_menores_30,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'menores0_30.csv',row.names = FALSE)
write.csv(locs[my_list_menores_50,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'menores0_50.csv',row.names = FALSE)
write.csv(locs[my_list_menores_70,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'menores0_70.csv',row.names = FALSE)
write.csv(locs[my_list_menores_85,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'menores0_85.csv',row.names = FALSE)
write.csv(locs[my_list_menores_90,c('LOC','MUN','ENTIDAD','NOM_LOC','NOM_MUN','NOM_ENT','POBTOT','LATITUD','LONGITUD')],'menores0_90.csv',row.names = FALSE)


net <- setClass('net', contains = "environment",
                slots = c(length="numeric", points="matrix", update = "POSIXct"),
                prototype = list(length=numeric(0), points=matrix(0,0,0),update <- Sys.time()))

setMethod("[[<-", c("net", "character", "missing"),
                     function(x, i, j, ..., value) {
                           ev <- as(x, "environment")
                           ev[[i]] <- value  #update the object in the environment
                           x@update <- Sys.time() # and the update time
                           x})

add_points <- function(net,df){
  for (row in row.names(df)){
    lat_str <- df[row,'LATITUD']
    lon_str <- df[row,'LONGITUD']
    if (is.na(lat_str) || is.na(lon_str)){
      next
    }
    lat_sec <- stri_sub(lat_str,-2,-1)
    lat_min <- stri_sub(lat_str,-4,-3)
    lat_deg <- stri_sub(lat_str,1,-5)
    lon_sec <- stri_sub(lon_str,-2,-1)
    lon_min <- stri_sub(lon_str,-4,-3)
    lon_deg <- stri_sub(lon_str,1,-5)
    lat <- char2dms(paste(lat_deg,'d',lat_min,'\'',lat_sec,'"',sep=''))
    lon <- char2dms(paste(lon_deg,'d',lon_min,'\'',lon_sec,'"',sep=''))

    p <- cbind(-as.numeric.DMS(lon),as.numeric.DMS(lat))
    if ( length(net$points) > 0) {
      net$length <- net$length + min(distHaversine(net$points,p))/1000.0
      net$points <- rbind(net$points,p)
    } else {
      net$points <- rbind(p)
    }
  }  
  return(net)
}
mynet <- net(length=0,points=rbind(cbind(0,0)),update = Sys.time())
mynet$points <- rbind()
mynet$length <- 0
mynet<-add_points(mynet,df_mayores_30)
mynet<-add_points(mynet,df_menores_30)
print(mynet$length)
mynet<-add_points(mynet,df_mayores_50)
mynet<-add_points(mynet,df_menores_50)
print(mynet$length)
mynet<-add_points(mynet,df_mayores_70)
mynet<-add_points(mynet,df_menores_70)
print(mynet$length)
mynet<-add_points(mynet,df_mayores_85)
mynet<-add_points(mynet,df_menores_85)
print(mynet$length)
mynet<-add_points(mynet,df_mayores_90)
mynet<-add_points(mynet,df_menores_90)
print(mynet$length)
