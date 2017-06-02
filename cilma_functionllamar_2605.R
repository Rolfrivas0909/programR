nc=nc_open("physics.nc")
sst=ncvar_get(nc,"sst")
sss=ncvar_get(nc,"sss")
lat=ncvar_get(nc,"lat")
lon=ncvar_get(nc,"lon")
time=ncvar_get(nc,"time")

meses=rep(1:12,length=length(time))

clim=array(dim=c(dim(sst)[1:2],12))

for(i in 1:12){
  clim[,,i]=apply(sst[,,meses=i],
                  MARGIN = c(1,2),
                  FUN=mean,na.rm=TRUE)
}

par(mfrow=c(3,4),mar=c(4,4,1,1))
for(i in 1:12){
  image.plot(lon,lat,clim[,,i]) #enero cim[,,i]indica que se haga un mapa de cada i
}


calculateClim=function(sst,time){#valores que colocar sst y time
  
  meses=rep(1:12,length=length(time))
  
  clim=array(dim=c(dim(sst)[1:2],12))
  
  for(i in 1:12){
    clim[,,i]=apply(sst[,,meses=i],
                    MARGIN = c(1,2),
                    FUN=mean,na.rm=TRUE)
  }
  
  return(clim)
  
}

clim2=calculateClim(sst,time)
identical(clim,clim2) #nos indica si son identicos, y da valores de verdad

plotclim1=par(mfrow=c(3,4),mar=c(4,4,1,1))
for(i in 1:12){
  image.plot(lon,lat,clim2[,,i]) #enero cim[,,i]indica que se haga un mapa de cada i
}

climgraf12=function(lon,lat,clim){
  par(mfrow=c(3,4),mar=c(4,4,1,1))
  for(i in 1:12){
    image.plot(lon,lat,clim[,,i]) #enero clim[,,i]indica que se haga un mapa de cada i
  }
  
  return(invisible())
}

climgraf12(lon,lat,clim)
identical(plotclim1,climgraf12)

grafmensual=climgraf12(clim)  


climsss=calculateClim(sss,time)
climgraf12(lon,lat,sss)

#validación
#probar climgraf12 antes de ejecutar la validación
climgraf12(lon,lat,sst)
climgraf12(lon,lat,sst[,,1:8])

climgraf12=function(lon,lat,clim){
 
   #if se usa para validar y excluir valores no adecuados
  
  if(dim(clim)[3]!=12){
    stop("clim debe tener 12 pasos de tiempo")
  }
  
  par(mfrow=c(3,4),mar=c(4,4,1,1))
  
  for(i in 1:12){
    image.plot(lon,lat,clim[,,i],...) #enero clim[,,i]indica que 
    #se haga un mapa de cada i
    #los ... nos permite agregarle variables del image.plot de lo contrario
    #no nos dejaría verificar
  }
  
  return(invisible())
  #return es obligatorio para que se guarde la información que
  #después llamaremos
}

#volver a probar para ver que da el error o advertencia que consideremos conveniente
climgraf12(lon,lat,sst)
climgraf12(lon,lat,sst[,,1:8])

#validar calculate clim
#datos
#pedir con que función actuar por ejemplo
#"usar media parte FUN=mean también puede ser FUN=median"
#"na.rm es remover los NA, se puede querer modificar entonces
#incluir en la function"

#intento
calculateClim=function(sst,time){#valores que colocar sst y time
  
  
  
  meses=rep(1:12,length=length(time))
  
  clim=array(dim=c(dim(sst)[1:2],12))
  
  if(dim(clim)[3]!=length(time)){
    stop("no tiene clim las mismas dimensiones de time")}
  
  for(i in 1:12){
    clim[,,i]=apply(sst[,,meses=i],
                    MARGIN = c(1,2),
                    FUN=mean,na.rm=TRUE)
  }
  
  return(clim)
  
}






