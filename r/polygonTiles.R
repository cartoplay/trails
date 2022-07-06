library(sp)
library(rgdal)
library(rgeos)


rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}


## methods inspired from https://github.com/qgis/QGIS/blob/master/python/plugins/processing/algs/qgis/TilesXYZ.py

deg2num = function(lonDeg, latDeg, zoom){
	latRad = deg2rad(latDeg)
	n = 2.0^zoom
	xtile = as.integer((lonDeg + 180)/360*n)
	ytile = as.integer((1.0-log(tan(latRad)+(1/cos(latRad)))/pi)/2.0*n)
	return (c(xtile, ytile))
}

num2deg = function(xtile, ytile, zoom){
	n = 2.0^zoom
	lonDeg = xtile / n * 360.0 - 180.0
	latRad = atan(sinh(pi * (1 - 2 * ytile / n)))
	latDeg = rad2deg(latRad)
    return (c(lonDeg,latDeg))
}



makeTileBoundary = function(zoom,xtile,ytile){
		Sr1= Polygon(rbind(num2deg(xtile,ytile, zoom),
		num2deg(xtile+1,ytile, zoom),
		num2deg(xtile+1,ytile+1, zoom),
		num2deg(xtile,ytile+1, zoom),
		num2deg(xtile,ytile, zoom)))
		Srs1 = Polygons(list(Sr1), paste0(xtile,"_",ytile))
		return (Srs1)
}

makeAllBoundaries = function(NW,SE,zooms,elFolder){
	daList = list()
	daNames = c()
	zC = c()
	xC = c()
	yC = c()
	for (zoom in zooms){
	 minTile = deg2num(NW[1],NW[2],zoom)
	 maxTile = deg2num(SE[1],SE[2],zoom)
	 xTiles = c(minTile[1]:maxTile[1])	
	 yTiles = c(minTile[2]:maxTile[2])
	 for (xtile in xTiles){
	 	for (ytile in yTiles){
	 		daList = append(daList, makeTileBoundary(zoom,xtile,ytile))
	 		zC = append(zC,zoom)
	 		xC = append(xC,xtile)
	 		yC = append(yC,ytile)
	 		daNames = append(daNames, paste0(zoom,"_",xtile,"_",ytile))
	 	}
	 }
	}

SpP = SpatialPolygons(daList, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
spdf <- SpatialPolygonsDataFrame(SpP, data=data.frame(zxy=daNames, zoom = zC, xtile = xC, ytile = yC, row.names=row.names(SpP)))
zoomName = paste0("polyTilesZoom",min(zooms),"_",max(zooms))
writeOGR(spdf, elFolder, zoomName, driver="ESRI Shapefile",overwrite_layer = TRUE)
}

elShpFolder = '/Users/javierarce/Documents/gis/trails/routedata/'
elShp = 'disolved_by_route'
shp = readOGR(elShpFolder, elShp)
shp@bbox

NW = shp@bbox[,'min'] # shp@bbox[,1]
SE = shp@bbox[,'max'] # shp@bbox[,2]
zooms = c(12:18)
elFolder = "/Users/javierarce/Documents/dev/trails/r/shape"
makeAllBoundaries(NW,SE,zooms,elFolder)


## to do 
## make the method so it accepts the shapefile 
## make the 
