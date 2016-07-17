
### as_sp spielereien
library(osmar)
library(sp)

bb <- center_bbox(174.76778, -36.85056, 700,700)
src<- osmsource_api()
ua<- get_osm(bb, source=src)

osm_crs()
a<-as_sp(ua,"points")
uasplines<- as_sp(ua, "lines")
uaspoly <- as_sp(ua, "polygons")
rel_bus<- subset(ua, ids=find_down(ua,relation(
                                      find(ua, relation(tags(v=="bus"))))
                                      ))
buslines<- as_sp(rel_bus, "lines")
buspoints <- as_sp(rel_bus, "points")
plot(uaspoly, axes=TRUE)
plot(buslines, add=TRUE, col=7, lwd=2)
plot(buspoints, add=TRUE, col=2, cex=0.5)

nd<- get_osm(node(18961430), source=src)
sp_nd<-as_sp(nd)

wy<- get_osm(way(3810479), source=src)
wyfull<- get_osm(way(3810479), source=src, full=TRUE)
sp_wy <- as_sp(wy)
sp_wyfull <- as_sp(wyfull)

rl<- get_osm(relation(30023), source=src)
rlfull <- get_osm(relation(30023), source=src, full=TRUE)
sp_rl <- as_sp(rl)
sp_rlfull<- as_sp(rlfull)

routeBus <- find(ua, relation(tags(v=="bus")))
bus_osmar <- list("vector", length(routeBus))
for(i in 1:length(routeBus))
  bus_osmar[[i]] <- get_osm(relation(routeBus[i]), source=src, full=TRUE)
bus_osmarsp <- list("vector", length(routeBus))
for(i in 1:length(routeBus))
  bus_osmarsp[[i]] <- as_sp(bus_osmar[[i]], "lines")

for(i in 1:length(routeBus)){
  plot(bus_osmarsp[[i]], add=TRUE, axes=TRUE, col=i)
  plot(uasplines, add=TRUE, col=2)
  Sys.sleep(1)
}

buil_ids <- find(ua, way(tags(k=="building")))
buil_ids <- find_down(ua, way(buil_ids))
buildings <- subset(ua, ids=buil_ids)
build_spol<- as_sp(buildings, "polygons")
build_spl <- as_sp(buildings, "lines")
par(mfrow=c(1,2))
plot(build_spol, col=2)
plot(build_spl, col=2)



#######

a1 <- as_sp(ua, "points")
str(a1)

a2 <- as_sp(ua, "points", simplify = FALSE)
str(a2)

a3 <- as_sp(ua, what = c("points", "lines"))
str(a3, 1)

a4 <- as_sp(ua)
str(a4, 1)
