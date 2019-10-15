## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics('landuse.png')


## ------------------------------------------------------------------------
library(tidyverse) # although not needed for working with sf!


## ------------------------------------------------------------------------
library(sf)


## ----echo=FALSE----------------------------------------------------------
knitr::include_graphics('sf_deps.png')


## ------------------------------------------------------------------------
p1 = st_point(c(3,5))
class(p1)
p2 = st_point(c(4,6))
p3 = st_point(c(4,4))
pts = st_sfc(p1, p2, p3)
class(pts)
sf = st_sf(a = c(3,2.5,4), b = c(1,2,4), geom = pts)
class(sf)
sf = st_as_sf(tibble::tibble(a = c(3,2.5,4), b = c(1,2,4), geom = pts))
class(sf)
sf
plot(sf, cex = 3, pch = 16, key.pos = 1, breaks = seq(.5,4.5,1), pal = sf.colors(4))


## ------------------------------------------------------------------------
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf")) # read as sf-tibble
agr = c(AREA = "aggregate", PERIMETER = "aggregate", CNTY_ = "identity",
  CNTY_ID = "identity", NAME = "identity", FIPS = "identity", FIPSNO = "identity",
  CRESS_ID = "identity", BIR74 = "aggregate", SID74 = "aggregate", NWBIR74 = "aggregate",
  BIR79 = "aggregate", SID79 = "aggregate", NWBIR79  = "aggregate")
st_agr(nc) = agr 
nc[c(9:11,15)]


## ------------------------------------------------------------------------
pt = st_as_sfc("POINT (-78.25073 34.07663)")
st_intersection(nc, st_sfc(pt, crs = st_crs(nc)))


## ------------------------------------------------------------------------
i = st_intersection(nc["CNTY_"], st_sfc(pt, crs = st_crs(nc)))


## ------------------------------------------------------------------------
nc1 = st_transform(nc, 2264) # NC state plain, US feet
pt1 = st_transform(st_sfc(pt, crs = st_crs(nc)), 2264)
i1 = st_intersection(nc1["CNTY_"], pt1)


## ------------------------------------------------------------------------
plot(nc[c("SID74", "SID79")], key.pos = 4)


## ------------------------------------------------------------------------
nc %>% select(SID74, SID79) %>% gather(VAR, SID, -geom) -> nc2 # stack two columns
ggplot() + geom_sf(data = nc2, aes(fill = SID)) + facet_wrap(~VAR, ncol = 1) +
  scale_y_continuous(breaks = 34:36) +
  scale_fill_gradientn(colors = sf.colors(12)) 


## ------------------------------------------------------------------------
(dt = Sys.Date())
class(dt)
(tm = Sys.time())
class(tm)


## ------------------------------------------------------------------------
data(air, package = "spacetime") # requires spacetime to be installed
sel = 1800:2300
library(xts)
a.xts = xts(t(air)[sel,], order.by = dates[sel])


## ------------------------------------------------------------------------
xts::.parseISO8601("2002-03")


## ------------------------------------------------------------------------
plot(a.xts["2003-01/2003-06",1:4], main = "PM10, four rural background stations, DE")


## ------------------------------------------------------------------------
image(dates[sel], 1:70, as.matrix(a.xts), ylab = "station")


## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(stars))
suppressPackageStartupMessages(library(ggplot2))
x = 1:5
y = 1:4
d = st_dimensions(x = x, y = y, .raster = c("x", "y"))
m = matrix(runif(20),5,4)
r1 = st_as_stars(r = m, dimensions = d)

r = attr(d, "raster")
r$affine = c(0.2, -0.2)
attr(d, "raster") = r
r2 = st_as_stars(r = m, dimensions = d)

r = attr(d, "raster")
r$affine = c(0.1, -0.3)
attr(d, "raster") = r
r3 = st_as_stars(r = m, dimensions = d)

x = c(1, 2, 3.5, 5, 6)
y = c(1, 1.5, 3, 3.5)
d = st_dimensions(x = x, y = y, .raster = c("x", "y"))
r4 = st_as_stars(r = m, dimensions = d)

grd = st_make_grid(cellsize = c(10,10), offset = c(-130,10), n= c(8,5), crs=st_crs(4326))
r5 = st_transform(grd, "+proj=laea +lon_0=-70 +lat_0=35")

par(mfrow = c(2,3))
r1 = st_make_grid(cellsize = c(1,1), n = c(5,4), offset = c(0,0))
plot(r1, main = "regular")
plot(st_geometry(st_as_sf(r2)), main = "rotated")
plot(st_geometry(st_as_sf(r3)), main = "sheared")
plot(st_geometry(st_as_sf(r4, as_points = FALSE)), main = "rectilinear")
plot(st_geometry((r5)), main = "curvilinear")


## ------------------------------------------------------------------------
library(stars) # will load sf
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
plot(x)


## ----eval=FALSE----------------------------------------------------------
## install.packages("starsdata", repos = "http://gis-bigdata.uni-muenster.de/pebesma" , type = "source")
## library(starsdata)


## ------------------------------------------------------------------------
have_starsdata = require("starsdata") # TRUE to do the big data example with pkg starsdata; instructions for installing below








## ----eval=FALSE----------------------------------------------------------
## write_stars(s2.ndvi, "s2.tif")


## ------------------------------------------------------------------------
a = air[,sel]
dim(a)
library(units)
(a.st = st_as_stars(list(PM10 = set_units(air[,sel], ppm))))


## ------------------------------------------------------------------------
crs = 32632 # UTM zone 32N
a.st %>% 
  st_set_dimensions(1, values = st_as_sfc(stations)) %>% 
  st_set_dimensions(2, values = dates[sel]) %>% 
  st_transform(crs) -> a.st2
a.st2
st_bbox(a.st2)
st_crs(a.st2)


## ----out.width='100%'----------------------------------------------------
knitr::include_graphics('cube1.png')


## ----out.width='100%'----------------------------------------------------
knitr::include_graphics('cube2.png')


## ----out.width='100%'----------------------------------------------------
knitr::include_graphics('cube3.png')


## ----out.width='100%'----------------------------------------------------
knitr::include_graphics('cube4.png')


## ----eval=FALSE----------------------------------------------------------
## remotes::install_github("r-spatial/stars")


## ------------------------------------------------------------------------
DE_NUTS1 %>% st_as_sfc() %>% st_transform(crs) -> de # DE_NUTS1 is part of the "air" datasets
grd = st_as_stars(de)
grd[[1]][grd[[1]] == 0] = NA
plot(grd, axes = TRUE)


## ------------------------------------------------------------------------
library(gstat)
st_apply(a.st2, "sfc", mean, na.rm = TRUE) %>% 
	st_as_sf() %>%
	na.omit()  -> a.means
v = variogram(mean ~ 1, a.means)
v.fit = fit.variogram(v, vgm(10, "Exp", 1e5, 10))
plot(v, v.fit)


## ------------------------------------------------------------------------
int <- krige(mean~1, a.means, grd, v.fit)
plot(int, reset = FALSE, key.pos = 4, breaks = "pretty")
plot(de, col = NA, border = 'red', add = TRUE)
plot(st_geometry(a.means), col = 'green', add = TRUE, pch = 16)


## ------------------------------------------------------------------------
library(viridis)
g = ggplot() + coord_equal() +
    scale_fill_viridis() +
    theme_void() +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0))
g + geom_stars(data = int) + geom_sf(data = de, fill = NA) + geom_sf(data = a.means)




## ------------------------------------------------------------------------
tif = system.file("tif/L7_ETMs.tif", package = "stars")
read_stars(tif) %>%
  slice(prec, index = 1, along = "band") %>%
  st_warp(crs = "+proj=lcc") %>%
  plot()


## ------------------------------------------------------------------------
prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat")))
## plot(prec) ## gives error about unique breaks:
## remove NAs, zeros, and give a large number
## of breaks (used for validating in detail)
qu_0_omit = function(x, ..., n = 22) {
  x = units::drop_units(na.omit(x))
  c(0, quantile(x[x > 0], seq(0, 1, length.out = n)))
}
library(dplyr) # loads slice generic
prec_slice = slice(prec, index = 17, along = "time")
breaks = qu_0_omit(prec_slice[[1]])
plot(prec_slice, border = NA, breaks = breaks, reset = FALSE)
nc = sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg")
plot(st_geometry(nc), add = TRUE, reset = FALSE, col = NA, border = 'red')


## ----eval=FALSE----------------------------------------------------------
## remotes::install_github("r-spatial/stars")

