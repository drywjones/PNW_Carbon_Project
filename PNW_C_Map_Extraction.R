# Extract map data
library(raster)
library(svDialogs)

## read in raster image - georeferenced in QGIS
raster1<-"D:/Adatafolder/R work files/Rasters/Cohen_80_georef.tif"

## utility function for median / mode values


extract_data_from_map<-function(raster.full.path,# name and full path to .tiff file to use
                     num.boxes,# number of categories shown in map (for categorical data)
                     is.noisey=FALSE,# if pixelation is enough to make distinguishing borders 
                                     # somewhat difficult then set to TRUE.
                     raster.out, # if saving to disk the name of the raster to use - the same path
                                # used in raster.full.path will be used as the location to save to
                                # if no name is entered then raster will not be saved to disk
                     tol=.5,    # tolerance - higher value captures more of the map
                                # but may be more inaccurate.
                    blur.rad=1  # controls the size of the matrix used to blur the image
                                # using the focal command in raster - creates a symmetric
                                # circular matrix with dims = 1+2*r. If not using focal then
                                # 2*blur.rad+1 defines the length of pixels of a square matrix
                                # used for determining median value. 
                    
                    ) {
  
  ## this is a helper function that returns the mode or median of a vector
  getmed.mode <- function(v) {
    v<-v[!is.na(v)]
    a<-median(v)
    if(a%in%c(1:num.boxes)){
      b<-a
    }else{b<-round(a)
    return(b)
    }
  }
  beep <- function(n = 3){
    for(i in seq(n)){
      system("rundll32 user32.dll,MessageBeep -1")
      Sys.sleep(.5)
    }
  }
 if(missing(raster.out)){
   raster.out<-paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(raster.full.path)),"_data")

 } 
  
  layer_name<-substr(gsub("[[:space:]]", "", paste0(raster.out,"_data")),1, 12)# return first twelve characters of original file name with no spaces
  
  # these libraries are used in the function so load them:
require(raster)
require(sp)
require(rgdal)
require(svDialogs)
library(RcppRoll)
  # load image as a raster brick:
  raster.in<-raster::brick(raster.full.path)
  
  # get the directory the raster is coming from
  raster.path<-dirname(raster.full.path)
  
  # set location of raster actions to raster directory
  rasterOptions(tmpdir=raster.path)
  
## check to see if image has noise in it and blur a little if needed.
if(is.noisey==TRUE){
  ## original image may need some blurring:
  ## this is for three layered rasters (RBG)
  ## 
dims<-1+2*blur.rad

## The code here is for using a circular matrix with the focal function
## this appears to not work as well as the roll_median function from RcppRoll pacakge
## but I'm leaving it here in case a greater level of control over the moving
## window matrix is desired
#  x.coords<-1:dims
#  y.coords<-1:dims
#  xys<-expand.grid(xs=x.coords,ys=y.coords)
#  cent.x<-ceiling(dims/2)
#  cent.y<-cent.x
#  xys$distance<-sqrt((xys$xs-cent.x)^2+(xys$ys-cent.y)^2) # calculate distance to center of matrix for all points
#  cm<-matrix(xys$distance,ncol=dims,nrow=dims,byrow =TRUE) #create matrix

# set everything outside of radius to 0:
#  cm[cm>blur.rad]<-NA
# try(
#  for(i in 1:dim(raster.in)[3]){
    
#  raster.in[[i]]<-raster::focal(raster.in[[i]],w=cm,fun=getmed.mode) 
    
#  },silent=TRUE)

## using RcppRoll is faster than focal - but it doesn't work directly on 
## rasters - have to conver the layers to matrix first then convert back to raster
## later - save CRS and extent from input raster first:
crs.rast<-crs(raster.in)
extent.rast<-extent(raster.in)

## create a list to store the raster layers:
raster.list<-list()
try(
  for(i in 1:dim(raster.in)[3]){
    
    suppressWarnings(raster.list[[i]]<-raster::raster(RcppRoll::roll_median(as.matrix(raster.in[[i]]),n=dims,align="center"))) 
    
  },silent=TRUE)
## combine raster layers back into a brick:
raster.in<-raster::brick(raster.list)
crs(raster.in)<-crs.rast
extent(raster.in)<-extent.rast
}
raster2<-raster.in

# number of points, type of points, and parameters ####
## depending on the number of categories you will have to do this
## a few times. Click on the center of the box
labels.list<-list()
values.list<-list()
error.list<-list()
xs.list<-list()
ys.list<-list()
#dev.new("windows",noRStudioGD = TRUE)

windows()
raster::plotRGB(raster2)
year.or.yr.range <- list(dlgInput(paste0("Enter year or year range of the map"),Sys.info()["user"])$res)
flux.or.store<-list(dlgInput(paste0("Is this data flux or storage? Type flux or store into the box."),Sys.info()["user"])$res)
dlg_message("The next step will be to select the center of each category box to associate the color in the category legend with the colors on the map. Click OK when ready to start - look in the console window for promps indicating which category index number you are supposed to click on.",type=c("ok")) 
for(i in 1:num.boxes){
  message(paste0("Select center of category box ",i," in legend"))
  location<-locator(1,type="p",par(pch=16,col="blue"))
  label <- dlgInput(paste0("Enter name of category ",i),Sys.info()["user"])$res
  values <- dlgInput(paste0("Enter mean value of category ",i),Sys.info()["user"])$res
  error<- dlgInput(paste0("Enter the error of category ",i,". For ranges use the word to between values (e.g. -4 to 3), for sd or se just type a single numeric value."),Sys.info()["user"])$res
  labels.list[[i]]<-label
  values.list[[i]]<-values
  error.list[[i]]<-error
  xs.list[[i]]<-location$x
  ys.list[[i]]<-location$y
  if(i == num.boxes){

    units<-dlgInput(paste0("Enter units for categories "),Sys.info()["user"])$res
    err.unit<-dlgInput(paste0("Enter the type of error (range, sd, se, etc.) "),Sys.info()["user"])$res
    units<-rep(units,num.boxes)
    err.unit<-(rep(err.unit,num.boxes))
  }

}

## double check values from output above using a data.frame view:
beep(n=1)
dlg_message("Click OK to verify and/or edit the data table created based on your earlier data entry.",type=c("ok")) 
data.check<-data.frame(errors=c(unlist(error.list)),err.units=c(err.unit),labels=c(unlist(labels.list)),values=c(unlist(values.list)),units=c(units))
data.check<-edit(data.check)

## convert data.frame into separate vectors:
errors<-c(data.check$errors)
labels<-c(data.check$labels)
values<-c(data.check$values)
err.units<-c(data.check$err.units)
locations<-list(x=xs.list,y=ys.list)
points<-list(locations$x,locations$y)
xrange<-abs(extent(raster2)[1]-extent(raster2)[2])
yrange<-abs(extent(raster2)[3]-extent(raster2)[4])

point.samp<-list()
for(j in 1:length(points[[1]])){
  samp.x<-unlist(points[[1]][j])
  samp.y<-unlist(points[[2]][j])
  samp.x.exts<-c(samp.x-3*xrange/dim(raster2)[1],samp.x+3*xrange/dim(raster2)[1])
  samp.x.exts<-c(samp.x.exts[order(samp.x.exts)])
  samp.y.exts<-c(samp.y-3*yrange/dim(raster2)[2],samp.y+3*yrange/dim(raster2)[2])
  samp.y.exts<-c(samp.y.exts[order(samp.y.exts)])
  point.samp[[j]]<-raster::sampleRandom(raster2,30,ext=c(samp.x.exts[1],samp.x.exts[2],
                                                      samp.y.exts[1],samp.y.exts[2]))
}

# get min and max values for each color band for each box:
point.min<-list()
point.max<-list()
point.mean<-list()
for(i in 1:length(point.samp)){
  point.dat<-point.samp[[i]]
  point.min[[i]]<-c(apply(point.dat,MARGIN=c(2),min))
  point.max[[i]]<-c(apply(point.dat,MARGIN=c(2),max))
  point.mean[[i]]<-c(apply(point.dat,MARGIN=c(2),mean))
}

## min max is first entry - 1 is min 2 is max
## second number is list item - which sample point value min max
## third is the number related to the band that was sampled.
min.max<-list((point.min),(point.max))

## plot map and select the polygon that encompasses only the mapped data
## not the legends - background colors could be included in data otherwise:
beep(n=1)
dlg_message("The next step will be to click on each vertex of the boundary of the data from the map to create a selection polygon. Lines will show up between consecutive points. Only the data within the boundary will be used for further processing. Right click and select stop when boundary is complete. Click ok to start the process.",
            type=c("ok"))
raster1.1<-raster::select(raster2, use="pol")
## check that the map looks right:

raster::plotRGB(raster1.1)
## rasters can only contain numbers or factors so save factors as vector here:

factors<-as.factor(labels)

# increase range of min max to capture more of the map:


mins<-lapply(min.max[[1]],function(x) x*(1-tol))
maxs<-lapply(min.max[[2]],function(x) x*(1+tol))
colors<-sapply(point.mean,function(x)rgb(red=x[1],
                                          green=x[2],
                                          blue=x[3],
                                          maxColorValue=255))

## this limits the selected cells (0 for not match 1 for match) to only those
## that are within 50 % of the min or max across the three bands - could do 4
## if intensity varies. NAs will show up from cutting the map area down from
## original raster but don't matter as far as matching goes.

matches<-list()
for(i in 1:(length(points[[1]]))){
  matches[[i]]<-(raster1.1[[1]]>=mins[[i]][[1]]&raster1.1[[1]]<=maxs[[i]][[1]])*
    (raster1.1[[2]]>=mins[[i]][[2]]&raster1.1[[2]]<=maxs[[i]][[2]])* 
    (raster1.1[[3]]>=mins[[i]][[3]]&raster1.1[[3]]<=maxs[[i]][[3]])*i
}
## combine results list into one raster brick:
matches2<-raster::brick(matches)
## replace 0 values (unmatched pixels) with NA
values(matches2)[values(matches2)==0]=NA



## combine layers into one using median value across all layers
## creating a single layer with category numbers as pixel values:
categories<-raster::calc(matches2,fun=getmed.mode)


## fill in some NA values with median of what is around them
## have to use only 1's otherwise it will cause problems:
interp.mat<-matrix(c(1,1,1,
                     1,1,1,
                     1,1,1))

categories2<-raster::focal(categories,w=interp.mat,fun=getmed.mode,NAonly=TRUE)

# plot resulting map with some margins so you can select corners more easily:



# NA values may extend over map - mask the map so that those values are not included:
#categories3<-raster::mask(categories2,raster1.1)


# use this code to manuall adjust new raster in case NA values are outside of original extent:

#raster::plot(categories2)
#beep(n=1)
#dlg_message("Use the cursor to create a new selection polygon that encompasses data excluding values outside of original map. Right click and select stop when done. Click OK to start.",type=c("ok")) 
#categories3<-raster::select(categories2, use="pol")

## create data frame for category metadata
rat.dat<-data.frame(ID=c(1:num.boxes),
                    Labels=c(labels),
                    Value=c(values),
                    Error=c(errors),
                    Err.unit=c(err.units),
                    Yr.range=unlist(year.or.yr.range),
                    Units=c(units))
rat.dat$Value<-suppressWarnings(as.numeric(rat.dat$Value))
names(rat.dat)[match("Value",names(rat.dat))]<-flux.or.store
rat.dat$Value<-rat.dat$ID
categories3<-raster::ratify(categories2,count=TRUE)
## now make value.raster by replacing category values (column 1) with mean values 
## in column 3
## specify attribute table for raster
err.type<-rat.dat$Err.unit[1]
suppressWarnings(try(
if(err.type=="range"||err.type=="Range"||err.type=="ranges"||err.type=="Ranges"){
  ranges<-rat.dat$Error
  new.ranges<-strsplit(ranges,"to")
  ## remove spaces:
  new.ranges2<-lapply(new.ranges,function(x)gsub("[[:space:]]", "", x))
  ## remove commas in case they were used for large values:
  new.ranges3<-lapply(new.ranges2,function(x)gsub(",", "", x))
  err.mins<-sapply(new.ranges3,function(x)min(as.numeric(x)))
  err.maxes<-sapply(new.ranges3,function(x)max(as.numeric(x)))
  rat.dat$Err.min<-err.mins
  rat.dat$Err.max<-err.maxes
  
}
))
levs<-levels(categories3[[1]])
suppressWarnings(lev.dat<-try(merge(levs,rat.dat,by=c("ID"))))

lev.dat$colors<-colors
lev.dat$ID<-lev.dat$ID-1
levels(categories3)[[1]]<-lev.dat
names(categories3)<-layer_name

# use this if you want values from the map instead of categories - 
# advantage of categories is that using the RAT table output you can
# quickly calculate error (max min) and values. 
## final.raster<-raster::subs(categories3,rat.dat,by=1,which=3)

final.raster<-categories3


  out_map_name<-paste0(raster.path,"/",raster.out)
  try(raster::writeRaster(final.raster,out_map_name,format="GTiff",overwrite=TRUE,
                          options=c("PROFILE=GTiff"),
                          datatype="INT4S",RAT=TRUE)
      )
  ## write raster attribute table:
  foreign::write.dbf(levels(final.raster)[[1]], file = paste0(out_map_name,".tif.vat.dbf"))
  ## make color table for display in arcgis
  r<- readGDAL(paste0(out_map_name,".tif"))
  lev.dat$color<- as.character(lev.dat$color)
  lev.dat$Labels<- as.character(lev.dat$Labels)
  suppressWarnings(try(rgdal::writeGDAL(r, paste0(out_map_name,".tif"), type="Byte", 
                             colorTable=list(lev.dat$color), 
                             catNames=list(lev.dat$Labels), mvFlag=11L,
                             setStatistics = TRUE
  )))
  dev.off()
  return(final.raster)



}
raster1<-full.path.to.cohen.raster

fin_rast<-extract_data_from_map(raster1,
                                num.boxes=9,
                                is.noisey = TRUE,
                                tol=0.25,
                                blur.rad=1)








