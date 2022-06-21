#' A function to alter the extent of rasters
#'
#' This function shifts the extent of a raster from
#'  -180:180 to 0:360 and vice versa
#'
#' @param ras Raster object to shift the extent
#' @param method Numerical value to indicate direction of
#' extent change. '1' shifts from -180:180 to 0:360. '2' shifts from
#' 0:360 to -180:180.
#' @export
#' @examples
#' ### Create Test data
#' r<-raster()
#' values(r) <- c(rep(1:360,length.out=64800/2),rep(1:360,length.out=64800/2))
#' plot(r)
#' # convert r from -180:180 to 0:360
#' out <- reextent(r, method = 1)
#' plot(out)
#' # convert back to -180:180
#' out2 <- reextent(out, method = 2)
#' plot(out2)
#'
#' @return Shifted raster


reextent <- function(ras,method){
  if(method==1){
    #create extents to cut map into two chunks
    ext1 <- raster::extent(-180, 0, -90, 90)
    ext2 <- raster::extent(0, 180, -90, 90)
    #crop raster
    r1<-raster::crop(ras,ext1)
    r1 <- raster::shift(r1, 360)
    r2<-raster::crop(ras,ext2)
    #merge raster after shift
    rasout <- raster::merge(r1, r2, overlap = FALSE)
    return(rasout)
  }
  if(method==2){
    #create extents to cut map into two chunks
    ext1 <- raster::extent(0, 180, -90, 90)
    ext2 <- raster::extent(180, 360, -90, 90)
    #crop raster
    r1<-raster::crop(ras,ext1)
    r2<-raster::crop(ras,ext2)
    r2 <- raster::shift(r2, -360)
    #merge raster after shift
    out <- raster::merge(r1, r2, overlap = FALSE)
    return(out)
  }
}
