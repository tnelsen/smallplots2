#' plotextract2
#'
#' This function uses a pre-defined shapefile to extract data from a gerefernced
#' tiff file.
#' @keywords UAS
#' @param datapath File path to smallplots_data or equivilant file structure.
#' @param shapefile file path to the shapefile generated with plotshapes or other shapefile
#' @extraction_tiff .tif image
#' @filename output file name
#' @export
#' @examples
#' plotextract2()

plotextract2 <- function(datapath, shapefile, extraction_tiff, filename){
  # Read in the previously created shapefile.
  shape <- rgdal::readOGR(dsn = shapefile)

  # Transform the shapefile.
  transd <- sp::spTransform(shape, "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Read in raster.
  utiff <- raster::raster(extraction_tiff)

  # Extract data based on buffers
  tiff_img <- raster::crop(utiff, raster::extent(transd))
  raster::plot(tiff_img)
  raster::plot(transd, add = T)

  plot.means <- raster::extract(tiff_img, transd, fun = mean, df = TRUE)
  plot.sd <- raster::extract(tiff_img, transd, fun = sd, df = TRUE)
  plot.pixels <- raster::extract(tiff_img, transd, fun=function(x, ...) length(x), df = TRUE)

  extracted.data <- cbind(plot.means, plot.sd, plot.pixels)
  extracted.data <- extracted.data[,c(1,2,4,6)]

  #identify what band working with
  # Split file names to get dates.
  spl <- strsplit(names(utiff), split = "_")
  df <- t(as.data.frame(spl))

  # Get rid of long row names and rename colums so that we can use them to index.
  rownames(df) <- c()
  df <- df[c(2, 4)]
  df <- as.data.frame(matrix(df, ncol = 2))
  colnames(df)<- c('FileDate','Band')

  # Change df from a matrix to a data frame.
  class(df)
  df$Date <- gsub("l", "/", df$FileDate)
  df$Date <- as.Date(df$Date, "%m/%d/%y")

  ##################################

  names(extracted.data) <- c("id", "plot.mean", "plot.sd", "plot.pixels")
  plot.data <- shape@data
  plot.data$Date <- rep(df$Date, nrow(plot.data))
  plot.data$Band <- rep(df$Band, nrow(plot.data))
  # Merge extracted data with plot data
  merge_data <- base::merge(plot.data, extracted.data, by = "id")

  # Write data to csv.
  utils::write.csv(merge_data, file.path(datapath, filename), row.names = FALSE)
}


