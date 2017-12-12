#' plotshapes
#'
#' Interactive creation of a shapefile from the centers of research plots.
#' @param datapath path to smallplots_data or equivalent. See README
#' @param base_image path to your base image
#' @param shp_name output name to for the created shapefile
#' @keywords UAS, shapefile
#' @export
#' @examples
#' plotshapes()

plotshapes <- function(datapath, base_image, shp_name){

	# Load in .tif file to use for shapefile creation - red band clearest
	base::invisible(base::writeLines(base::strwrap("> This program goes automatically generates a shapefile in order to extract data from a TIFF image.")))

	# User selection of location - if more than 1 location
	base::invisible(base::writeLines(base::strwrap("> There are many user selections throughout this program. When prompted press the number of your selection, then 'Enter.' '...' in the console is also a prompt to press enter when ready to continue.")))
	base::invisible(base::readline(prompt =  "..."))


	# Create raster.
	rast <- raster::raster(base_image)

	base::invisible(base::writeLines(base::strwrap("The image is projecting. This may take some time. Please wait.")))

	# Project to coordinate system - takes time
	WGS.r <- raster::projectRaster(rast, crs = "+init=epsg:4326 +proej=longlat
																 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

	# Load in plot data
	plot_data <- readxl::read_excel(file.path(datapath, "metadata.xlsx"),
																	sheet = "Plot_Data")
	field_data <- readxl::read_excel(file.path(datapath, "metadata.xlsx"),
																	 sheet = "Field_Data")
	sp_output <- list()
	single.field <- function(plot_data, field_data, WGS.r, sp_output){
		base::invisible(base::writeLines(base::strwrap("Now that the image has been chosen please answer questions about the field as the program requies information about the field dimensions and plot metadata.")))

	plotlength <- base::invisible(base::readline(prompt = "1. Plot length: "))
	columns <- base::invisible(base::readline(prompt = "2. Columns: "))
	rows <- base::invisible(base::readline(prompt = "3. Rows: "))
	plotlength <- base::as.numeric(plotlength)
	columns <- base::as.numeric(columns) - 1
	rows <- base::as.numeric(rows) - 1

	# Digitizing loop ------------------------------------------------------------
	raster::plot(WGS.r)
	base::invisible(base::writeLines(base::strwrap("Now you need to digitize the center of a few plots. The plots will be centerd and generated based on your selections.")))
	base::invisible(base::readline(prompt =  "..."))
	base::invisible(base::writeLines(strwrap("Step 1: Draw a box around the field  you are working with in the plot window by clicking the top left corner and the bottom right corner. Make the box slightly larger than the field so that you can see all the edges. NOTE: This function can only work with plots that are adjacent to eachother and uniform in shape and distribution.")))
		closecrop <- raster::select(WGS.r)
		raster::plot(closecrop)

		base::invisible(base::writeLines(base::strwrap("Step 2: Draw another box around the bottommost row. This includes any fill.")))
		cropa <- raster::select(closecrop)
		raster::plot(cropa)

		first.points <- function(cropa, plotlength, columns, rows){
			# Create points.
			base::invisible(base::writeLines(base::strwrap("> Step 3: Click the center of the plot in the bottom left corner, then the center of the plot in the bottom right corner. Do your best to center the point in relation to the width and the length of the plot.")))

			plot.centers.south <- raster::click(cropa, n = 2, xy = TRUE)

			# Change corners to format needed for field.plots.
			corner.SW <- base::as.matrix(plot.centers.south[1,1:2])
			corner.SE <- base::as.matrix(plot.centers.south[2,1:2])

			# Set up the WE bearing of the field.
			WE.bearing <- geosphere::finalBearing(corner.SW, corner.SE)

			# Calculate the distance to across the field to set the plot width.
			corners_df <- base::as.data.frame(plot.centers.south)[, 1:2]
			base::names(corners_df) <- c("lon", "lat")
			distmat <- geosphere::distm(corners_df, fun = geosphere::distHaversine)
			field_width <- distmat[1,2]
			plotwidth <- field_width / columns

			# Change the lengths of plots and allies from feet to meters.
			Mplotlength <- plotlength*0.3048
			Mplotwidth <- plotwidth

			# Set up a blank point matrix to be filled in the loop.
			point.matrix <- base::matrix(corner.SW, nrow = 1, ncol = 2)

			# Populate point matrix with points.
			for (i in 1:columns){
				new.point <- geosphere::destPoint(point.matrix[nrow(point.matrix),],
																					b = WE.bearing, d = Mplotwidth)
				point.matrix <- base::rbind(point.matrix, new.point)
			}

			# Change format of points.
			#point.matrix <- point.matrix
			plot.corners <- point.matrix
			#plot.corners.df <- base::as.data.frame(point.matrix)
			plot.corners.sp <- sp::SpatialPoints(point.matrix)

			raster::plot(cropa)
			graphics::points(plot.corners.sp, pch = 19)

			first.points.check <- function(corner.SW, plot.corners, WE.bearing, Mplotwidth,
																		 Mplotlength, point.matrix){
				base::invisible(base::writeLines(base::strwrap("> Are you satisfied with the location of the points?")))
				base::invisible(base::writeLines(c("1: Yes", "2: No")))
				answer <- base::invisible(base::readline(prompt = "Selection: "))

				if (answer == "2"){
					base::invisible(base::writeLines(base::strwrap("> You will have to choose your points again. Try to adjust them according to the inconsistancies in the current image.")))
					first.points(cropa = cropa, plotlength = plotlength, columns = columns, rows = rows)
				}

				raster::plot(closecrop)
				fp_list <- list(corner.SW, plot.corners, WE.bearing, Mplotwidth,
												Mplotlength, point.matrix)
				return(fp_list)

			}

			# Run first.points.check()
			fp_list <- first.points.check(corner.SW, plot.corners, WE.bearing, Mplotwidth,
																		Mplotlength, point.matrix)
			return(fp_list)
			}

		# Run first.points
		fp_list <- first.points(cropa = cropa, plotlength = plotlength, columns = columns, rows = rows)
		corner.SW <- fp_list[[1]]
		plot.corners <- fp_list[[2]]
		WE.bearing <- fp_list[[3]]
		Mplotwidth <- fp_list[[4]]
		Mplotlength <-fp_list[[5]]
		point.matrix <- fp_list[[6]]

		base::invisible(base::writeLines(base::strwrap("> Step 4: Draw another box around the row that contains topmost row.")))
		cropb <- raster::select(closecrop)
		raster::plot(cropb)

		populate.points <- function(){
			base::invisible(base::writeLines(base::strwrap("> Step 5: Click the center of the plot in the top left corner. Do your best to center the point in relation to the width and the length of the plot.", width = 80)))

			plot.center.north <- raster::click(cropb, n = 1, xy = TRUE)
			corner.NW <- base::as.matrix(plot.center.north[ ,1:2])
			NS.bearing <- geosphere::finalBearing(corner.SW, corner.NW)

			# populate the whole field
			for (i in 1:rows){
				new.point <- geosphere::destPoint(plot.corners[nrow(plot.corners)-columns,],
																					b = NS.bearing, d = Mplotlength)
				plot.corners <- base::rbind(plot.corners, new.point)

				for (i in 1:columns){
					new.point <- geosphere::destPoint(plot.corners[nrow(plot.corners),],
																						b = WE.bearing, d = Mplotwidth)
					plot.corners <- base::rbind(plot.corners, new.point)
				}
			}
			# Change format of points.
			#plot.corners <- plot.corners
			plot.corners.df <- base::as.data.frame(plot.corners)
			plot.corners.sp <- sp::SpatialPoints(plot.corners)

			raster::plot(cropb)
			graphics::points(plot.corners.sp, pch = 19)

			populate.points.check <- function(){
				base::invisible(base::writeLines(base::strwrap("Are you satisfied with the location of the points?")))
				base::invisible(base::writeLines(c("1: Yes", "2: No")))
				answer1b <- base::invisible(base::readline(prompt = "Selection: "))

				if (answer1b == "2"){
					base::invisible(base::writeLines(base::strwrap("You will have to choose your points again. Try to adjust them according to the inconsistancies in the current image.")))
					plot.corners <- point.matrix
					plot.corners.df <- base::as.data.frame(point.matrix)
					plot.corners.sp <- sp::SpatialPoints(point.matrix)

					populate.points()
				}
			}

			# Run populate.points.check
			populate.points.check()
			return(list(plot.corners, plot.corners.sp, NS.bearing))
		}

		pp_list <- populate.points()
		plot.corners <- pp_list[[1]]
		plot.corners.sp <- pp_list[[2]]
		NS.bearing <- pp_list[[3]]

		raster::plot(closecrop)
		graphics::points(plot.corners.sp, pch = 19, cex = 0.3)

		spatial.polys2 <- function(){
			# Set the size of the polygons that will form the shape file
			base::invisible(base::writeLines(base::strwrap("Now you will need to set the size of the size of the area that is used to extract the reflectance values. Enter in the number of feet from the center point for both width and length when prompted.")))
			base::invisible(base::readline(prompt =  "..."))
			polyWidth <- base::invisible(base::readline(prompt =
																										"Width of extraction polygon (ft): "))
			polyLength <- base::invisible(base::readline(prompt =
																									 	"Length of extraction polygon (ft): "))

			# Convert width and length to meters
			polyWidth_m <- base::as.numeric(polyWidth)*0.3048
			polyLength_m <- base::as.numeric(polyLength)*0.3048
			# Calculate the points around the edges of the polygons
			topPoint <- geosphere::destPoint(plot.corners, b = NS.bearing, d = polyLength_m)
			botPoint <- geosphere::destPoint(plot.corners, b = 180-NS.bearing, d = polyLength_m)
			lpoints <- base::rbind(topPoint, botPoint)
			rSide <- geosphere::destPoint(lpoints, b = WE.bearing, d = polyWidth_m)
			lSide <- geosphere::destPoint(lpoints, b = 180+WE.bearing, d = polyWidth_m)

			# Convert to dataframe and set ID's so can split list and draw lines between
			# points.
			lSide.df <- base::as.data.frame(lSide)
			lSide.df$ID <- 1:(base::nrow(lSide.df)/2)
			lSide.df$plotOrder <- base::rep(c(2,1), each = base::nrow(lSide.df)/2)

			rSide.df <- base::as.data.frame(rSide)
			rSide.df$ID <- 1:(base::nrow(rSide.df)/2)
			rSide.df$plotOrder <- base::rep(3:4, each = base::nrow(lSide.df)/2)

			shape.points <- base::rbind(rSide.df, lSide.df)

			# Need to order in circle for polygon to plot.
			shape.points <- shape.points[base::with(shape.points, base::order(ID, plotOrder)),]
			shape.points <- shape.points[, 1:3]
			#shape.points.global <- shape.points

			# Split data frame into list by ID
			poly.list <- base::split(shape.points, shape.points$ID)

			# Get rid of Id to create polygons
			poly.list <- base::lapply(poly.list, function(x) { x["ID"] <- NULL; x })
			poly.list2 <- base::lapply(poly.list, function(x){x <- rbind(x, x[1,])})
			poly.list3 <- base::lapply(poly.list2, base::as.matrix)

			# Creat polygons
			# Make polygons of all nested lists.
			ps <- base::lapply(poly.list3, sp::Polygon)

			# Add id variable.
			p1 <- base::lapply(base::seq_along(ps), function(i) sp::Polygons(list(ps[[i]]), ID = base::names(poly.list3)[i]))

			# Create spatial polygons object
			my_spatial_polys <- sp::SpatialPolygons(p1, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))

			spatial.polys.checka(my_spatial_polys)
			spatial.polys.checkb(my_spatial_polys)
			return(list(my_spatial_polys, shape.points))
		}

		spatial.polys.checka <- function(my_spatial_polys){

			raster::plot(cropa)
			raster::plot(my_spatial_polys, add = T)

			base::invisible(base::writeLines(base::strwrap("Are you satisfied with the polygons in the bottom of the field?")))
			base::invisible(base::writeLines(c("1: Yes", "2: No")))
			answer2 <- base::invisible(base::readline(prompt = "Selection: "))


			if (answer2 == "2"){
				base::invisible(base::writeLines(base::strwrap("You can adjust the size of the polygons so that they all fit inside the plots. You will be prompted to reset the size of the polygons.")))
				spatial.polys2()
			}
		}

		spatial.polys.checkb <- function(my_spatial_polys){
			raster::plot(cropb)
			raster::plot(my_spatial_polys, add = T)
			base::invisible(base::writeLines(base::strwrap("Are you satisfied with the polygons in the top of the field?")))
			base::invisible(base::writeLines(c("1: Yes", "2: No")))
			answer3 <- base::invisible(base::readline(prompt = "Selection: "))

			if (answer3 == "2"){
				base::invisible(base::writeLines(base::strwrap("Adjust the size of the polygons so that they all fit inside the plots. You will be prompted to reset the size of the polygons.", width = 80)))
				spatial.polys2()
			}
		}

		msp_list <- spatial.polys2()
		my_spatial_polys <- msp_list[[1]]
		shape.points <- msp_list[[2]]

		output.shapefile <- function(my_spatial_polys, shape.points, fdata = data.frame()){
			# Create Spatial Polygons data frame
			my_spatial_polys_df <- sp::SpatialPolygonsDataFrame(my_spatial_polys,
																													base::data.frame(id = base::unique(shape.points$ID)))

			base::invisible(base::writeLines("Is there fill around the edges of this field?"))
			base::invisible(base::writeLines(c("1: Yes", "2: No")))
			answer5 <- base::invisible(base::readline(prompt = "Selection: "))

			if (answer5 == "1"){
				fill <- c(1:(columns+1),
									(base::nrow(my_spatial_polys_df)-columns):base::nrow(my_spatial_polys_df),
									base::seq(from = 1, to  = (base::nrow(my_spatial_polys_df)-columns),
														by = columns + 1),
									base::seq(from = columns + 1, to  = base::nrow(my_spatial_polys_df),
														by = columns + 1))

				# Get rid of fill polygons
				"%ni%" <- base::Negate("%in%")
				my_spatial_polys_df <- base::subset(my_spatial_polys_df, id %ni% fill)
				my_spatial_polys_df$id <- 1:base::nrow(my_spatial_polys_df)
			}

			# Subset plot data to this field -------------------------------------------
			if(nrow(fdata) == 1){
				blocks <- base::as.numeric(base::strsplit(fdata$blocks, "/")[[1]])
				attributes <- plot_data[plot_data$location == fdata$location & plot_data$trial == fdata$trial &
																	plot_data$nitro.mana == fdata$nitro.mana &
																	plot_data$irrig.mana == fdata$irrig.mana &
																	plot_data$species == fdata$species, ]
				attributes <- base::subset(attributes, block %in% blocks)

				attributesOrdered <- attributes[with(attributes, base::order(row, column)),]

				# Create unique id.
				id <- 1:base::nrow(attributesOrdered)

				# Match ID to attribute file
				total.plot.data <- base::cbind(attributesOrdered, id)

				# Add attributes to shape file
				#require(sp)
				merged_sp <- sp::merge(my_spatial_polys_df, total.plot.data, by = "id")
				# Field names cannot be longer than 10 characters in a shapefile
				names(merged_sp) <- base::strtrim(names(merged_sp),10)
				sp_output <- c(sp_output, merged_sp)
				return(sp_output)
			}

			if(nrow(fdata) != 1){
				sp_output <- c(sp_output, my_spatial_polys_df)
				return(sp_output)
			}

		}

		sp_output <- output.shapefile(my_spatial_polys = my_spatial_polys,
																	shape.points = shape.points)

		second_field <- function(){
			# Set up for making a second field shapefile on the same image
			base::invisible(base::writeLines(base::strwrap("> Do you have another field to add
																										 in this image?")))
			base::invisible(base::writeLines(c("1: Yes", "2: No")))
			answer_addshp<- base::invisible(base::readline(prompt = "Selection: "))

			if (answer_addshp == "1"){
				sp_output <- single.field(plot_data, field_data, WGS.r, sp_output)
			} else{
				#merge shapefiles
				return(sp_output)
			}
		}
		sp_output <- second_field()
		return(sp_output)
		}

	final_output <- single.field(plot_data, field_data, WGS.r, sp_output)
	merged_sp <- final_output[[1]]

	if(length(final_output) > 1){
		for (i in 2:base::length(final_output)){
			merged_sp <- base::rbind(merged_sp, final_output[i])
		}
	}

	rgdal::writeOGR(obj = merged_sp, dsn = base::file.path(datapath, shp_name), layer = "fieldPlots", driver = "ESRI Shapefile")
	base::invisible(base::writeLines(base::strwrap("> A shapefile has been created for this field. To overwrite it or to srart a new field run plotshapes()")))
		}
