################################################################################
#' Make a SpatialPolygonsDataFrame object of the biomes
#'
#' \code{gd_get_biomes_spdf} returns a SpatialPolygonsDataFrame object of
#' the Whittaker's biomes modified by Ricklefs (2008) in function of mean
#' annual temperature (MAT) and mean annual precipitation (MAP)
#' (MAT in degrees Celsius and MAP in mm).
#'
#' This function is not commonly used alone, but it is called by
#' \code{\link{gd_get_biome}}.
#'
#' @family Get Data Functions
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#'    in a single biome. By default, deserts are not merged.
#'
#' @return An object of class SpatialPolygonsDataFrame
#'
#' @examples
#' # Get the SpatialPolygonsDataFrame object of the biomes
#' biomes_spdf <- gd_get_biomes_spdf()
#'
#' @export

# START
# Function declaration
gd_get_biomes_spdf <- function(merge_deserts = FALSE) {

  # STEP 0
  # Argument checks
  # Is merge_deserts logical?
  if (!(is.logical(merge_deserts))) {
    stop('merge_deserts must be logical.')
  }
  # Is merge_deserts NA?
  if (is.na(merge_deserts)) {
    stop('merge_deserts must be either TRUE or FALSE.')
  }

  # STEP 1
  # Create the data frame
  biomes_df <- data.frame(
    mat = c(
      29.339, 13.971, 15.371, 17.510, 24.131, 27.074, 28.915, 29.201, 29.339,
      13.971, -9.706, -7.572, 4.491, 17.510, 15.371, 13.971, 17.510, 4.491,
      -7.572, -9.706, -6.687, -0.949, 3.098, 7.147, 10.165, 13.918, 18.626,
      18.176, 17.510, 18.626, 13.918, 10.165,  7.147, 3.098, -0.949, 1.039,
      1.998, 2.444, 3.118, 4.446, 7.758, 12.614, 18.720, 18.637, 18.626, -0.949,
      -6.687, -4.395, -4.098, -1.592, 0.914, 4.155, 3.118, 2.444, 1.998, 1.039,
      -0.949, 18.720,  12.614, 7.758, 4.446, 3.118, 4.155, 15.716, 20.136,
      19.392, 18.720, 18.720, 19.392, 20.136, 22.278, 23.756, 24.199, 24.714,
      25.667, 26.105, 27.414, 27.772, 25.709, 21.736, 18.720, 17.510, 18.176,
      18.626, 18.637, 18.720, 21.736, 25.709, 27.772, 28.418, 28.915, 27.074,
      24.131, 17.510, -6.687, -8.896, -9.706, -13.382, -15.366, -15.217, -8.373,
      -4.098, -1.592, -4.098, -4.395, -6.687
      ),
    map = c(
      21.3, 23.0, 174.6, 535.1, 702.9, 847.9, 992.4, 532.1, 21.3, 23.0, 7.3,
      87.2, 314.6, 535.1, 174.6, 23.0, 535.1, 314.6, 87.2, 7.3, 202.6, 391.7,
      529.9, 783.1, 956.9, 1116.5, 1269.3, 794.3, 535.1, 1269.3, 1116.5, 956.9,
      783.1, 529.9, 391.7, 514.8, 673.4, 968.5, 1630.6, 1839.7, 2028.0, 2224.0,
      2355.7, 1837.6, 1269.3, 391.7, 202.6, 922.9, 1074.1, 1405.9, 1744.9,
      2012.3, 1630.6, 968.5, 673.4, 514.8, 391.7, 2355.7, 2224.0, 2028.0,
      1839.7, 1630.6, 2012.3, 2930.1, 3377.7, 2917.0, 2355.7, 2355.7, 2917.0,
      3377.7, 3896.5, 4343.1, 4415.2, 4429.8, 4279.0, 4113.7, 3344.4, 2790.6,
      2574.0, 2414.3, 2355.7, 535.1, 794.3, 1269.3, 1837.6, 2355.7, 2414.3,
      2574.0, 2790.6, 1920.3, 992.4, 847.9, 702.9, 535.1, 202.6, 50.8, 7.3,
      34.8, 98.8, 170.8, 533.0, 1074.1, 1405.9, 1074.1, 922.9, 202.6
      ),
    biome = c(
      rep('Subtropical desert', 9), rep('Temperate grassland/desert', 7),
      rep('Woodland/shrubland', 13), rep('Temperate forest', 16),
      rep('Boreal forest', 12), rep('Temperate rain forest', 10),
      rep('Tropical rain forest', 14), rep('Tropical seasonal forest/savanna', 13),
      rep('Tundra', 12)
      )
  )

  # STEP 2
  # Merge deserts if specified
  if (merge_deserts){

    biome <- as.character(biomes_df$biome)

    biome[grepl('desert', biome, fixed = TRUE)] <- 'Desert'

    biomes_df$biome <- as.factor(biome)

  }

  # STEP 3
  # Create SpatialPolygonsDataFrame object
  list_pol <- sapply(as.character(unique(biomes_df$biome)),
                     function(id_biome,df)
                       sp::Polygon(cbind(df$map[df$biome == id_biome],
                                         df$mat[df$biome == id_biome])),
                     df=biomes_df, USE.NAMES = TRUE)

  sp_biomes <- sp::SpatialPolygons(
    lapply(1:length(list_pol),
           function(i, x) {sp::Polygons(list(x[[i]]),
                                        names(x)[i])},
           x = list_pol)
  )

  spdf_biomes <- sp::SpatialPolygonsDataFrame(
    sp_biomes, data.frame(biome = names(list_pol)), match.ID = 'biome'
    )

  # STEP 4
  # Return SpatialPolygonsDataFrame object
  return(spdf_biomes)

  # END FUNCTION
}

################################################################################
#' Get the biome, temperature and precipitation of a site
#'
#' \code{gd_get_biome} takes either geographical coordinats or climatic data
#' and returns the Whittaker's biome.
#'
#' This function takes a data frame including at least either two columns
#' with geographical coordinates (latitude and longitude), or two columns with
#' climatic data (mean annual temperature and mean annual precipitation).
#' It returns the same data frame with extra columns of climatic data (si_mat
#' and si_map, if not provided) and biome (si_biome), according to
#' \code{\link{gd_get_biomes_spdf}}.
#'
#' Alternatively it accepts either a value of latitude and a value of longitude
#' or a value of mean annual temperature (MAT) and a value of mean annual
#' precipitation (MAP), and returns a data frame with latitude (if provided),
#' longitude (if provided), mean annual temperature, mean annual precipitation
#' and biome.
#'
#' Climatic data is obtained from WorldClim 1.4 using the \code{RFc} package.
#'
#' @family Get Data Functions
#'
#' @param data Data frame including either geographical coordinates (latitude
#'    and longitude) or climatic data (MAT and MAP).
#'
#' @param si_lat Character, name of the column containing latitude values in
#'    decimal degrees or, alternatively, if \code{data} not provided, a numeric
#'    value of latitude.
#'
#' @param si_long Character, name of the column containing longitude values in
#'    decimal degrees or, alternatively, if \code{data} not provided, a numeric
#'    value of longitude.
#'
#' @param si_mat Character, name of the column containing mean annual
#'    temperature values in degrees Celsius or, alternatively, if \code{data}
#'    not provided, a numeric value of MAT.
#'
#' @param si_map Character, name of the column containing mean annual
#'    precipitation values in millimeters or alternatively, if \code{data} not
#'    provided, a numeric value of MAP.
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#'    in a single biome. By default, deserts are not merged.
#'
#' @return A data frame with the original data and extra columns of
#'    mean annual temperature, mean annual precipitation and biome.
#'
#' @examples
#' # Create a data frame with coordinates
#' sites <- data.frame(site = c("A", "B"), lat = c(69.49, 41.43),
#'                     long = c(27.23, 2.07))
#'
#' # Get temperature, precipitation and biomes
#' gd_get_biome(sites, si_lat = "lat", si_long = "long")
#'
#' # Alternatively, for one site
#' gd_get_biome(si_mat = 15.8, si_map = 622)
#'
#' @export

# START
# Function declaration
gd_get_biome <- function(data, si_lat, si_long, si_mat, si_map,
                         merge_deserts = FALSE) {

  # STEP 0
  # Argument checks
  # Are either coordinates or climatic data provided?
  if (all(c(any(c(missing(si_lat), missing(si_long))),
        c(any(missing(si_mat), missing(si_map)))))) {
    stop('Either coordinates or climatic data must be provided.')
  }

  if (missing(data)){

    if (all(c(!missing(si_lat), !missing(si_long)))) {
      # Are si_lat and si_long numeric?
      if (any(c(!is.numeric(si_lat), !is.numeric(si_long)))) {
        stop('Coordinates must be numeric.')
      }
      # Are coordinates values possible?
      if (any(c(abs(si_lat) > 90, abs(si_long) > 180))) {
        stop('Coordinates are not meaningful.')
      }
    }

    if (all(c(!missing(si_mat), !missing(si_map)))) {
      # Are si_mat and si_map numeric?
      if (any(c(!is.numeric(si_mat), !is.numeric(si_map)))) {
        stop('Climatic variables must be numeric.')
      }
      # Are climatic values possible?
      if (any(c((si_mat > 30 | si_mat < -16), (si_map > 4430 | si_map < 7)))) {
        stop('Climatic data is not meaningful.')
      }
    }

  } else {

    # Is data a data.frame?
    if (!is.data.frame(data)) {
      stop('Provided data object is not a data frame.')
    }

    if (all(c(!missing(si_lat), !missing(si_long)))) {
      # Are si_lat and/or si_long characters?
      if (!all(c(is.character(si_lat), is.character(si_long)))) {
        stop('si_lat and/or si_long are not characters.')
      }
      # Does data contain a latitude variable?
      if (is.null(data[[si_lat]])) {
        stop('There is no latitude variable in this dataset.')
      }
      # Does data contain a longitude variable?
      if (is.null(data[[si_long]])) {
        stop('There is no longitude variable in this dataset.')
      }
      # Are si_lat and si_long numeric?
      if (any(c(!is.numeric(data[[si_lat]]), !is.numeric(data[[si_long]])))) {
        stop('Coordinates must be numeric.')
      }
      # Are there NA values in si_lat and/or si_long?
      if (any(c(is.na(data[[si_lat]]), is.na(data[[si_long]])))) {
        stop('Coordinates have NA values.')
      }
      # Are coordinates values possible?
      if (any(c(any(abs(data[[si_lat]]) > 90),
                any(abs(data[[si_long]]) > 180)))) {
        stop('Coordinates are not meaningful.')
      }
    }

    if (all(c(!missing(si_mat), !missing(si_map)))) {
      # Are si_mat and/or si_map characters?
      if (!all(c(is.character(si_mat), is.character(si_map)))) {
        stop('si_mat and/or si_map are not characters.')
      }
      # Does data contain a MAT variable?
      if (is.null(data[[si_mat]])) {
        stop('There is no temperature variable in this dataset.')
      }
      # Does data contain a MAP variable?
      if (is.null(data[[si_map]])) {
        stop('There is no precipitation variable in this dataset.')
      }
      # Are si_mat and si_map numeric?
      if (any(c(!is.numeric(data[[si_mat]]), !is.numeric(data[[si_map]])))) {
        stop('Climatic variables must be numeric.')
      }
      # Are there NA values in si_mat and/or si_map?
      if (any(c(is.na(data[[si_mat]]), is.na(data[[si_map]])))) {
        stop('Climatic variables have NA values.')
      }
      # Are climatic values possible?
      if (any(c((any(data[[si_mat]] > 30) | any(data[[si_mat]] < -16)),
                (any(data[[si_map]] > 4430) | any(data[[si_map]] < 7))))) {
        stop('Climatic data is not meaningful.')
      }
    }
  }

  # Is merge_deserts logical?
  if (!(is.logical(merge_deserts))) {
    stop('merge_deserts must be logical.')
  }
  # Is merge_deserts NA?
  if (is.na(merge_deserts)) {
    stop('merge_deserts must be either TRUE or FALSE.')
  }

  # STEP 1
  # Variables preparation
  # 1.1 Construct data frame if individual values provided
  if (missing(data)){

    if (all(c(!missing(si_lat), !missing(si_long)))){
      lat_site <- si_lat
      long_site <- si_long
      data <- data.frame(si_lat = si_lat, si_long = si_long)

      if (all(c(!missing(si_mat), !missing(si_map)))){
        t_site <- si_mat
        p_site <- si_map
        data$si_mat <- si_mat
        data$si_map <- si_map
      }

    } else if (all(c(!missing(si_mat), !missing(si_map)))){
      t_site <- si_mat
      p_site <- si_map
      data <- data.frame(si_mat = si_mat, si_map = si_map)
    }

  # 1.2 Construct coordinates and climatic vectors if data provided
  } else {

    if (all(c(!missing(si_lat), !missing(si_long)))){
      lat_site <- data[[si_lat]]
      long_site <- data[[si_long]]
    }

    if (all(c(!missing(si_mat), !missing(si_map)))){
      t_site <- data[[si_mat]]
      p_site <- data[[si_map]]
    }

  }

  # STEP 2
  # Obtain MAT and MAP values and append them to data, if not provided
  # 2.1 Obtain MAT and MAP values
  if (any(c(missing(si_mat), missing(si_map)))){

    suppressMessages(
      t_site <- as.vector(
        RFc::fcTimeSeriesYearly(
          'airt', lat_site, long_site, firstYear = 1990, lastYear = 1990,
          firstDay = 1, lastDay = 365, startHour = 0, stopHour = 23,
          dataSets = 'WorldClim 1.4'
          )$values
        )
    )

    suppressMessages(
      p_site <- 12*as.vector(
        RFc::fcTimeSeriesYearly(
          'prate', lat_site, long_site, firstYear = 1990,lastYear = 1990,
          firstDay = 1, lastDay = 365, startHour = 0, stopHour = 23,
          dataSets = 'WorldClim 1.4'
          )$values
        )
    )

    # 2.1 Append MAT and MAP values
    data$si_mat <- t_site
    data$si_map <- p_site

  }

  # STEP 3
  # Obtain biome
  clim_point <- sp::SpatialPoints(data.frame(x = p_site, y = t_site))

  biome <- sp::over(
    clim_point, gd_get_biomes_spdf(merge_deserts = merge_deserts)
    )[[1]]

  # STEP 4
  # Append biome and return the data frame
  # 4.1 Append biome to data
  data$si_biome <- droplevels(biome)

  # 4.2 Return data with the new variables
  return(data)

  # END FUNCTION
}
