################################################################################
#' Plot a diagram of biomes
#'
#' \code{vis_biome} produces a ggplot object showing the Whittaker's biomes as
#' colored areas according to mean annual temperature (MAT) and mean annual
#' precipitation (MAP) using a SpatialPolygonsDataFrame object obtained with
#' \code{\link{gd_get_biomes_spdf}}
#'
#' @family Visualization Functions
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#' in a single biome. By default, deserts are not merged.
#'
#' @return A ggplot object showing the biomes.
#'
#' @examples
#' # Create a ggplot diagram of the biomes
#' vis_biome()
#'
#' @export

# START
# Function declaration
vis_biome <- function(merge_deserts = FALSE) {

  # STEP 0
  # Argument checks
  # Is merge_deserts logical?
  if (!(is.logical(merge_deserts))) {
    stop('merge_deserts must be logical')
  }
  # Is merge_deserts NA?
  if (is.na(merge_deserts)) {
    stop('merge_deserts must be either TRUE or FALSE')
  }

  # STEP 1
  # Get biomes SpatialPointsDataFrame object
  suppressMessages(
    biomes_df <- ggplot2::fortify(gd_get_biomes_spdf(
      merge_deserts = merge_deserts))
  )

  # STEP 2
  # Make and return the plot object
  # 2.1 Make color palette
  if (merge_deserts){

    pal <- viridis::viridis(9)[c(2,9,4,7,8,6,1,3)]

  } else {

    pal <- viridis::viridis(9)[c(2,5,4,9,7,8,6,1,3)]

  }

  # 2.2 Make the plot object
  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = biomes_df,
      ggplot2::aes_(x = ~long, y = ~lat, group = ~id, fill = ~id)) +
    ggplot2::scale_fill_manual('Biomes', values = pal) +
    ggplot2::xlab('Mean annual precipitation (mm)') +
    ggplot2::ylab(expression(paste('Mean annual temperature ', (degree*C))))

  # 2.3 Return the plot object
  return(plot)

  # END FUNCTION
}

################################################################################
#' Plot a diagram of biomes with sites as dots
#'
#' \code{vis_location_biome} produces a ggplot object showing the biomes as
#' colored areas according to mean annual temperature (MAT) and mean annual
#' precipitation (MAP), using the function \code{\link{vis_biome}}, and adds
#' the sites on it according to their values of MAT and MAP, obtained using
#' the function \code{\link{gd_get_biome}} if not provided.
#'
#' @family Visualization Functions
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
#' @param point_labels A character vector the same length of the number of
#'    sites to be plotted, indicating site labels. If missing, no labels are
#'    plotted.
#'
#' @param col_fill A character vector the same length of the number of
#'    sites to be plotted, indicating the fill color for site points, or
#'    alternatively a character vector of length one indicating a single
#'    fill color for all sites. The default is white.
#'
#' @param merge_deserts Logical indicating if desert biomes should be merged
#'    in a single biome. By default, deserts are not merged.
#'
#' @return A ggplot object showing the biomes and sites.
#'
#' @examples
#' # Create a data frame with coordinates
#' sites <- data.frame(site = c("A", "B"), lat = c(69.49, 41.43),
#'                     long = c(27.23, 2.07))
#'
#' # Create a ggplot diagram of the biomes with sites
#' vis_location_biome(sites, si_lat = "lat", si_long = "long",
#'                    point_labels = sites$site)
#'
#' # Alternatively, for one site
#' vis_location_biome(si_mat = 15.8, si_map = 622, col_fill = 'red')
#'
#' @export

# START
# Function declaration
vis_location_biome <- function(data, si_lat, si_long, si_mat, si_map,
                               point_labels, col_fill = 'white',
                               merge_deserts = FALSE) {

  # STEP 0
  # Argument checks
  if (!missing(point_labels)) {
    # Is point_labels a character vector or factor?
    if (all(c(!is.character(point_labels), !is.factor(point_labels)))) {
      stop('point_labels must be a character vector or factor.')
    }
    if (missing(data)) {
      # Is point_labels either of length one or equal to the number of sites?
      if (length(point_labels) != 1) {
        stop('point_labels must be either of length one or length equal ',
             'to the number of sites.')
      }
    } else {
      if (all(c(length(point_labels) != nrow(data)),
              length(point_labels) != 1)) {
        stop('point_labels must be either of length one or length equal ',
             'to the number of sites.')
      }
    }
  }
  # Is col_fill a character vector or factor?
  if (all(c(!is.character(col_fill), !is.factor(col_fill)))) {
    stop('col_fill must be a character vector or factor.')
  }
  if (missing(data)) {
    # Is col_fill either of length one or equal to the number of sites?
    if (length(col_fill) != 1) {
      stop('col_fill must be either of length one or length equal ',
           'to the number of sites.')
    }
  } else {
    if (all(c(length(col_fill) != nrow(data)), length(col_fill) != 1)) {
      stop('col_fill must be either of length one or length equal ',
           'to the number of sites.')
    }
  }

  # STEP 1
  # Prepare the data frame
  # 1.1 Get the needed information
  df <- gd_get_biome(data = data, si_lat = si_lat, si_long = si_long,
                     si_mat = si_mat, si_map = si_map,
                     merge_deserts = merge_deserts)

  # 1.2 Change column names if needed
  if (!missing(data)) {
    if (all(c(!missing(si_mat), !missing(si_map)))) {

      names(df)[[which(names(df) == si_mat)]] <- 'si_mat'
      names(df)[[which(names(df) == si_map)]] <- 'si_map'

    }
  }

  # 1.3 Add labels if provided


  # STEP 2
  # Make and return the plot
  # 2.1 Get biome plot
  plot <- vis_biome(merge_deserts = merge_deserts)

  # 2.2 Make the new plot object
  plot <- plot +
    ggplot2::geom_point(data = df,
                        ggplot2::aes(x = si_map, y = si_mat), color = 'black',
                        shape = 21, fill = col_fill, size = 2, stroke = 0.5)  +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c (0, 4500), ylim = c(-16, 30),
                             expand = FALSE)

  # 2.3 Add labels if provided
  if (!missing(point_labels)) {

    df$point_labels <- point_labels

    plot <- plot +
      ggplot2::geom_text(data = df, ggplot2::aes(x = si_map, y = si_mat,
                                        label = point_labels), nudge_y = 1)
  }

  # 3.3 Return the plot object
  return(plot)

  # END FUNCTION
}
