library(ggbiome)

###########################################################
context('B1. Biomes plot')

test_that('argument checks work', {
  expect_error(vis_biome(merge_deserts = 25),
               'merge_deserts must be logical')
  expect_error(vis_biome(merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE')
})

test_that('plot is created', {
  expect_is(vis_biome(), 'ggplot')
})

###########################################################
context('B2. Biomes plot with sites')

foo_data_coord <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_lat = c(-36.785, 69.491822025, 41.4309888889, 34.3863888888889),
  si_long = c(146.582, 27.2310822944, 2.07361111111111, -106.529444444444)
)

foo_data_coord_bad1 <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_lat = c(-36.785, NA, 41.4309888889, 34.3863888888889),
  si_long = c(146.582, 27.2310822944, 2.07361111111111, -106.529444444444)
)

foo_data_coord_bad2 <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_lat = c(-36.785, 69.491822025, 41.4309888889, 34.3863888888889),
  si_long = c(146.582, 27.2310822944, 2.07361111111111, -196.529444444444)
)

foo_data_clim <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_mat = c(11.85414, -1.46320, 15.73861, 11.16112),
  si_map = c(1137.5661, 424.9901, 626.0845, 332.1532)
)

foo_data_clim_bad1 <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_mat = c(11.85414, -1.46320, 15.73861, 11.16112),
  si_map = c(1137.5661, 424.9901, NA, 332.1532)
)

foo_data_clim_bad2 <- data.frame(
  si_code = c('A', 'B', 'C', 'D'),
  si_mat = c(11.85414, -35.46320, 15.73861, 11.16112),
  si_map = c(1137.5661, 424.9901, 626.0845, 332.1532)
)

test_that('argument checks work', {
  expect_error(vis_location_biome(si_lat = 41.43, si_map = 626),
               'Either coordinates or climatic data must be provided.')
  expect_error(vis_location_biome(si_lat = '41.43', si_long = 2.07),
               'Coordinates must be numeric.')
  expect_error(vis_location_biome(si_lat = -95, si_long = 2.07),
               'Coordinates are not meaningful.')
  expect_error(vis_location_biome(si_mat = 15.7, si_map = '626'),
               'Climatic variables must be numeric.')
  expect_error(vis_location_biome(si_mat = -20, si_map = 626),
               'Climatic data is not meaningful.')
  expect_error(vis_location_biome('foo', si_lat = 'si_lat', si_long = 'si_long'),
               'Provided data object is not a data frame.')
  expect_error(vis_location_biome(foo_data_coord,
                            si_lat = 'si_lat', si_long = 'foo'),
               'There is no longitude variable in this dataset.')
  expect_error(vis_location_biome(foo_data_coord,
                            si_lat = 'foo', si_long = 'si_long'),
               'There is no latitude variable in this dataset.')
  expect_error(vis_location_biome(foo_data_coord,
                            si_lat = 'si_code', si_long = 'si_long'),
               'Coordinates must be numeric.')
  expect_error(vis_location_biome(foo_data_coord_bad1,
                            si_lat = 'si_lat', si_long = 'si_long'),
               'Coordinates have NA values.')
  expect_error(vis_location_biome(foo_data_coord_bad2,
                            si_lat = 'si_lat', si_long = 'si_long'),
               'Coordinates are not meaningful.')
  expect_error(vis_location_biome(foo_data_clim,
                            si_mat = 'si_mat', si_map = 'foo'),
               'There is no precipitation variable in this dataset.')
  expect_error(vis_location_biome(foo_data_clim,
                            si_mat = 'foo', si_map = 'si_map'),
               'There is no temperature variable in this dataset.')
  expect_error(vis_location_biome(foo_data_clim,
                            si_mat = 'si_mat', si_map = 'si_code'),
               'Climatic variables must be numeric.')
  expect_error(vis_location_biome(foo_data_clim_bad1,
                            si_mat = 'si_mat', si_map = 'si_map'),
               'Climatic variables have NA values.')
  expect_error(vis_location_biome(foo_data_clim_bad2,
                            si_mat = 'si_mat', si_map = 'si_map'),
               'Climatic data is not meaningful.')
  expect_error(vis_location_biome(foo_data_coord, si_lat = 'si_lat',
                                  si_long = 'si_long', point_labels = 1),
               'point_labels must be a character vector or factor.')
  expect_error(vis_location_biome(foo_data_coord, si_lat = 'si_lat',
                                  si_long = 'si_long',
                                  point_labels = c('A', 'B')),
               'point_labels must be either of length one or length equal')
  expect_error(vis_location_biome(si_lat = 41.43, si_long = 2.07,
                                  point_labels = c('A', 'B')),
               'point_labels must be either of length one or length equal')
  expect_error(vis_location_biome(foo_data_clim, si_mat = 'si_mat',
                                  si_map = 'si_map', col_fill = 1),
               'col_fill must be a character vector or factor.')
  expect_error(vis_location_biome(foo_data_clim, si_mat = 15.7, si_map = 626,
                                  col_fill = c('white', 'black')),
               'col_fill must be either of length one or length equal')
  expect_error(vis_location_biome(foo_data_coord, si_lat = 'si_lat',
                            si_long = 'si_long', merge_deserts = 25),
               'merge_deserts must be logical.')
  expect_error(vis_location_biome(foo_data_clim, si_mat = 'si_mat',
                            si_map = 'si_map', merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE.')
})

test_that('plot is created', {
  expect_is(vis_location_biome(
    foo_data_clim, si_mat = 'si_mat', si_map = 'si_map',
    point_labels = foo_data_clim$si_code,
    col_fill = c('black', 'green', 'red', 'blue')
    ), 'ggplot')
  expect_is(vis_location_biome(
    si_lat = 41.43, si_long = 2.07, point_labels = 'A', col_fill = 'red'
    ), 'ggplot')
})
