library(ggbiome)

###########################################################
context('A1. Get biomes object')

test_that('argument checks work', {
  expect_error(gd_get_biomes_spdf(merge_deserts = 25),
               'merge_deserts must be logical.')
  expect_error(gd_get_biomes_spdf(merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE.')
})

test_that('biomes object is created', {
  expect_is(gd_get_biomes_spdf(), 'SpatialPolygonsDataFrame')
  expect_is(gd_get_biomes_spdf(merge_deserts = TRUE),
            'SpatialPolygonsDataFrame')
})

###########################################################
context('A2. Get biome of a site')

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

results_foo_data <- cbind(foo_data_coord, foo_data_clim[,-1], data.frame(
  si_biome = c('Temperate forest', 'Boreal forest', 'Woodland/shrubland',
               'Temperate grassland/desert')
))

results_foo_data_md <- cbind(foo_data_coord, foo_data_clim[,-1], data.frame(
  si_biome = c('Temperate forest', 'Boreal forest', 'Woodland/shrubland', 'Desert')
))

results_foo_data_nodf <- data.frame(
  si_lat = 34.3863888888889, si_long = -106.529444444444,
  si_mat = 11.16112, si_map = 332.1532,
  si_biome = 'Temperate grassland/desert'
)

results_foo_data_nodf_md <- data.frame(
  si_lat = 34.3863888888889, si_long = -106.529444444444,
  si_mat = 11.16112, si_map = 332.1532,
  si_biome = 'Desert'
)

test_that('argument checks work', {
  expect_error(gd_get_biome(si_lat = 41.43, si_map = 626),
               'Either coordinates or climatic data must be provided.')
  expect_error(gd_get_biome(si_lat = '41.43', si_long = 2.07),
               'Coordinates must be numeric.')
  expect_error(gd_get_biome(si_lat = -95, si_long = 2.07),
               'Coordinates are not meaningful.')
  expect_error(gd_get_biome(si_mat = 15.7, si_map = '626'),
               'Climatic variables must be numeric.')
  expect_error(gd_get_biome(si_mat = -20, si_map = 626),
               'Climatic data is not meaningful.')
  expect_error(gd_get_biome('foo', si_lat = 'si_lat', si_long = 'si_long'),
               'Provided data object is not a data frame.')
  expect_error(gd_get_biome(foo_data_coord,
                            si_lat = 'si_lat', si_long = 'foo'),
               'There is no longitude variable in this dataset.')
  expect_error(gd_get_biome(foo_data_coord,
                            si_lat = 'foo', si_long = 'si_long'),
               'There is no latitude variable in this dataset.')
  expect_error(gd_get_biome(foo_data_coord,
                            si_lat = 'si_code', si_long = 'si_long'),
               'Coordinates must be numeric.')
  expect_error(gd_get_biome(foo_data_coord_bad1,
                            si_lat = 'si_lat', si_long = 'si_long'),
               'Coordinates have NA values.')
  expect_error(gd_get_biome(foo_data_coord_bad2,
                            si_lat = 'si_lat', si_long = 'si_long'),
               'Coordinates are not meaningful.')
  expect_error(gd_get_biome(foo_data_clim,
                            si_mat = 'si_mat', si_map = 'foo'),
               'There is no precipitation variable in this dataset.')
  expect_error(gd_get_biome(foo_data_clim,
                            si_mat = 'foo', si_map = 'si_map'),
               'There is no temperature variable in this dataset.')
  expect_error(gd_get_biome(foo_data_clim,
                            si_mat = 'si_mat', si_map = 'si_code'),
               'Climatic variables must be numeric.')
  expect_error(gd_get_biome(foo_data_clim_bad1,
                            si_mat = 'si_mat', si_map = 'si_map'),
               'Climatic variables have NA values.')
  expect_error(gd_get_biome(foo_data_clim_bad2,
                            si_mat = 'si_mat', si_map = 'si_map'),
               'Climatic data is not meaningful.')
  expect_error(gd_get_biome(foo_data_coord, si_lat = 'si_lat',
                            si_long = 'si_long', merge_deserts = 25),
               'merge_deserts must be logical.')
  expect_error(gd_get_biome(foo_data_clim, si_mat = 'si_mat',
                            si_map = 'si_map', merge_deserts = NA),
               'merge_deserts must be either TRUE or FALSE.')
})

test_that('the new variables are added and their type is correct', {
  expect_equal(
    c(names(foo_data_coord), 'si_mat', 'si_map', 'si_biome'),
    names(gd_get_biome(foo_data_coord, si_lat = 'si_lat', si_long = 'si_long'))
  )
  expect_equal(
    c(names(foo_data_clim), 'si_biome'),
    names(gd_get_biome(foo_data_clim, si_mat = 'si_mat', si_map = 'si_map'))
  )
  expect_is(gd_get_biome(foo_data_coord, si_lat = 'si_lat',
                         si_long = 'si_long')$si_map, 'numeric')
  expect_is(gd_get_biome(si_lat = 41.43, si_long = 2.07)$si_mat, 'numeric')
  expect_is(gd_get_biome(foo_data_clim, si_mat = 'si_mat',
                         si_map = 'si_map')$si_biome, 'factor')
})

test_that('new data is correct', {
  expect_equal(gd_get_biome(foo_data_coord, si_lat = 'si_lat',
                            si_long = 'si_long'), results_foo_data,
               tolerance = .0001)
  expect_equal(gd_get_biome(foo_data_coord, si_lat = 'si_lat',
                            si_long = 'si_long', merge_deserts = TRUE),
               results_foo_data_md, tolerance = .0001)
  expect_equal(gd_get_biome(foo_data_clim, si_mat = 'si_mat',
                            si_map = 'si_map'),
               subset(results_foo_data, select = -c(si_lat, si_long)),
               tolerance = .0001)
  expect_equal(gd_get_biome(foo_data_clim, si_mat = 'si_mat',
                            si_map = 'si_map', merge_deserts = TRUE),
               subset(results_foo_data_md, select = -c(si_lat, si_long)),
               tolerance = .0001)
  expect_equal(gd_get_biome(si_lat = 34.3863888888889,
                            si_long = -106.529444444444),
               results_foo_data_nodf, tolerance = .0001)
  expect_equal(gd_get_biome(si_lat = 34.3863888888889,
                            si_long = -106.529444444444, merge_deserts = TRUE),
               results_foo_data_nodf_md, tolerance = .0001)
  expect_equal(gd_get_biome(si_mat = 11.16112, si_map = 332.1532),
               subset(results_foo_data_nodf, select = -c(si_lat, si_long)),
               tolerance = .0001)
  expect_equal(gd_get_biome(si_mat = 11.16112, si_map = 332.1532,
                            merge_deserts = TRUE),
               subset(results_foo_data_nodf_md, select = -c(si_lat, si_long)),
               tolerance = .0001)
})
