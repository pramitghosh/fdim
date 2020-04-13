context("Projections\n")
library('fdim')
library('sf')

test_that(desc = "Importing sf with long/lat CRS",
            {
              expect_equal(st_is_longlat(fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh")), FALSE)
              expect_equal(st_crs(fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh"))$input, "EPSG:3857")
            }
          )

test_that(desc = "Matching projections of sf and grid",
            {
              expect_equal(st_crs(fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh"))$input, st_crs(fdim:::overlay_grid(cs = 100000, f = fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh")))$input)
            }
          )