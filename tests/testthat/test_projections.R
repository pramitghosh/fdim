context("Projections\n")
library('sameSVD')
library('sf')
library('rnaturalearth')

deutschland = ne_countries(scale = "medium", country = "Germany", returnclass = "sf")

test_that(desc = "Importing sf with long/lat CRS",
            {
              expect_equal(st_is_longlat(sameSVD:::import_sf(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh")), FALSE)
              expect_equal(st_crs(sameSVD:::import_sf(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh"))$input, "EPSG:3857")
            }
          )

test_that(desc = "Reading sf object with long/lat CRS",
            {
              expect_equal(st_crs(import_SVD(x = deutschland))$input, "EPSG:3857")
            }
          )

test_that(desc = "Matching projections of sf and grid",
            {
              expect_equal(st_crs(sameSVD:::import_sf(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh"))$input, st_crs(sameSVD:::overlay_grid(cs = 100000, f = sameSVD:::import_sf(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh")))$input)
            }
          )