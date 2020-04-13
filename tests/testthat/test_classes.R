context("Classes of objects returned by functions")
library('fdim')

test_that(desc = "Importing sf",
            {
              expect_is(fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh"), "sf")
            }
          )

test_that(desc = "Overlaying grid",
            {
              expect_is(fdim:::overlay_grid(cs = 100000, f = fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh")), "sfc_POLYGON")
            }
          )