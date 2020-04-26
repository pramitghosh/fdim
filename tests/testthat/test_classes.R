context("Classes of objects returned by functions\n")
library('sameSVD')

test_that(desc = "Importing sf",
            {
              expect_is(sameSVD:::import_sf(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh"), "sf")
            }
          )

test_that(desc = "Overlaying grid",
            {
              expect_is(sameSVD:::overlay_grid(cs = 100000, f = sameSVD:::import_sf(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh")), "sfc_POLYGON")
            }
          )