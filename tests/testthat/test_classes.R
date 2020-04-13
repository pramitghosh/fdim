context("Classes of objects returned by functions")
library('fdim')

test_that(desc = "Importing sf",
            {
              expect_is(st_is_longlat(fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh")), FALSE)
            }
          )