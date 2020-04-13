context("Test numeric outputs\n")
library('fdim')

test_grids = lapply(as.list(seq(25000, 100000, 25000)), fdim:::overlay_grid, fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh"))
cell_counts = as.numeric(lapply(test_grids, fdim:::count_cells, fdim:::import_sf(dsn = system.file(package = "fdim"), layer = "madhya_pradesh")))

test_that(desc = "Cell counts",
            {
              expect_identical(cell_counts, c(703, 196, 97, 57))
            }
          )

test_that(desc = "Box-Counting Dimension",
            {
              expect_less_than(abs(bcd(dsn = system.file(package = "fdim"), layer = "madhya_pradesh") - 1.834641), 1/1000000)
            }
          )
