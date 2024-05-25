context("Test numeric outputs\n")
library('sameSVD')

test_grids = lapply(as.list(seq(25000, 100000, 25000)), sameSVD:::overlay_grid, import_SVD(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh"))
cell_counts = as.numeric(lapply(test_grids, sameSVD:::count_cells, import_SVD(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh")))

# MP_sa = import_SVD(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh") %>% bcd(l = matrix(rep(seq(10000, 100000, 10000), 2), ncol = 2))

test_that(desc = "Cell counts",
            {
              expect_identical(cell_counts, c(703, 196, 97, 57))
            }
          )

test_that(desc = "Box-Counting Dimension",
            {
              expect_lt(abs((import_SVD(dsn = system.file(package = "sameSVD"), layer = "madhya_pradesh") %>% bcd(type = "self_similarity")) - 1.834641), 1/1000000)
            }
          )

# test_that(desc = "Box-Counting Self-affinity",
#             {
#               expect_lt(abs(MP_sa[1] - 0.91), 1/100)
#               expect_lt(abs(MP_sa[2] - 0.90), 1/100)
#             }
#           )