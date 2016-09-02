source("../R/read.bh.R")

test_that("loading a bh data sample file results in what we expet it to..", {
  #get some sample data
  test.bh.data <- read.bh.file(qgen.id = 6,
                               tgen.id = 3,
                               bh.folder = "../inst/extradata")
  #must have a special header like below
  expect_equal(
    c(
      "QUERY_ORF_ID",
      "COMULATIVE_BITSCORE",
      "ID_QUERY_GENOME",
      "ID_TARGET_GENOME"
    ),
    colnames(test.bh.data)
  )
  expect_equal(41, nrow(test.bh.data))
  expect_equal(4, ncol(test.bh.data))
})
