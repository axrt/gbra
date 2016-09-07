source("../R/read.bhs.R")

test_that("All bhs-files are being read properly and form a proper hit table..", {
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short")
  #must have a special header like below
  expect_equal(
    c(
      "QUERY_ORF_ID",
      "ID_QUERY_GENOME",
      "3","6"
    ),
    colnames(test.bh.data)
  )
  expect_equal(4, ncol(test.bh.data))
  expect_equal(94, nrow(test.bh.data))
})