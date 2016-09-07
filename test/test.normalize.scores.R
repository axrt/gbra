source("../R/normalize.scores.R")

test_that("The bh data can be normalized to min 0 :: max 1..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% data.frame
  #normalize the data
  test.bh.data.norm<- test.bh.data %>% normalize.scores
  
  expect_true(min(test.bh.data.norm[,3:4])<=0)
  expect_true(max(test.bh.data.norm[,3:4])>=1)
})