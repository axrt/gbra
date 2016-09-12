source("../R/get.rsq.R")


test_that("R squared can be drawn from a model properly..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  model<- test.bh.data %>% fit.glm
  expect_equal(model %>% get.rsq %>% as.numeric, 1)
  
  
})