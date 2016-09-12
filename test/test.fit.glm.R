source("../R/fit.glm.R")


test_that("A logistic regression model can be fit properly..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  model<- fit.gml(test.bh.data)
  expect_equal(model$aic, 4)
})