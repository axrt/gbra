source("../R/get.glm.formula.R")


test_that("A formula for a logistic regression is being assembled properly..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  expect_equal(test.bh.data %>% get.glm.formula, "cluster ~ X3+X6")
  
})