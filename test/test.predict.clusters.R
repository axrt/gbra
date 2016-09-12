source("../R/predict.clusters.R")


test_that("Clusters can be predicted given a trained model..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  model<- test.bh.data %>% fit.glm
  predicted.data<- model %>% predict.clusters(data.rest=test.bh.data)
  expect_equal(predicted.data$cluster, predicted.data$predicted)
})