source("../R/append.cluster.R")


test_that("Clusters are correctly assigned to a cooked data frame with two genomes..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  expect_equal(test.bh.data %>% ncol, 3)
  expect_equal(test.bh.data[test.bh.data$X3==0,] %>% genome.ids.in.coocked.df, c(3))
})