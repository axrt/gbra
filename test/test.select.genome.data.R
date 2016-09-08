source("../R/select.genome.data.R")


test_that("Genomes are being properly selected from a coocked dataframe..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table
  
  test.bh.data.select<- test.bh.data %>% select.genome.data(orgs=c(3))
  
  expect_equal(ncol(test.bh.data.select),1)
  
  test.bh.data.select<- test.bh.data %>% select.genome.data(orgs=c(3), exclude = FALSE)
  
  expect_equal(ncol(test.bh.data.select),1)
})