source("../R/genome.ids.in.cooked.df.R")


test_that("A set of genome ids can be extracted from rownames correctly..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table
  
  expect_equal(test.bh.data %>% genome.ids.in.coocked.df, c(3,6))
  
})