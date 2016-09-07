source("../R/restrict.minimal.hits.R")
source("../R/read.bhs.R")

test_that("Hits can be restricted as planned..",{
  
  #get some sample data
  #it will be read as data.table, so convert to data.frame first
  test.df<- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% data.frame
  #add some artificial columns
  test.df<- test.df %>% mutate(TEST= rep(c(0,1), nrow(test.df)/2))
  #now expect that every other row will be thrown out
  test.df.restricted <- restrict.minimal.hits(df = test.df, minhit = 2)
  
  expect_equal(47, nrow(test.df.restricted))
  
})