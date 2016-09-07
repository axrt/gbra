source("../R/sign.bh.table.R")


test_that("Genome ID is being properly attached to the rownames..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    attach.genomeid.header
  
  #attach genome ids to proper orf ids
  test.bh.data.genome.id<- test.bh.data %>% sign.bh.table
  
  expect_true(
    all(
      sapply(rownames(test.bh.data.genome.id), function(x){
        return(grepl(pattern = "X", x = x, fixed = TRUE))
      })
    )
  )
  
})