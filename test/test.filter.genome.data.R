source("../R/filter.genome.data.R")


test_that("Genome orfs are being properly filtered from a coocked dataframe..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    attach.genomeid.header %>%
    sign.bh.table
  
  test.bh.data.genomes<- test.bh.data %>% filter.genome.data(orgs = c(6))
  
  get.genome.ids<- function(df){
    ids<- sapply(rownames(df), function(x){
      return(
        strsplit(x=x, split = "X", fixed=TRUE)[[1]][1]
      )
    })
    return(
      unique(ids)
    )
  }
  
  expect_equal(
    test.bh.data.genomes %>% get.genome.ids %>% as.numeric, c(6)
  )
  
  test.bh.data.genomes<- test.bh.data %>% filter.genome.data(orgs = c(6,3))
  
  expect_equal(
    test.bh.data.genomes %>% get.genome.ids %>% as.numeric, c(3,6)
  )
})