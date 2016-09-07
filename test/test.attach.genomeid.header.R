source("../R/attach.genomeid.header.R")

test_that("GenomeID header can be properly attached to a normalized bh data.frame..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores
  
  test.bh.data.named<- test.bh.data %>% attach.genomeid.header
  expect_true(
    all(
      sapply(colnames(.), function(x){
        return(
          substr(x,1,2)=="X"
        )
      })
      )
    )
  num.3<- test.bh.data %>% filter(ID_QUERY_GENOME==3) %>% nrow
  num.5<- test.bh.data %>% filter(ID_QUERY_GENOME==5) %>% nrow
  })