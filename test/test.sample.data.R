source("../R/sample.data.R")


test_that("A cooked dataframe can be sampled prperly..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  ncluster.one<- test.bh.data %>% filter(cluster==TRUE) %>% nrow
  ncluster.two<- test.bh.data %>% filter(cluster==FALSE) %>% nrow
  
  test.bh.data %>%
    group_by(cluster) %>%
    do({
      expect_true(nrow(.) %in% c(ncluster.one, ncluster.two))
      return(.)
    })
})