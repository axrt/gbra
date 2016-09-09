source("../R/sample.data.2.R")


test_that("A cooked dataframe can be sampled prperly and returned with the validation part..",{
  
  #get some sample data
  test.bh.data <- read.bhs(bh.folder = "../inst/extradata", ext = ".short") %>% 
    data.frame %>%
    normalize.scores %>%
    sign.bh.table %>%
    append.cluster
  
  ncluster.one<- test.bh.data %>% filter(cluster==TRUE) %>% nrow
  ncluster.two<- test.bh.data %>% filter(cluster==FALSE) %>% nrow
  
  test.bh.data %>% sample.data.2 -> partitioned.data
  partitioned.data$train %>%
    group_by(cluster) %>%
    do({
      expect_true(nrow(.) %in% sapply((c(ncluster.one, ncluster.two)*0.75),floor))
      return(.)
    })
  partitioned.data$validate %>%
    group_by(cluster) %>%
    do({
      expect_true(nrow(.) %in% sapply((c(ncluster.one, ncluster.two)*0.25),ceiling))
      return(.)
    })
})