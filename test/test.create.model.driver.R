source("../R/create.model.driver.R")
source("../R/legend.R")

test_that("A driver for model generation can be assembled properly..",{
  
  legend<- load.legend("../inst/extradata/legend.csv", sep="\t")
  model.drv<- create.model.driver(legend = legend)
  len<- nrow(legend)
  len<- (len^2-len)/2 #upper triangle of the matrix
  expect_equal(len, nrow(model.drv))
  expect_equal(
    setdiff(
      unique(model.drv$X1), unique(model.drv$X2)
      ),
    1
    )
})