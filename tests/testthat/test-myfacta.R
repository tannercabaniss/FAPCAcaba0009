test_that("Error catching", {
  expect_error(myfacta(m=1)) # No S or R provided
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE))) # No m provided
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE), # Both S and R provided
                       R=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE),
                       m=1))
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE), # Not a positive integer
                       m=0))
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE), # Not a positive integer
                       m=-1))
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE), # Not less than p=3
                       m=4))
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3, byrow=TRUE), # Not an integer
                       m=2.3))
  expect_error(myfacta(S=matrix(c(1,0,0,0,1,0,0,0,1,0), nrow=5, ncol=2, byrow=TRUE), # S not a square, symmetric matrix
                       m=2))
  expect_error(myfacta(R=matrix(c(1,0,0,0,0.5,0,0,0,1), nrow=3, ncol=3, byrow=TRUE), # Invalid correlation matrix
                       m=2))
})
