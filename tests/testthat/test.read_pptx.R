testthat::test_that(".pptx files load properly",{
  tpptx = read_pptx(pptx = system.file('extdata','example.pptx',package='readOffice'))
  expect_is(tpptx,"list")
  expect_is(tpptx[[1]],"list")
  expect_equal(unlist(purrr::map(tpptx,length),use.names = F),c(4,9,6,4,5))
  expect_equal(length(tpptx),5)
})

testthat::test_that("tables can be ignored",{
  tpptx = read_pptx(pptx = system.file('extdata','example.pptx',package='readOffice'),tables = F)
  expect_is(tpptx,"list")
  expect_is(tpptx[[1]],"list")
  expect_equal(unlist(purrr::map(tpptx,length),use.names = F),c(4,9,5,4,5))
  expect_equal(length(tpptx),3)
})

testthat::test_that("diagrams can be ignored",{
  tpptx = read_pptx(pptx = system.file('extdata','example.pptx',package='readOffice'),diagrams = F)
  expect_is(tpptx,"list")
  expect_is(tpptx[[1]],"list")
  expect_equal(unlist(purrr::map(tpptx,length),use.names = F),c(4,9,6,3,3))
  expect_equal(length(tpptx),2)
})

testthat::test_that("drawings can be ignored",{
  tpptx = read_pptx(pptx = system.file('extdata','example.pptx',package='readOffice'),drawings = F)
  expect_is(tpptx,"list")
  expect_is(tpptx[[1]],"list")
  expect_equal(unlist(purrr::map(tpptx,length),use.names = F),c(4,4,6,4,5))
  expect_equal(length(tpptx),3)
})

testthat::test_that(".ppt files don't load",{
  tpptx = try(read_pptx(pptx = system.file('extdata','example.ppt',package='readOffice')),silent = T)
  expect_is(tpptx,"try-error")
})
