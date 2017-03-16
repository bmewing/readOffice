testthat::test_that(".docx files load properly",{
  tdocx = read_docx(docx = system.file('extdata','example.docx',package='readOffice'))
  expect_is(tdocx,"list")
  expect_is(tdocx[[1]],"list")
  expect_equal(unlist(purrr::map(tdocx,length),use.names = F),c(10,11,6))
  expect_equal(length(tdocx),3)
})

testthat::test_that("tables can be ignored",{
  tdocx = read_docx(docx = system.file('extdata','example.docx',package='readOffice'),tables = F)
  expect_is(tdocx,"list")
  expect_is(tdocx[[1]],"list")
  expect_equal(unlist(purrr::map(tdocx,length),use.names = F),c(9,11,6))
  expect_equal(length(tdocx),3)
})

testthat::test_that("diagrams can be ignored",{
  tdocx = read_docx(docx = system.file('extdata','example.docx',package='readOffice'),diagrams = F)
  expect_is(tdocx,"list")
  expect_is(tdocx[[1]],"list")
  expect_equal(unlist(purrr::map(tdocx,length),use.names = F),c(10,11))
  expect_equal(length(tdocx),2)
})

testthat::test_that("drawings can be ignored",{
  tdocx = read_docx(docx = system.file('extdata','example.docx',package='readOffice'),drawings = F)
  expect_is(tdocx,"list")
  expect_is(tdocx[[1]],"list")
  expect_equal(unlist(purrr::map(tdocx,length),use.names = F),c(8,10,6))
  expect_equal(length(tdocx),3)
})

testthat::test_that(".doc files don't load",{
  tdocx = try(read_docx(docx = system.file('extdata','example.doc',package='readOffice')),silent = T)
  expect_is(tdocx,"try-error")
})
