context('data_package_download()')

test_that("Validate directory structure", {
  
  # Download data package to temporary directory and read
  
  suppressMessages(
    suppressWarnings(
      data_package_download(
        data.pkg.doi = 'doi:10.18739/A2DP3X'
      )
    )
  )
  
  # Check directory names
  
  expect_true(
    all(
      list.dirs(paste0(tempdir(), '/data_package'), recursive = FALSE, full.names = FALSE) %in% 
        c(
          'doi_10.18739_A2RV0D11H__Alaska_Schools_Rentention_Definitions__csv',
          'doi_10.18739_A2RV0D11H__Alaska_Schools_Rentention2009_15__csv',
          'doi_10.18739_A2RV0D11H__Alaska_Schools_Rentention2009_15__xlsx'
        )
    )
  )

  # Clean up
  
  suppressMessages(data_package_remove())
  
})
