test_that("Tables", {
	skip_if_not_installed("knitr")
	skip_if_not_installed("tibble")

	md_sbgj <- readme_markdown_table(df_sbgj())
	md_pcbj <- readme_markdown_table(df_pcbj())
	expect_s3_class(md_sbgj, "knitr_kable")
	expect_s3_class(md_pcbj, "knitr_kable")

	expect_true(nrow(df_sbgj()) >= 9L)
	expect_true(nrow(df_pcbj()) >= 11L)
})
