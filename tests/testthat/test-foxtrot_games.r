test_that("`pcbj_fox_in_the_forest()`", {
	skip_if_not(cache_has("Carl_Rungius_Red_Fox.jpg"))
	f <- pcbj_fox_in_the_forest()
	expect_true(file.exists(f))
	unlink(f)
})
