test_that("`pcbj_fox_in_the_forest()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("Carl_Rungius_Red_Fox.jpg"))
	f <- pcbj_fox_in_the_forest(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})
