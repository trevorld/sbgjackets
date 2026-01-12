test_that("`pcbj_pinochle()`", {
	skip_if_not_installed("gtable")
	skip_if_not(cache_has("pinochle.jpg"))
	f <- pcbj_pinochle()
	expect_true(file.exists(f))
	unlink(f)
})
