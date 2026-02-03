test_that("GIPF jackets", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("yinsh_bgg_image.jpg"))
	f <- sbgj_yinsh()
	expect_true(file.exists(f))
	unlink(f)
})
