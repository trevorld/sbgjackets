test_that("spirograph jacket", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("spiro.png"))
	f <- sbgj_spirograph()
	expect_true(file.exists(f))
	unlink(f)
})
