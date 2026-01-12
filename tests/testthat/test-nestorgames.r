test_that("nestorgames jackets", {
	skip_if_not(has_runtime_dependencies())
	f <- sbgj_shibumi()
	expect_true(file.exists(f))
	unlink(f)
})
