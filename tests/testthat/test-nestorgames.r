test_that("nestorgames jackets", {
	f <- sbgj_shibumi()
	expect_true(file.exists(f))
	unlink(f)
})
