test_that("nestorgames jackets", {
	skip_if_not(has_runtime_dependencies())

	skip_if_not(cache_has("keks_photo.jpg"))
	f <- sbgj_nestortiles()
	expect_true(file.exists(f))
	unlink(f)

	f <- sbgj_shibumi()
	expect_true(file.exists(f))
	unlink(f)
})
