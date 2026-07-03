test_that("`pcw_blorg_in_the_midwest()`", {
	skip_if_not(has_runtime_dependencies())
	f <- pcw_blorg_in_the_midwest()
	expect_true(file.exists(f))
	unlink(f)
})
