test_that("`pcbj_qwixx()`", {
	skip_if_not(has_runtime_dependencies())
	f <- pcbj_qwixx(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})
