test_that("`pcbj_wizard()`", {
	skip_if_not(has_runtime_dependencies())
	skip_if_not(cache_has("wizard_blue_bird.jpg"))
	f <- pcbj_wizard(double = TRUE)
	expect_true(file.exists(f))
	unlink(f)
})
