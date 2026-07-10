test_that("protect_url_wrap() inserts a word joiner after the URL scheme", {
	expect_equal(
		protect_url_wrap("CC BY-SA 4.0: https://creativecommons.org/licenses/by-sa/4.0/"),
		"CC BY-SA 4.0: https://\u2060creativecommons.org/licenses/by-sa/4.0/"
	)
	expect_equal(
		protect_url_wrap("See http://example.com"),
		"See http://\u2060example.com"
	)
	expect_equal(protect_url_wrap("Personal Use Only"), "Personal Use Only")
})
