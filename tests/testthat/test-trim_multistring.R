test_that("trim_multistring() removes common leading tabs", {
	x <- "\n\t\tline1\n\t\t\tline2\n\t\tline3\n"
	expect_equal(trim_multistring(x), "line1\n\tline2\nline3")
})

test_that("trim_multistring() handles single-level indentation", {
	x <- "\n\ta\n\tb\n"
	expect_equal(trim_multistring(x), "a\nb")
})
