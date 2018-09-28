# https://github.com/jimhester/lintr
if (requireNamespace("lintr", quietly = TRUE)) {
    context("lints")

    line_exclusions <- c("src/RcppExports.cpp", "R/RcppExports.R")

    test_that("package Style", {
        lintr::expect_lint_free(cache = TRUE, exclusions = line_exclusions)
    })
}
