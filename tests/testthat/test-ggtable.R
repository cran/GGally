suppressMessages(require(broom))

test_that("example", {
  skip_if_not_installed("Hmisc")
  reg <- lm(
    Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
    data = iris
  )
  ggally_expect_doppelganger("lm", ggcoef(reg))

  data(tips)
  ggally_expect_doppelganger(
    "tips",
    ggtable(tips, "smoker", c("day", "time", "sex"))
  )

  # displaying row proportions
  ggally_expect_doppelganger(
    "tips-cells",
    ggtable(tips, "smoker", c("day", "time", "sex"), cells = "row.prop")
  )

  # filling cells with residuals
  ggally_expect_doppelganger(
    "tips-fill-std_resid",
    ggtable(
      tips,
      "smoker",
      c("day", "time", "sex"),
      fill = "std.resid",
      legend = 1
    )
  )
  ggally_expect_doppelganger(
    "tips-fill-resid",
    ggtable(tips, "smoker", c("day", "time", "sex"), fill = "resid", legend = 1)
  )

  # if continuous variables are provided, just displaying some summary statistics
  ggally_expect_doppelganger(
    "tips-continuous",
    ggtable(tips, c("smoker", "total_bill"), c("day", "time", "sex", "tip"))
  )

  # specifying weights
  d <- as.data.frame(Titanic)
  ggally_expect_doppelganger(
    "titanic-weight-freq",
    ggtable(
      d,
      "Survived",
      c("Class", "Sex", "Age"),
      mapping = aes(weight = Freq),
      cells = "row.prop",
      fill = "std.resid"
    )
  )
})
