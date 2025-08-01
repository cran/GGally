set.seed(123)
data(diamonds, package = "ggplot2")
diamonds.samp <- diamonds[sample(1:dim(diamonds)[1], 100), ]

iris2 <- iris
iris2$alphaLevel <- c("setosa" = 0.2, "versicolor" = 0.3, "virginica" = 0)[
  iris2$Species
]

test_that("stops", {
  # basic parallel coordinate plot, using default settings
  # ggparcoord(data = diamonds.samp, columns = c(1, 5:10))
  # this time, color by diamond cut
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = NULL,
      order = "anyClass"
    ),
    "can't use the 'order' methods "
  )
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = NULL,
      order = "allClass"
    ),
    "can't use the 'order' methods "
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = c(1, 2)
    ),
    "invalid value for 'groupColumn'"
  )
  expect_error(
    ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 1i),
    "invalid value for 'groupColumn'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      scale = "notValid"
    ),
    "invalid value for 'scale'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      centerObsID = nrow(diamonds.samp) + 10
    ),
    "invalid value for 'centerObsID'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      missing = "notValid"
    ),
    "invalid value for 'missing'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      order = "notValid"
    ),
    "invalid value for 'order'"
  )
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      order = 1i
    ),
    "invalid value for 'order'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      showPoints = 1
    ),
    "invalid value for 'showPoints'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      alphaLines = "notAColumn"
    ),
    "'alphaLines' column is missing in data"
  )
  tmpDt <- diamonds.samp
  tmpDt$price[1] <- NA
  range(tmpDt$price)
  expect_error(
    ggparcoord(
      data = tmpDt,
      columns = c(1, 5:10),
      groupColumn = 2,
      alphaLines = "price"
    ),
    "missing data in 'alphaLines' column"
  )
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      alphaLines = "price"
    ),
    "invalid value for 'alphaLines' column; max range "
  )
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      alphaLines = -0.1
    ),
    "invalid value for 'alphaLines'; must be a scalar value"
  )
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      alphaLines = 1.1
    ),
    "invalid value for 'alphaLines'; must be a scalar value"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      boxplot = 1
    ),
    "invalid value for 'boxplot'"
  )

  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      shadeBox = c(1, 2)
    ),
    "invalid value for 'shadeBox'; must be a single color"
  )
  expect_error(
    ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      shadeBox = "notacolor"
    ),
    "invalid value for 'shadeBox'; must be a valid R color"
  )

  expect_error(
    ggparcoord(
      diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      splineFactor = NULL
    ),
    "invalid value for 'splineFactor'"
  )
})

test_that("alphaLines", {
  p <- ggparcoord(
    data = iris2,
    columns = 1:4,
    groupColumn = 5,
    order = "anyClass",
    showPoints = TRUE,
    title = "Parallel Coordinate Plot for the Iris Data",
    alphaLines = "alphaLevel"
  )
  expect_equal(length(p$layers), 2)
  expect_equal(
    mapping_string(get("mapping", envir = p$layers[[1]])$alpha),
    "alphaLevel"
  )
})

test_that("splineFactor", {
  ## Use splines on values, rather than lines (all produce the same result)
  columns <- c(1, 5:10)
  p1 <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = TRUE)
  p2 <- ggparcoord(diamonds.samp, columns, groupColumn = 2, splineFactor = 3)

  pList <- list(p1, p2)
  for (p in pList) {
    expect_equal(
      mapping_string(get("mapping", envir = p$layers[[1]])$x),
      "spline.x"
    )
    expect_equal(
      mapping_string(get("mapping", envir = p$layers[[1]])$y),
      "spline.y"
    )

    tmp <- unique(as.numeric(
      get("data", envir = p$layers[[1]])$ggally_splineFactor
    ))
    expect_true((tmp == 3) || (tmp == 21))
  }

  p <- ggparcoord(
    data = iris2,
    columns = 1:4,
    groupColumn = 5,
    splineFactor = 3,
    alphaLines = "alphaLevel"
  )
  expect_equal(
    mapping_string(get("mapping", p$layers[[1]])$alpha),
    "alphaLevel"
  )

  p <- ggparcoord(
    data = iris2,
    columns = 1:4,
    groupColumn = 5,
    splineFactor = 3,
    showPoints = TRUE
  )
  expect_equal(length(p$layers), 2)
  expect_equal(mapping_string(get("mapping", p$layers[[1]])$x), "spline.x")
  expect_equal(mapping_string(get("mapping", p$layers[[2]])$y), "value")
})

test_that("splineFactor as is", {
  iris2 <- iris
  iris2$alphaLevel <- c("setosa" = 0.2, "versicolor" = 0.3, "virginica" = 0)[
    iris2$Species
  ]

  k <- 4
  p_no_visible_spline <- ggparcoord(
    data = iris2,
    columns = seq_len(k),
    groupColumn = 5,
    splineFactor = I(k)
  )
  p_single_split_between <- ggparcoord(
    data = iris2,
    columns = seq_len(k),
    groupColumn = 5,
    splineFactor = I(2 * k)
  )

  ggally_expect_doppelganger(
    "ggparcoord-splineFactor-as-is-4",
    p_no_visible_spline
  )
  ggally_expect_doppelganger(
    "ggparcoord-splineFactor-as-is-8",
    p_single_split_between
  )
})

test_that("groupColumn", {
  ds2 <- diamonds.samp
  ds2$color <- mapping_string(ds2$color)

  # column 3 has a character
  # column 4 has a factor
  p <- ggparcoord(data = ds2, columns = c(1, 3:10), groupColumn = 2)
  expect_true("color" %in% levels(p$data$variable))
  expect_true("clarity" %in% levels(p$data$variable))
  expect_true(is.numeric(p$data$value))
  expect_equal(mapping_string(p$mapping$colour), colnames(ds2)[2])

  p <- ggparcoord(
    data = ds2,
    columns = c(
      "carat",
      "color",
      "clarity",
      "depth",
      "table",
      "price",
      "x",
      "y",
      "z"
    ),
    order = c(1, 3:10),
    groupColumn = "cut"
  )
  expect_true("color" %in% levels(p$data$variable))
  expect_true("clarity" %in% levels(p$data$variable))
  expect_true(is.numeric(p$data$value))
  expect_equal(levels(p$data$cut), levels(ds2$cut))

  # group column is a regular column
  ## factor
  # p <- ggparcoord(data = ds2, columns = c(1, 3:10), groupColumn = 4)
  # expect_true("clarity" %in% levels(p$data$variable))
  ## character
  # p <- ggparcoord(data = ds2, columns = c(1, 3:10), groupColumn = 3)
  # expect_true("color" %in% levels(p$data$variable))
  ## numeric
  # p <- ggparcoord(data = ds2, columns = c(1, 3:10), groupColumn = 1)
  # expect_true("carat" %in% levels(p$data$variable))
})

test_that("scale", {
  for (scale in c(
    "std",
    "robust",
    "uniminmax",
    "globalminmax",
    "center",
    "centerObs"
  )) {
    p <- ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      scale = scale
    )
  }
  expect_true(TRUE)
})

test_that("missing", {
  ds2 <- diamonds.samp
  ds2[3, 1] <- NA

  for (missing in c("exclude", "mean", "median", "min10", "random")) {
    p <- ggparcoord(
      data = ds2,
      columns = c(1, 5:10),
      groupColumn = 2,
      missing = missing
    )
  }
  expect_true(TRUE)
})

test_that("order", {
  if (requireNamespace("scagnostics", quietly = TRUE)) {
    for (ordering in c(
      "Outlying",
      "Skewed",
      "Clumpy",
      "Sparse",
      "Striated",
      "Convex",
      "Skinny",
      "Stringy",
      "Monotonic"
    )) {
      p <- ggparcoord(
        data = diamonds.samp,
        columns = c(1, 5:10),
        groupColumn = 2,
        order = ordering
      )
      expect_true(all(
        levels(p$data) != c("carat", "depth", "table", "price", "x", "y", "z")
      ))
    }
  }

  for (ordering in c("skewness", "allClass", "anyClass")) {
    p <- ggparcoord(
      data = diamonds.samp,
      columns = c(1, 5:10),
      groupColumn = 2,
      order = ordering
    )
    expect_true(all(
      levels(p$data) != c("carat", "depth", "table", "price", "x", "y", "z")
    ))
  }
})

test_that("missing and order(anyClass)", {
  ds2 <- diamonds.samp
  ds2[3, 1] <- NA
  missing_options <- c("exclude", "mean", "median", "min10", "random")

  for (missing in missing_options) {
    p <- ggparcoord(
      data = ds2,
      columns = c(1, 5:10),
      groupColumn = 2,
      missing = missing,
      order = "anyClass"
    )
  }
  expect_true(TRUE)
})

test_that("basic", {
  # no color supplied
  p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10))
  expect_true(is.null(p$mapping$colour))

  # color supplied
  p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), groupColumn = 2)
  expect_false(is.null(p$mapping$colour))

  # title supplied
  ttl <- "Parallel Coord. Plot of Diamonds Data"
  p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), title = ttl)
  expect_equal(get_labs(p)$title, ttl)

  col <- "blue"
  p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10), shadeBox = col)
  expect_equal(length(p$layers), 2)
  expect_equal(get("aes_params", envir = p$layers[[1]])$colour, col)

  p <- ggparcoord(
    data = diamonds.samp,
    columns = c(1, 5:10),
    mapping = ggplot2::aes(size = 1)
  )
  expect_equal(length(p$layers), 1)
  expect_equal(p$mapping$size, 1)
})


test_that("size", {
  p <- ggparcoord(
    data = diamonds.samp,
    columns = c(1, 5:10),
    mapping = ggplot2::aes(size = gear)
  )
  expect_equal(mapping_string(p$mapping$size), "gear")

  p <- ggparcoord(data = diamonds.samp, columns = c(1, 5:10)) +
    ggplot2::aes(size = gear)
  expect_equal(mapping_string(p$mapping$size), "gear")
})


test_that("columns containing only a single value do not cause an scaling error", {
  df <- data.frame(obs = 1:5, var1 = sample(10, 5), var2 = rep(3, 5))

  # no scaling
  expect_silent(ggparcoord(data = df, columns = 1:3, scale = "globalminmax"))
  # requires scaling, must not throw an errror due to scaling the single values (to NaN)
  expect_silent(ggparcoord(data = df, columns = 1:3, scale = "uniminmax"))

  df2 <- data.frame(df, var3 = factor(c("a", "b", "c", "a", "c")))
  # requires scaling, must not throw an errror due to scaling the single values (to NaN)
  expect_silent(ggparcoord(data = df2, columns = 1:4, scale = "uniminmax"))

  df3 <- data.frame(df2, var4 = factor(c("d", "d", "d", "d", "d")))
  expect_silent(ggparcoord(data = df3, columns = 1:4, scale = "uniminmax"))
  expect_silent(ggparcoord(data = df3, columns = 1:4, scale = "robust"))
  expect_silent(ggparcoord(data = df3, columns = 1:4, scale = "std"))
})
