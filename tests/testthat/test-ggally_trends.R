test_that("example", {
  data(tips)

  ggally_expect_doppelganger(
    "point",
    ggplot(tips) +
      aes(x = day, y = total_bill) +
      geom_point()
  )

  ggally_expect_doppelganger(
    "geom-default",
    ggplot(tips) +
      aes(x = day, y = total_bill) +
      stat_weighted_mean()
  )

  ggally_expect_doppelganger(
    "geom-line",
    ggplot(tips) +
      aes(x = day, y = total_bill, group = 1) +
      stat_weighted_mean(geom = "line")
  )

  ggally_expect_doppelganger(
    "geom-line-grouped",
    ggplot(tips) +
      aes(x = day, y = total_bill, colour = sex, group = sex) +
      stat_weighted_mean(geom = "line")
  )

  ggally_expect_doppelganger(
    "geom-bar-dodge",
    ggplot(tips) +
      aes(x = day, y = total_bill, fill = sex) +
      stat_weighted_mean(geom = "bar", position = "dodge")
  )

  # computing a proportion on the fly
  ggally_expect_doppelganger(
    "geom-bar-dodge-percent",
    ggplot(tips) +
      aes(x = day, y = as.integer(smoker == "Yes"), fill = sex) +
      stat_weighted_mean(geom = "bar", position = "dodge") +
      scale_y_continuous(labels = scales::label_percent())
  )

  # taking into account some weights
  d <- as.data.frame(Titanic)
  ggally_expect_doppelganger(
    "titanic",
    ggplot(d) +
      aes(
        x = Class,
        y = as.integer(Survived == "Yes"),
        weight = Freq,
        fill = Sex
      ) +
      geom_bar(stat = "weighted_mean", position = "dodge") +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(y = "Survived")
  )

  tips_f <- tips
  tips_f$day <- factor(tips$day, c("Thur", "Fri", "Sat", "Sun"))

  # Numeric variable
  ggally_expect_doppelganger(
    "trends",
    ggally_trends(tips_f, mapping = aes(x = day, y = total_bill))
  )
  ggally_expect_doppelganger(
    "trends-color",
    ggally_trends(tips_f, mapping = aes(x = day, y = total_bill, colour = time))
  )

  # Binary variable
  ggally_expect_doppelganger(
    "trends-binary",
    ggally_trends(tips_f, mapping = aes(x = day, y = smoker))
  )
  ggally_expect_doppelganger(
    "trends-binary-color",
    ggally_trends(tips_f, mapping = aes(x = day, y = smoker, colour = sex))
  )

  # Discrete variable with 3 or more categories
  ggally_expect_doppelganger(
    "trends-many",
    ggally_trends(tips_f, mapping = aes(x = smoker, y = day))
  )
  ggally_expect_doppelganger(
    "trends-many-color",
    ggally_trends(tips_f, mapping = aes(x = smoker, y = day, color = sex))
  )

  # Include zero on Y axis
  ggally_expect_doppelganger(
    "trends-incl-zero-false",
    ggally_trends(
      tips_f,
      mapping = aes(x = day, y = total_bill),
      include_zero = TRUE
    )
  )
  ggally_expect_doppelganger(
    "trends-incl-zero-true",
    ggally_trends(
      tips_f,
      mapping = aes(x = day, y = smoker),
      include_zero = TRUE
    )
  )

  # Change line size
  ggally_expect_doppelganger(
    "trends-size-3",
    ggally_trends(
      tips_f,
      mapping = aes(x = day, y = smoker, colour = sex),
      linewidth = 3
    )
  )

  # Define weights with the appropriate aesthetic
  d <- as.data.frame(Titanic)
  ggally_expect_doppelganger(
    "trends-titanic",
    ggally_trends(
      d,
      mapping = aes(x = Class, y = Survived, weight = Freq, color = Sex),
      include_zero = TRUE
    )
  )
})
