## ------------------------------------------------------------------------
library(GGally)
library(ggplot2)
reg <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
ggcoef(reg)

## ------------------------------------------------------------------------
d <- as.data.frame(Titanic)
log.reg <- glm(Survived ~ Sex + Age + Class, family = binomial, data = d, weights = d$Freq)
ggcoef(log.reg, exponentiate = TRUE)

## ------------------------------------------------------------------------
ggcoef(reg, vline = FALSE, conf.int = FALSE, exclude_intercept = TRUE)

## ------------------------------------------------------------------------
ggcoef(log.reg, exponentiate = TRUE, vline_color = "red", vline_linetype =  "solid", errorbar_color = "blue", errorbar_height = .25)

## ------------------------------------------------------------------------
ggcoef(log.reg, exponentiate = TRUE, color = "purple", size = 5, shape = 18)

## ------------------------------------------------------------------------
ggcoef(log.reg, exponentiate = TRUE, mapping = aes(x = estimate, y = term, size = p.value)) +
  scale_size_continuous(trans = "reverse")

## ------------------------------------------------------------------------
cust <- data.frame(
  term = c("male vs. female", "30-49 vs. 18-29", "50+ vs. 18-29", "urban vs. rural"),
  estimate = c(.456, 1.234, 1.897, 1.003),
  conf.low = c(.411, 1.042, 1.765, 0.678),
  conf.high = c(.498, 1.564, 2.034, 1.476),
  variable = c("sex", "age", "age", "residence")
)
cust$term <- factor(cust$term, cust$term)
ggcoef(cust, exponentiate = TRUE)
ggcoef(cust, exponentiate = TRUE, mapping = aes(x = estimate, y = term, colour = variable), size = 5)

