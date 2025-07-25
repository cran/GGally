#' Survival curves
#'
#' This function produces Kaplan-Meier plots using \pkg{ggplot2}.
#' As a first argument it needs a \code{survfit} object, created by the
#' \code{survival} package. Default settings differ for single stratum and
#' multiple strata objects.
#'
#' @export
#' @param s an object of class \code{survfit}
#' @param CI should a confidence interval be plotted? Defaults to \code{TRUE}
#'    for single stratum objects and \code{FALSE} for multiple strata objects.
#' @param plot.cens mark the censored observations?
#' @param surv.col colour of the survival estimate. Defaults to black for
#'    one stratum, and to the default \pkg{ggplot2} colours for multiple
#'    strata. Length of vector with colour names should be either 1 or equal
#'    to the number of strata.
#' @param cens.col colour of the points that mark censored observations.
#' @param lty.est linetype of the survival curve(s). Vector length should be
#'    either 1 or equal to the number of strata.
#' @param lty.ci linetype of the bounds that mark the 95% CI.
#' @param size.est line width of the survival curve
#' @param size.ci line width of the 95% CI
#' @param cens.size point size of the censoring points
#' @param cens.shape shape of the points that mark censored observations.
#' @param back.white if TRUE the background will not be the default
#'    grey of \code{ggplot2} but will be white with borders around the plot.
#' @param xlab the label of the x-axis.
#' @param ylab the label of the y-axis.
#' @param main the plot label.
#' @param order.legend boolean to determine if the legend display should be ordered by final survival time
#' @return An object of class \code{ggplot}
#' @author Edwin Thoen
#' @importFrom stats time
#' @examples
#' # Small function to display plots only if it's interactive
#' p_ <- GGally::print_if_interactive
#'
#' if (require(survival) && require(scales)) {
#'   lung <- survival::lung
#'   sf.lung <- survival::survfit(Surv(time, status) ~ 1, data = lung)
#'   p_(ggsurv(sf.lung))
#'
#'   # Multiple strata examples
#'   sf.sex <- survival::survfit(Surv(time, status) ~ sex, data = lung)
#'   pl.sex <- ggsurv(sf.sex)
#'   p_(pl.sex)
#'
#'   # Adjusting the legend of the ggsurv fit
#'   p_(pl.sex +
#'     ggplot2::guides(linetype = "none") +
#'     ggplot2::scale_colour_discrete(
#'       name   = "Sex",
#'       breaks = c(1, 2),
#'       labels = c("Male", "Female")
#'     ))
#'
#'   # Multiple factors
#'   lung2 <- dplyr::mutate(lung, older = as.factor(age > 60))
#'   sf.sex2 <- survival::survfit(Surv(time, status) ~ sex + older, data = lung2)
#'   pl.sex2 <- ggsurv(sf.sex2)
#'   p_(pl.sex2)
#'
#'   # Change legend title
#'   p_(pl.sex2 + labs(color = "New Title", linetype = "New Title"))
#'
#'   # We can still adjust the plot after fitting
#'   kidney <- survival::kidney
#'   sf.kid <- survival::survfit(Surv(time, status) ~ disease, data = kidney)
#'   pl.kid <- ggsurv(sf.kid, plot.cens = FALSE)
#'   p_(pl.kid)
#'
#'   # Zoom in to first 80 days
#'   p_(pl.kid + ggplot2::coord_cartesian(xlim = c(0, 80), ylim = c(0.45, 1)))
#'
#'   # Add the diseases names to the plot and remove legend
#'   p_(pl.kid +
#'     ggplot2::annotate(
#'       "text",
#'       label = c("PKD", "Other", "GN", "AN"),
#'       x = c(90, 125, 5, 60),
#'       y = c(0.8, 0.65, 0.55, 0.30),
#'       size = 5,
#'       colour = scales::pal_hue(
#'         h         = c(0, 360) + 15,
#'         c         = 100,
#'         l         = 65,
#'         h.start   = 0,
#'         direction = 1
#'       )(4)
#'     ) +
#'     ggplot2::guides(color = "none", linetype = "none"))
#' }
ggsurv <- function(
  s,
  CI = "def",
  plot.cens = TRUE,
  surv.col = "gg.def",
  cens.col = "gg.def",
  lty.est = 1,
  lty.ci = 2,
  size.est = 0.5,
  size.ci = size.est,
  cens.size = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab = "Time",
  ylab = "Survival",
  main = "",
  order.legend = TRUE
) {
  rlang::check_installed(c("survival", "scales"))

  strata <- ifelse(is.null(s$strata), 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)

  if (strata == 1) {
    fn <- ggsurv_s
  } else {
    fn <- ggsurv_m
  }

  pl <- fn(
    s,
    CI,
    plot.cens,
    surv.col,
    cens.col,
    lty.est,
    lty.ci,
    size.est,
    size.ci,
    cens.size,
    cens.shape,
    back.white,
    xlab,
    ylab,
    main,
    strata,
    order.legend
  )
  pl
}

# survival function for single survival
ggsurv_s <- function(
  s,
  CI = "def",
  plot.cens = TRUE,
  surv.col = "gg.def",
  cens.col = "gg.def",
  lty.est = 1,
  lty.ci = 2,
  size.est = 0.5,
  size.ci = size.est,
  cens.size = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab = "Time",
  ylab = "Survival",
  main = "",
  strata = 1,
  order.legend = TRUE
) {
  dat <- data.frame(
    time = c(0, s$time),
    surv = c(1, s$surv),
    up = c(1, s$upper),
    low = c(1, s$lower),
    cens = c(0, s$n.censor)
  )
  dat.cens <- dat[!is.na(dat$cens) & dat$cens != 0, ]

  col <- ifelse(surv.col == "gg.def", "black", surv.col)

  pl <- ggplot(dat, aes(x = .data$time, y = .data$surv)) +
    geom_step(col = col, lty = lty.est, linewidth = size.est) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main)

  if (identical(CI, TRUE) | identical(CI, "def")) {
    pl <- pl +
      geom_step(
        aes(y = .data$up),
        color = col,
        lty = lty.ci,
        linewidth = size.ci
      ) +
      geom_step(
        aes(y = .data$low),
        color = col,
        lty = lty.ci,
        linewidth = size.ci
      )
  }

  if (identical(plot.cens, TRUE)) {
    if (nrow(dat.cens) == 0) {
      stop("There are no censored observations")
    }
    col <- ifelse(cens.col == "gg.def", "red", cens.col)

    pl <- pl +
      geom_point(
        data = dat.cens,
        mapping = aes(y = .data$surv),
        shape = cens.shape,
        col = col,
        size = cens.size
      )
  }

  if (back.white == TRUE) {
    pl <- pl + theme_bw()
  }

  pl
}

# survival function for multiple survivals
ggsurv_m <- function(
  s,
  CI = "def",
  plot.cens = TRUE,
  surv.col = "gg.def",
  cens.col = "gg.def",
  lty.est = 1,
  lty.ci = 2,
  size.est = 0.5,
  size.ci = size.est,
  cens.size = 2,
  cens.shape = 3,
  back.white = FALSE,
  xlab = "Time",
  ylab = "Survival",
  main = "",
  strata = length(s$strata),
  order.legend = TRUE
) {
  n <- s$strata

  has_many <- all(grepl(",", names(s$strata)))
  if (has_many) {
    gr.name <- "combination"
    ugroups <- names(s$strata)
  } else {
    # singular
    strataEqualNames <- strsplit(names(s$strata), "=")
    gr.name <- strataEqualNames[[1]][[1]]
    ugroups <- vapply(strataEqualNames, `[[`, character(1), 2)
  }

  getlast <- function(x) {
    res <- NULL
    maxTime <- max(x$time)
    for (mo in names(x$strata)) {
      sur <- x[mo]$surv
      n <- length(sur)
      # grab the last survival value
      surValue <- sur[n]
      if (isTRUE(all.equal(surValue, 0))) {
        # if they die, order by percent complete of max observation.
        # tie value of 0 if the last person dies at the last time
        surTime <- x[mo]$time[n]
        surValue <- (surTime / maxTime) - 1
      }
      res <- append(res, surValue)
    }
    return(res)
  }

  if (isTRUE(order.legend)) {
    group_order <- order(getlast(s), decreasing = TRUE)
    lastv <- ugroups[group_order]
    if (length(surv.col) == length(n)) {
      surv.col <- surv.col[group_order]
    }
    if (length(cens.col) == length(n)) {
      cens.col <- cens.col[group_order]
    }
  } else {
    lastv <- ugroups
  }
  groups <- factor(ugroups, levels = lastv)
  gr.df <- vector("list", strata)
  n.ind <- cumsum(c(0, n))

  for (i in 1:strata) {
    indI <- (n.ind[i] + 1):n.ind[i + 1]
    gr.df[[i]] <- data.frame(
      time = c(0, s$time[indI]),
      surv = c(1, s$surv[indI]),
      up = c(1, s$upper[indI]),
      low = c(1, s$lower[indI]),
      cens = c(0, s$n.censor[indI]),
      group = rep(groups[i], n[i] + 1)
    )
  }

  dat <- do.call(rbind, gr.df)

  pl <- ggplot(dat, aes(x = .data$time, y = .data$surv, group = .data$group)) +
    geom_step(aes(col = .data$group, lty = .data$group), linewidth = size.est) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main)

  pl <- if (surv.col[1] != "gg.def") {
    scaleValues <- if (length(surv.col) == 1) {
      rep(surv.col, strata)
    } else {
      surv.col
    }
    pl + scale_colour_manual(values = scaleValues)
  } else {
    pl + scale_colour_discrete()
  }

  lineScaleValues <- if (length(lty.est) == 1) {
    rep(lty.est, strata)
  } else {
    lty.est
  }
  pl <- pl + scale_linetype_manual(values = lineScaleValues)

  if (identical(CI, TRUE)) {
    stepLty <- if ((length(surv.col) > 1 | surv.col == "gg.def")[1]) {
      lty.ci
    } else {
      surv.col
    }
    pl <- pl +
      geom_step(
        aes(y = .data$up, lty = .data$group, col = .data$group),
        lty = stepLty,
        linewidth = size.ci
      ) +
      geom_step(
        aes(y = .data$low, lty = .data$group, col = .data$group),
        lty = stepLty,
        linewidth = size.ci
      )
  }

  if (identical(plot.cens, TRUE)) {
    dat.cens <- dat[!is.na(dat$cens) & dat$cens != 0, ]
    dat.cens <- dat.cens[!is.na(dat.cens$group) & dat.cens$group != "PKD", ]

    if (nrow(dat.cens) == 0) {
      stop("There are no censored observations")
    }
    if (length(cens.col) == 1) {
      if (identical(cens.col, "gg.def")) {
        # match the colors of the lines
        pl <- pl +
          geom_point(
            data = dat.cens,
            mapping = aes(y = .data$surv, col = .data$group),
            shape = cens.shape,
            size = cens.size,
            show.legend = FALSE
          )
      } else {
        # supply the raw color value
        pl <- pl +
          geom_point(
            data = dat.cens,
            mapping = aes(y = .data$surv),
            shape = cens.shape,
            color = cens.col,
            size = cens.size
          )
      }
    } else if (length(cens.col) > 0) {
      # if (!(identical(cens.col, surv.col) || is.null(cens.col))) {
      #   warning ("Color scales for survival curves and censored points don't match.\nOnly one color scale can be used. Defaulting to surv.col")
      # }

      if (!identical(cens.col, "gg.def")) {
        if (length(cens.col) != strata) {
          warning(
            "Color scales for censored points don't match the number of groups. Defaulting to ggplot2 default color scale"
          )
          cens.col <- "gg.def"
        }
      }

      if (identical(cens.col, "gg.def")) {
        # match the group color value
        pl <- pl +
          geom_point(
            data = dat.cens,
            mapping = aes(y = .data$surv, col = .data$group),
            shape = cens.shape,
            show.legend = FALSE,
            size = cens.size
          )
      } else {
        # custom colors and maybe custom shape
        uniqueGroupVals <- levels(dat.cens$group)
        if (length(cens.shape) == 1) {
          cens.shape <- rep(cens.shape, strata)
        }

        if (length(cens.shape) != strata) {
          warning(
            "The length of the censored shapes does not match the number of groups (or 1). Defaulting shape = 3 (+)"
          )
          cens.shape <- rep(3, strata)
        }
        for (i in seq_along(uniqueGroupVals)) {
          groupVal <- uniqueGroupVals[i]
          dtGroup <- dat.cens[
            !is.na(dat.cens$group) & dat.cens$group == groupVal,
          ]
          if (nrow(dtGroup) == 0) {
            next
          }

          pl <- pl +
            geom_point(
              data = dtGroup,
              mapping = aes(y = .data$surv),
              color = I(cens.col[i]),
              shape = cens.shape[i],
              show.legend = FALSE,
              size = cens.size
            )
        }
      }
    }
  }

  if (identical(back.white, TRUE)) {
    pl <- pl + theme_bw()
  }

  pl <- pl +
    labs(
      color = gr.name,
      linetype = gr.name
    )

  pl
}
