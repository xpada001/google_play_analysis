#
# Code taken from John Fox's "car" package.
# Modified by Wayne Oldford to incorporate
#     interaction
#     quadratic (additive)
#     quadint,
#     loess
#     and to open it up to add whatever
#
library(mgcv)

showLabels3d <- function (x, y, z, labels, id.method = "identify", id.n = length(x),
          col = c("blue"), res = y - mean(y), range.x = range(x), range.z = range(z),
          offset = ((100/length(x))^(1/3)) * 0.02)
{
  if (!requireNamespace("rgl"))
    stop("rgl package is missing")
  if (id.method == "none")
    return(NULL)
  if (id.n > 0L) {
    if (missing(labels))
      labels <- as.character(seq(along = x))
    getPoints <- function(w) {
      names(w) <- labels
      iid <- seq(length = id.n)
      ws <- w[order(-w)[iid]]
      match(names(ws), labels)
    }
    ind <- switch(id.method, xz = getPoints(rowSums(qr.Q(qr(cbind(1,
                                                                  x, z)))^2)), y = getPoints(abs(res)), xyz = union(getPoints(abs(x -
                                                                                                                                    mean(x))), union(abs(z - mean(z)), getPoints(abs(res)))),
                  mahal = getPoints(rowSums(qr.Q(qr(cbind(1, x, y,
                                                          z)))^2)))
    rgl::rgl.texts(x[ind], y[ind] + offset, z[ind], labels[ind],
                   color = col)
    return(labels[ind])
  }
}

nice <- function (x, direction = c("round", "down", "up"), lead.digits = 1)
{
  direction <- match.arg(direction)
  if (length(x) > 1)
    return(sapply(x, nice, direction = direction, lead.digits = lead.digits))
  if (x == 0)
    return(0)
  power.10 <- floor(log(abs(x), 10))
  if (lead.digits > 1)
    power.10 <- power.10 - lead.digits + 1
  lead.digit <- switch(direction, round = round(abs(x)/10^power.10),
                       down = floor(abs(x)/10^power.10), up = ceiling(abs(x)/10^power.10))
  sign(x) * lead.digit * 10^power.10
}

scatter3d <- function (x, ...)
{
  if (!requireNamespace("rgl"))
    stop("rgl package missing")
  if (!requireNamespace("mgcv"))
    stop("mgcv package is missing")
  UseMethod("scatter3d")
}

scatter3d.formula <- function (formula, data, subset, radius, xlab, ylab, zlab, labels,
          ...)
{
  if (!requireNamespace("rgl"))
    stop("rgl package missing")
  if (!requireNamespace("mgcv"))
    stop("mgcv package is missing")
  na.save <- options(na.action = na.omit)
  on.exit(options(na.save))
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, sys.frame(sys.parent()))))
    m$data <- as.data.frame(data)
  m$na.action <- na.pass
  m$labels <- m$xlab <- m$ylab <- m$zlab <- m$... <- NULL
  m[[1]] <- as.name("model.frame")
  formula <- as.character(c(formula))
  formula <- as.formula(sub("\\|", "+", formula))
  m$formula <- formula
  X <- eval(m, parent.frame())
  if ("(radius)" %in% names(X)) {
    radius <- X[, "(radius)"]
    X <- X[, names(X) != "(radius)"]
  }
  else radius <- 1
  names <- names(X)
  if (missing(xlab))
    xlab <- names[2]
  if (missing(ylab))
    ylab <- names[1]
  if (missing(zlab))
    zlab <- names[3]
  if (missing(labels))
    labels <- rownames(X)
  if (ncol(X) == 3)
    scatter3d(X[, 2], X[, 1], X[, 3], xlab = xlab, ylab = ylab,
              zlab = zlab, labels = labels, radius = radius, ...)
  else if (ncol(X) == 4)
    scatter3d(X[, 2], X[, 1], X[, 3], groups = X[, 4], xlab = xlab,
              ylab = ylab, zlab = zlab, labels = labels, radius = radius,
              ...)
  else stop("incorrect scatter3d formula")
}


scatter3d.default <- function (x, y, z,
                                     xlab = deparse(substitute(x)),
                                     ylab = deparse(substitute(y)),
                                     zlab = deparse(substitute(z)),
                                     axis.scales = TRUE,
                                     revolutions = 0,
                                     bg.col = c("white", "black"),
                                     axis.col = if (bg.col == "white") c("darkmagenta", "black", "darkcyan") else c("darkmagenta", "white", "darkcyan"),
                                     surface.col = c("blue", "green", "orange", "magenta", "cyan", "red", "yellow", "gray"),
                                     surface.alpha = 0.5,
                                     neg.res.col = "red",
                                     pos.res.col = "green",
                                     square.col = if (bg.col == "white") "black" else "gray",
                                     point.col = "yellow",
                                     text.col = axis.col,
                                     grid.col = if (bg.col ==  "white") "black" else "gray",
                                     fogtype = c("exp2", "linear", "exp", "none"),
                                     residuals = (length(fit) == 1),
                                     surface = TRUE,
                                     fill = TRUE,
                                     grid = TRUE,
                                     grid.lines = 26,
                                     df.smooth = NULL,
                                     df.additive = NULL,
                                     df.loess = NULL,
                                     span=NULL,
                                     sphere.size = 1,
                                     radius = 1,
                                     threshold = 0.01,
                                     speed = 1,
                                     fov = 60,
                                     fit = "linear",
                                     groups = NULL,
                                     parallel = TRUE,
                                     ellipsoid = FALSE,
                                     level = 0.5,
                                     ellipsoid.alpha = 0.1,
                                     id.method = c("mahal", "xz", "y", "xyz", "identify", "none"),
                                     id.n = if (id.method ==  "identify") Inf else 0,
                                     labels = as.character(seq(along = x)),
                                     offset = ((100/length(x))^(1/3)) * 0.02,
                                     model.summary = FALSE,
          ...)
{
  if (!requireNamespace("rgl"))
    stop("rgl package missing")
  if (!requireNamespace("mgcv"))
    stop("mgcv package missing")
  id.method <- match.arg(id.method)
  if (residuals == "squares") {
    residuals <- TRUE
    squares <- TRUE
  }
  else squares <- FALSE
  summaries <- list()
  if ((!is.null(groups)) && (nlevels(groups) > length(surface.col)))
    stop(sprintf("Number of groups (%d) exceeds number of colors (%d)",
                 nlevels(groups), length(surface.col)))
  if ((!is.null(groups)) && (!is.factor(groups)))
    stop("groups variable must be a factor")
  counts <- table(groups)
  if (any(counts == 0)) {
    levels <- levels(groups)
    warning("the following groups are empty: ", paste(levels[counts ==
                                                               0], collapse = ", "))
    groups <- factor(groups, levels = levels[counts != 0])
  }
  bg.col <- match.arg(bg.col)
  fogtype <- match.arg(fogtype)
  if ((length(fit) > 1) && residuals && surface)
    stop("cannot plot both multiple surfaces and residuals")
  xlab
  ylab
  zlab
  rgl::rgl.clear()
  rgl::rgl.viewpoint(fov = fov)
  rgl::rgl.bg(color = bg.col, fogtype = fogtype)
  if (id.method == "identify") {
    xg <- x
    yg <- y
    zg <- z
    ggroups <- groups
    glabels <- labels
  }
  valid <- if (is.null(groups))
    complete.cases(x, y, z)
  else complete.cases(x, y, z, groups)
  x <- x[valid]
  y <- y[valid]
  z <- z[valid]
  labels <- labels[valid]
  minx <- min(x)
  maxx <- max(x)
  miny <- min(y)
  maxy <- max(y)
  minz <- min(z)
  maxz <- max(z)
  if (axis.scales) {
    lab.min.x <- nice(minx)
    lab.max.x <- nice(maxx)
    lab.min.y <- nice(miny)
    lab.max.y <- nice(maxy)
    lab.min.z <- nice(minz)
    lab.max.z <- nice(maxz)
    minx <- min(lab.min.x, minx)
    maxx <- max(lab.max.x, maxx)
    miny <- min(lab.min.y, miny)
    maxy <- max(lab.max.y, maxy)
    minz <- min(lab.min.z, minz)
    maxz <- max(lab.max.z, maxz)
    min.x <- (lab.min.x - minx)/(maxx - minx)
    max.x <- (lab.max.x - minx)/(maxx - minx)
    min.y <- (lab.min.y - miny)/(maxy - miny)
    max.y <- (lab.max.y - miny)/(maxy - miny)
    min.z <- (lab.min.z - minz)/(maxz - minz)
    max.z <- (lab.max.z - minz)/(maxz - minz)
  }
  if (!is.null(groups))
    groups <- groups[valid]
  x <- (x - minx)/(maxx - minx)
  y <- (y - miny)/(maxy - miny)
  z <- (z - minz)/(maxz - minz)
  size <- sphere.size * ((100/length(x))^(1/3)) * 0.015
  radius <- radius/median(radius)
  if (is.null(groups)) {
    if (size > threshold)
      rgl::rgl.spheres(x, y, z, color = point.col, radius = size *
                         radius)
    else rgl::rgl.points(x, y, z, color = point.col)
  }
  else {
    if (size > threshold)
      rgl::rgl.spheres(x, y, z, color = surface.col[as.numeric(groups)],
                       radius = size * radius)
    else rgl::rgl.points(x, y, z, color = surface.col[as.numeric(groups)])
  }
  if (!axis.scales)
    axis.col[1] <- axis.col[3] <- axis.col[2]
  rgl::rgl.lines(c(0, 1), c(0, 0), c(0, 0), color = axis.col[1])
  rgl::rgl.lines(c(0, 0), c(0, 1), c(0, 0), color = axis.col[2])
  rgl::rgl.lines(c(0, 0), c(0, 0), c(0, 1), color = axis.col[3])
  rgl::rgl.texts(1, 0, 0, xlab, adj = 1, color = axis.col[1])
  rgl::rgl.texts(0, 1.05, 0, ylab, adj = 1, color = axis.col[2])
  rgl::rgl.texts(0, 0, 1, zlab, adj = 1, color = axis.col[3])
  if (axis.scales) {
    rgl::rgl.texts(min.x, -0.05, 0, lab.min.x, col = axis.col[1])
    rgl::rgl.texts(max.x, -0.05, 0, lab.max.x, col = axis.col[1])
    rgl::rgl.texts(0, -0.1, min.z, lab.min.z, col = axis.col[3])
    rgl::rgl.texts(0, -0.1, max.z, lab.max.z, col = axis.col[3])
    rgl::rgl.texts(-0.05, min.y, -0.05, lab.min.y, col = axis.col[2])
    rgl::rgl.texts(-0.05, max.y, -0.05, lab.max.y, col = axis.col[2])
  }
  if (ellipsoid) {
    dfn <- 3
    if (is.null(groups)) {
      dfd <- length(x) - 1
      ell.radius <- sqrt(dfn * qf(level, dfn, dfd))
      ellips <- ellipsoid(center = c(mean(x), mean(y),
                                     mean(z)), shape = cov(cbind(x, y, z)), radius = ell.radius)
      if (fill)
        rgl::shade3d(ellips, col = surface.col[1], alpha = ellipsoid.alpha,
                     lit = FALSE)
      if (grid)
        rgl::wire3d(ellips, col = surface.col[1], lit = FALSE)
    }
    else {
      levs <- levels(groups)
      for (j in 1:length(levs)) {
        group <- levs[j]
        select.obs <- groups == group
        xx <- x[select.obs]
        yy <- y[select.obs]
        zz <- z[select.obs]
        dfd <- length(xx) - 1
        ell.radius <- sqrt(dfn * qf(level, dfn, dfd))
        ellips <- ellipsoid(center = c(mean(xx), mean(yy),
                                       mean(zz)), shape = cov(cbind(xx, yy, zz)),
                            radius = ell.radius)
        if (fill)
          rgl::shade3d(ellips, col = surface.col[j],
                       alpha = ellipsoid.alpha, lit = FALSE)
        if (grid)
          rgl::wire3d(ellips, col = surface.col[j], lit = FALSE)
        coords <- ellips$vb[, which.max(ellips$vb[1,
                                                  ])]
        if (!surface)
          rgl::rgl.texts(coords[1] + 0.05, coords[2],
                         coords[3], group, col = surface.col[j])
      }
    }
  }
  if (surface) {
    vals <- seq(0, 1, length.out = grid.lines)
    dat <- expand.grid(x = vals, z = vals)
    for (i in 1:length(fit)) {
      f <- match.arg(fit[i], c("linear", "interaction", "quadratic", "quadint", "smooth",
                               "additive", "loess"))

      if (is.null(groups)) {
        mod <- switch(f,
                      linear = lm(y ~ x + z),
                      interaction = lm(y ~ x + z + x:z),
                      quadratic = lm(y ~ x + z + I(x^2) + I(z^2)),
                      quadint = lm(y ~ x + z + x:z + I(x^2) + I(z^2)),
                      smooth = if (is.null(df.smooth)) {
                        mgcv::gam(y ~  s(x, z))
                        } else {
                          mgcv::gam(y ~ s(x, z, fx = TRUE,
                                          k = df.smooth))
                          },
                      additive = if (is.null(df.additive)) {
                        mgcv::gam(y ~  s(x, bs="cr") + s(z, bs="cr"))
                        } else {
                          mgcv::gam(y ~ s(x, fx = TRUE, bs="cr",
                                          k = df.additive[1] + 1) +
                                        s(z, fx = TRUE, bs="cr",
                                          k = (rev(df.additive +  1)[1] + 1))
                                    )
                          },
                      loess = if (is.null(span)) {
                                 if (is.null(df.loess)) {
                                   stats::loess(y ~ x + z)
                                   } else {
                                     stats::loess(y ~ x + z, enp.target = df.loess)
                                   }
                              }else {
                                     stats::loess(y ~ x + z, span=span)
                                    }

        )
        if (model.summary)
          summaries[[f]] <- summary(mod)
        yhat <- matrix(predict(mod, newdata = dat), grid.lines,
                       grid.lines)

        if (fill)
          rgl::rgl.surface(vals, vals, yhat, color = surface.col[i],
                           alpha = surface.alpha, lit = FALSE)
        if (grid)
          rgl::rgl.surface(vals, vals, yhat, color = if (fill)
            grid.col
            else surface.col[i], alpha = surface.alpha,
            lit = FALSE, front = "lines", back = "lines")
        if (residuals) {
          n <- length(y)
          fitted <- fitted(mod)
          colors <- ifelse(residuals(mod) > 0, pos.res.col,
                           neg.res.col)
          rgl::rgl.lines(as.vector(rbind(x, x)), as.vector(rbind(y,
                                                                 fitted)), as.vector(rbind(z, z)), color = as.vector(rbind(colors,
                                                                                                                           colors)))
          if (squares) {
            res <- y - fitted
            xx <- as.vector(rbind(x, x, x + res, x +
                                    res))
            yy <- as.vector(rbind(y, fitted, fitted,
                                  y))
            zz <- as.vector(rbind(z, z, z, z))
            rgl::rgl.quads(xx, yy, zz, color = square.col,
                           alpha = surface.alpha, lit = FALSE)
            rgl::rgl.lines(xx, yy, zz, color = square.col)
          }
        }
      }
      else {
        if (parallel) {
          mod <- switch(f, linear = lm(y ~ x + z + groups),
                        interaction = lm(y ~ x + z + x:z +
                                         groups),
                        quadratic = lm(y ~ x + z + I(x^2) + I(z^2) +
                                         groups),
                        quadint = lm(y ~ x + z + x:z + I(x^2) + I(z^2) +
                                       groups),
                        smooth = if (is.null(df.smooth)) mgcv::gam(y ~
                                                                                               s(x, z) + groups) else mgcv::gam(y ~ s(x,
                                                                                                                                      z, fx = TRUE, k = df.smooth) + groups),
                        additive = if (is.null(df.additive)) mgcv::gam(y ~
                                                                         s(x, bs="cr") + s(z, bs="cr") + groups) else mgcv::gam(y ~
                                                                                                                s(x, fx = TRUE, bs="cr", k = df.additive[1] + 1) +
                                                                                                                s(z, fx = TRUE, bs="cr", k = (rev(df.additive + 1)[1] + 1)) +
                                                                                                                groups),
                        loess = if (is.null(span)) {
                          if (is.null(df.loess)) {
                            stats::loess(y ~ x + z + groups)
                          } else {
                            stats::loess(y ~ x + z + groups, enp.target = df.loess)
                          }
                        }else {
                          stats::loess(y ~ x + z + groups, span=span)
                        }
                        )
          if (model.summary)
            summaries[[f]] <- summary(mod)
          levs <- levels(groups)
          for (j in 1:length(levs)) {
            group <- levs[j]
            select.obs <- groups == group
            yhat <- matrix(predict(mod, newdata = cbind(dat,
                                                        groups = group)), grid.lines, grid.lines)
            if (fill)
              rgl::rgl.surface(vals, vals, yhat, color = surface.col[j],
                               alpha = surface.alpha, lit = FALSE)
            if (grid)
              rgl::rgl.surface(vals, vals, yhat, color = if (fill)
                grid.col
                else surface.col[j], alpha = surface.alpha,
                lit = FALSE, front = "lines", back = "lines")
            rgl::rgl.texts(1, predict(mod, newdata = data.frame(x = 1,
                                                                z = 1, groups = group)), 1, paste(group,
                                                                                                  " "), adj = 1, color = surface.col[j])
            if (residuals) {
              yy <- y[select.obs]
              xx <- x[select.obs]
              zz <- z[select.obs]
              fitted <- fitted(mod)[select.obs]
              res <- yy - fitted
              rgl::rgl.lines(as.vector(rbind(xx, xx)),
                             as.vector(rbind(yy, fitted)), as.vector(rbind(zz,
                                                                           zz)), col = surface.col[j])
              if (squares) {
                xxx <- as.vector(rbind(xx, xx, xx + res,
                                       xx + res))
                yyy <- as.vector(rbind(yy, fitted, fitted,
                                       yy))
                zzz <- as.vector(rbind(zz, zz, zz, zz))
                rgl::rgl.quads(xxx, yyy, zzz, color = surface.col[j],
                               alpha = surface.alpha, lit = FALSE)
                rgl::rgl.lines(xxx, yyy, zzz, color = surface.col[j])
              }
            }
          }
        }
        else {
          levs <- levels(groups)
          for (j in 1:length(levs)) {
            group <- levs[j]
            select.obs <- groups == group
            mod <- switch(f, linear = lm(y ~ x + z, subset = select.obs),
                          interaction = lm(y ~ x + z + x:z, subset = select.obs),
                          quadratic = lm(y ~ x + z + I(x^2) +
                                           I(z^2), subset = select.obs),
                          quadratic = lm(y ~ x + z + x:z + I(x^2) +
                                           I(z^2), subset = select.obs),
                          smooth = if (is.null(df.smooth)) {mgcv::gam(y ~ s(x, z), subset = select.obs)} else {mgcv::gam(y ~ s(x, z, fx = TRUE, k = df.smooth), subset = select.obs)},
                          additive = if (is.null(df.additive)) mgcv::gam(y ~ s(x) + s(z), subset = select.obs) else mgcv::gam(y ~  s(x, fx = TRUE, k = df.additive[1] + 1) + s(z, fx = TRUE, k = (rev(df.additive + 1)[1] + 1)), subset = select.obs),
                          loess = if (is.null(span)) {
                            if (is.null(df.loess)) {
                              stats::loess(y ~ x + z, subset = select.obs)
                            } else {
                              stats::loess(y ~ x + z, enp.target = df.loess, subset = select.obs)
                            }
                          }else {
                            stats::loess(y ~ x + z, span=span, subset = select.obs)
                          }
                          )
            if (model.summary)
              summaries[[paste(f, ".", group, sep = "")]] <- summary(mod)
            yhat <- matrix(predict(mod, newdata = dat),
                           grid.lines, grid.lines)
            if (fill)
              rgl::rgl.surface(vals, vals, yhat, color = surface.col[j],
                               alpha = surface.alpha, lit = FALSE)
            if (grid)
              rgl::rgl.surface(vals, vals, yhat, color = if (fill)
                grid.col
                else surface.col[j], alpha = surface.alpha,
                lit = FALSE, front = "lines", back = "lines")
            rgl::rgl.texts(1, predict(mod, newdata = data.frame(x = 1,
                                                                z = 1, groups = group)), 1, paste(group,
                                                                                                  " "), adj = 1, color = surface.col[j])
            if (residuals) {
              yy <- y[select.obs]
              xx <- x[select.obs]
              zz <- z[select.obs]
              fitted <- fitted(mod)
              res <- yy - fitted
              rgl::rgl.lines(as.vector(rbind(xx, xx)),
                             as.vector(rbind(yy, fitted)), as.vector(rbind(zz,
                                                                           zz)), col = surface.col[j])
              if (squares) {
                xxx <- as.vector(rbind(xx, xx, xx + res,
                                       xx + res))
                yyy <- as.vector(rbind(yy, fitted, fitted,
                                       yy))
                zzz <- as.vector(rbind(zz, zz, zz, zz))
                rgl::rgl.quads(xxx, yyy, zzz, color = surface.col[j],
                               alpha = surface.alpha, lit = FALSE)
                rgl::rgl.lines(xxx, yyy, zzz, color = surface.col[j])
              }
            }
          }
        }
      }
    }
  }
  else levs <- levels(groups)
  if (id.method == "identify") {
    Identify3d(xg, yg, zg, axis.scales = axis.scales, groups = ggroups,
               labels = glabels, col = surface.col, offset = offset)
  }
  else if (id.method != "none") {
    if (is.null(groups))
      showLabels3d(x, y, z, labels, id.method = id.method,
                   id.n = id.n, col = surface.col[1])
    else {
      for (j in 1:length(levs)) {
        group <- levs[j]
        select.obs <- groups == group
        showLabels3d(x[select.obs], y[select.obs], z[select.obs],
                     labels[select.obs], id.method = id.method,
                     id.n = id.n, col = surface.col[j])
      }
    }
  }
  if (revolutions > 0) {
    for (i in 1:revolutions) {
      for (angle in seq(1, 360, length.out = 360/speed)) rgl::rgl.viewpoint(-angle,
                                                                            fov = fov)
    }
  }
  if (model.summary)
    return(summaries)
  else return(invisible(NULL))
}