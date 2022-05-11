"%||%" <- function(x, default) {
    if (is.null(x)) default else x
}

is.empty <- function(x) { length(x) == 0 }

getLabel <- function(x, default) {
    ifelse(is.null(y <- attr(x, "label")), default, y)
}

lumos <- function(x, ..., .graphical=FALSE) {
    if (.graphical) {
        UseMethod("lumos_plot")
    } else {
        UseMethod("lumos")
    }
}

lumos.formula <- function(data, ...) {
}

lumos.default <- function(data, ...) {

    name.data <- deparse1(substitute(data))
    name.dots <- as.character(unlist(match.call(expand.dots=FALSE)$...))

    getName <- function(x) {
        p <-str2lang(x) 
        if (length(p) > 1 && identical(p[[1]], as.name("$"))) {
            as.character(p[[3]])
        } else {
            x
        }
    }

    x     <- c(list(data), list(...))
    name  <- Vectorize(getName)(c(name.data, name.dots))
    label <- Vectorize(getLabel)(x, name)

    lumos_tabulate(x=x, name=name)
}

lumos_atomic <- function(data, ..., .drop=TRUE, .max=Inf, .pct=TRUE, .order.by.freq=(.max < Inf), .kable=TRUE) {

    if (!is.empty(list(...))) {
        .call <- match.call()
        .call[[1]] <- `lumos.default`
        return(eval(.call))
    }

    name.data <- deparse1(substitute(data))
    getType    <- function(x) { paste("Type:", sprintf("%s/%s", class(x), typeof(x))) }
    getMissing <- function(x) {
        paste("Missing:", ifelse(any(is.na(x)),
                sprintf("%s/%s (%s%%)",
                    sum(is.na(x)),
                    length(x),
                    formatC(100*mean(is.na(x)), format="f", digits=1)),
                "none"))
    }
    x <- data
    caption <- paste0(c(getLabel(x, name.data), getType(x), getMissing(x)), collapse="\n")

    if ((is.numeric(x) || is.integer(x)) && length(unique(x)) > .max) {
        tb <- c(
            Mean   = format(mean(x, na.rm=T), digit=3),
            SD     = format(sd(x, na.rm=T), digit=3),
            Median = format(median(x, na.rm=T)),
            Min    = format(min(x, na.rm=T)),
            Max    = format(max(x, na.rm=T)))
        tb <- data.frame(Statistic=names(tb), Value=tb)
    } else {
        x <- as.factor(x)
        if (.drop) {
            x <- droplevels(x)
        }
        tb <- table(x, useNA="ifany")
        pct <- as.numeric(prop.table(table(x, useNA="ifany")))
        tb <- data.frame(value=names(tb), N=as.numeric(tb), `%`=pct, check.names=FALSE)
        if (.order.by.freq) {
            tb <- tb[order(tb$N, decreasing=TRUE),]
        }
        tb <- head(tb, .max)
        n.o <- length(x) - sum(tb$N)
        if (n.o > 0) {
            tb <- rbind(tb, data.frame(value="Other", N=n.o, `%`=n.o/length(x), check.names=FALSE))
        }
        if (.pct) {
            tb$`%` <- sprintf("%.01f%%", 100*tb$`%`)
        } else {
            tb$`%` <- NULL
        }
        names(tb)[[1]] <- name.data
    }
    lumos_output(tb, ..., caption=caption, .kable=.kable)
}

lumos.numeric <- function(data, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_atomic`
    eval(.call)
}

lumos.character <- function(data, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_atomic`
    eval(.call)
}

lumos.factor <- function(data, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_atomic`
    eval(.call)
}

lumos.logical <- function(data, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_atomic`
    eval(.call)
}

lumos.data.frame <- function(data, ..., .max=Inf, .kable=TRUE) {

    x <- eval(substitute(list(...)), data, enclos=parent.frame())
    name.data <- deparse1(substitute(data))
    name.dots <- as.character(unlist(match.call(expand.dots=FALSE)$...))

    if (is.empty(x)) {
        .call <- match.call()
        .call[[1]] <- `lumos_dfsummary`
        return(eval(.call))
    } else if (length(x) == 1) {
        .call <- match.call()
        .call[[1]] <- as.name("lumos_atomic")
        .call[[2]] <- .call[[3]]
        .call[[3]] <- NULL
        return(eval(.call, data, enclos=parent.frame()))
    }

    name  <- name.dots
    label <- getLabel(x, name)

    lumos_tabulate(x=x, name=name)
}

lumos_tabulate <- function(x, name, ..., .drop=TRUE, .blanks=TRUE, .recycle=TRUE, .kable=TRUE) {

    caption <- NULL

    x <- lapply(x, as.factor)
    if (.drop) {
        x <- lapply(x, droplevels)
    }
    if (.recycle) {
        x <- as.data.frame(x)
    }
    tb <- do.call(table, c(x, list(useNA="ifany")))
    tb <- as.data.frame(tb)
    names(tb) <- c(name, "N")
    tb <- tb[do.call(order, tb),]
    tb <- tb[tb[[length(tb)]] != 0,]
    if (.blanks) {
        for (i in length(x):1) {
            combine <- function(...) paste(..., sep=";")
            id <- do.call(combine, Map(paste, unname(as.list(tb[, 1:i, drop=FALSE]))))
            id <- factor(id, levels=unique(id))
            y <- as.character(tb[[i]])
            y[duplicated(id)] <- ""
            tb[[i]] <- y
        }
    }

    lumos_output(tb, ..., .kable=.kable)
}

lumos_dfsummary <- function(data, ..., .kable=TRUE) {

    caption  <- paste0(collapse="\n", c(
            deparse1(substitute(data)),
            sprintf("%d rows, %d columns", dim(data)[1], dim(data)[2])))

    nmissing   <- function(x) { sum(is.na(x)) }
    nunique    <- function(x) { length(unique(x)) }
    firstclass <- function(x) { class(x)[1] }
    firstvalue <- function(x) { format(x[!is.na(x)][1]) }

    tb <- data.frame(variable=names(data),
        label   = Vectorize(getLabel)(data, NA),
        class   = sapply(data, firstclass),
        missing = sapply(data, nmissing),
        unique  = sapply(data, nunique),
        example = sapply(data, firstvalue))
    if (all(is.na(tb$label))) {
        tb$label <- NULL
    }
    rownames(tb) <- NULL

    lumos_output(tb, ..., .kable=.kable)
}

lumos_output <- function(tb, ..., caption=NULL, .kable=TRUE) {
    if (!is.null(.kable) && !isFALSE(.kable)) {
        if (isTRUE(.kable))
            knitr::kable(x=tb, row.names=FALSE, caption=caption)
        else {
            knitr::kable(x=tb, row.names=FALSE, caption=caption, format=.kable)
        }
    } else {
        tb
    }
}

lumos_plot_atomic <- function(x, y=NULL, ...) {
    .call <- match.call()
    if (!is.null(y)) {
        .call[[1]] <- `lumos_plot_bivar`
    } else {
        .call[[1]] <- `lumos_plot_univar`
    }
    eval(.call)
}

lumos_plot.numeric <- function(x, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_plot_atomic`
    eval(.call)
}

lumos_plot.factor <- function(x, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_plot_atomic`
    eval(.call)
}

lumos_plot.character <- function(x, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_plot_atomic`
    eval(.call)
}

lumos_plot.logical <- function(x, ...) {
    .call <- match.call()
    .call[[1]] <- `lumos_plot_atomic`
    eval(.call)
}

lumos_plot_univar <- function(x,
    ...,
    xlab = table1::label(x) %||% deparse1(substitute(x)),
    col1 = "#0000ff",
    col2 = "#e5e5ff",
    col3 = "#c74900") {

    xlab1 <- xlab

    if (is.numeric(x)) {
        x <- x[!is.na(x)]

        #hist(x, 15, prob=T, axes=F, ann=F, col=adjustcolor("black", 0.01), border=adjustcolor("black", 0.1))
        #axis(1)

        dens <- density(x)
        xx <- dens$x
        yy <- dens$y
        keep <- xx >= min(x) & xx <= max(x)
        xx <- xx[keep]
        yy <- yy[keep]
        plot(NA, type="n", xlim=range(xx), ylim=c(0, max(yy)), axes=F, ann=F)
        polygon(c(xx, rev(xx)), c(yy, rep(0, length(yy))), col=col2, border=col1)
        segments(median(x), 0, median(x), approx(xx, yy, median(x))$y, col=col1, lty="64")
        axis(1)
        #axis(1, at=c(min(x), median(x), max(x)), line=-5, lwd=0, col.axis=col3, cex.axis=2)

    } else {
        x <- as.factor(x)
        yy <- prop.table(table(x))
        xx <- 1:length(yy)
        plot(NA, xlim=range(xx) + c(-0.5, 0.5), ylim=c(0, 1.1*max(yy)), type="n", axes=F, ann=F)
        rect(xx - 0.45, 0, xx + 0.45, yy, col=col2, border=col1)
        axis(1, at=xx, labels=names(yy), lwd=0)
        text(xx, yy, paste0(table1::round_pad(100*yy, 1), "%"), pos=3, col=col3)
    }
    mtext(xlab1, side=3)
}


lumos_plot_bivar <- function(x, y, 
    ...,
    xlab = table1::label(x) %||% deparse1(substitute(x)),
    ylab = table1::label(y) %||% deparse1(substitute(y)),
    col1 = "#0000ff",
    col2 = "#e5e5ff",
    col3 = "#c74900") {

    xlab1 <- xlab
    ylab1 <- ylab

    if (is.numeric(x) && is.numeric(y)) {

        # Scatterplot
        #plot(x, y, frame.plot=F, col=col2, pch=16, ann=F)
        #lines(loess.smooth(x, y), col=col1)

        # 2D kernel density
        dens <- with(na.omit(data.frame(x=x, y=y)), MASS::kde2d(x, y))
        with(dens, plot(x, y, type="n", ann=F, frame.plot=F))

        lvls <- with(dens, pretty(range(z), 10))
        pal  <- colorRampPalette(c("white", col1))
        pal  <- colorRampPalette(c("white", pal(4)[2]))(length(lvls) - 1)
        with(dens, .filled.contour(x, y, z, levels=lvls, col=pal))
        #with(dens, contour(x, y, z, levels=lvls, col=col1, add=T, drawlabels=F))

        usr <- par("usr")
        r <- with(na.omit(data.frame(x=x, y=y)), cor(x, y))
        text(mean(usr[1:2]), mean(usr[3:4]), paste0("R = ", table1::round_pad(r, 3)), col=col3, cex=2, font=2)

    } else if ((!is.numeric(x) && is.numeric(y)) || (is.numeric(x) && !is.numeric(y))) {
        # Boxplots
        if ((!is.numeric(x) && is.numeric(y))) {
            boxplot(y ~ x, col=col2, border=col1, frame.plot=F, ann=F, horizontal=F)
        } else {
            #boxplot(x ~ y, col=col2, border=col1, frame.plot=F, ann=F, horizontal=T)

            # Ridge plot
            s <- split(x, y)
            plot(NA, type="n", xlim=range(x, na.rm=T), ylim=c(0, length(s)), axes=F, ann=F)
            for (i in 1:length(s)) {
                xi <- s[[i]]
                xi <- xi[!is.na(xi)]
                dens <- density(xi)
                xx <- dens$x
                yy <- dens$y
                keep <- xx >= min(xi) & xx <= max(xi)
                xx <- xx[keep]
                yy <- yy[keep]
                yy <- (i - 1) + 0.9*yy/max(yy)
                polygon(c(xx, rev(xx)), c(yy, rep(i-1, length(yy))), col=col2, border=col1)
                segments(median(xi), i-1, median(xi), approx(xx, yy, median(xi))$y, col=col1, lty="64")
            }
            axis(1)
            axis(2, at=seq_along(s) - 0.5, lwd=0, labels=names(s))
        }
    } else {
        # Mosaic plot
        mosaicplot(prop.table(table(x, y)), col=col2, border=col1, main=NULL, xlab="", ylab="")
    }
    mtext(xlab1, side=3, cex=1.5, font=2)
    #mtext(ylab1, side=2, cex=1.5, font=2, line=3)
    #mtext(ylab1, side=4, cex=1.5, font=2)
    usr <- par("usr")
    #text(usr[2] + 0.02*diff(usr[1:2]), mean(usr[3:4]), ylab1, cex=1.7, font=2, srt=-90, xpd=NA)
    text(usr[2], mean(usr[3:4]), ylab1, cex=1.7, font=2, srt=-90, xpd=NA)
}

library(CDISC)
adsl <- cdiscpilot01$adam$adsl

lumos(adsl, age)
lumos(adsl, age, .max=20)
lumos(adsl, sex)
lumos(adsl, sex, .max=20)
lumos(adsl, sex, race)
lumos(adsl, age, sex, .max=20)

lumos(CDISC::cdiscpilot01$sdtm$dm)
lumos(CDISC::cdiscpilot01$sdtm$dm, .kable=F)

lumos(adsl, .kable=F)
lumos(adsl$sex)
lumos(adsl$sex, adsl$race)
lumos(adsl$sex, adsl$race, adsl$ethnic)
lumos(adsl, sex)
lumos(adsl, sex, race)
lumos(adsl, sex, race, ethnic)

lumos(~adsl, sex)


lumos(1:100, .max=10)
lumos(adsl$age, .max=10)
lumos(adsl$age)
lumos(adsl, age)
lumos(adsl, age, .max=10)
lumos(adsl, sex, age)

lumos(adsl$sex, .graphical=T)
lumos(adsl$sex, adsl$age, .graphical=T)
lumos(adsl$age, .graphical=T)
lumos(adsl$age, adsl$sex, .graphical=T)





#' Shed Light on Your Data
#'
#' Like a magic wand for exploring a \code{data.frame}.  It's so powerful that
#' it can be abbreviated to the single letter \code{l}.
#'
#' @param data A \code{data.frame} (or, and object that can be converted to one
#' by call \code{link{as.data.frame}}).
#' @param ... Further argument, evaluated within \code{data} (so they can
#' directly refer to columns in data without quotes).
#' @param .drop If \code{TRUE}, unused factor levels are dropped.
#' @param .max For a single categorical variable, the maximum number of unique
#' categories to show (can be \code{Inf}).  For a single numeric variable, if
#' there are no more than this many unique values, the variable will be treated
#' as categorical.
#' @param .pct If \code{TRUE}, show percents along with counts for single
#' categorical variables.
#' @param .order.by.freq If \code{TRUE}, for a single categorical variable,
#' show the categories in decreasing order of frequency, from top to bottom
#' (i.e. show the most frequent categories on top).
#' @param .blanks If \code{TRUE}, insert blank spaces instead of repeating
#' consecutive values that are identical.
#' @param .recycle If \code{TRUE}, use vector recycling to make all arguments
#' have the same length.
#' @param .missing If \code{TRUE}, instead of the usual output, show the
#' frequency and precent (by default) of missing values per column of
#' \code{data}. If there are no missing values then no output is produced.
#' @param .gen If \code{TRUE}, instead of the usual output, run a "code
#' generation" procedure and print its output, then return \code{NULL}
#' invisibly. In this case \code{...} is ignored. See Details and Examples.
#' @param .kable If \code{TRUE}, call \code{\link[knitr]{kable}} on the final
#' object and return its results. Can also be a character string passed as the
#' \code{format} argument of \code{\link[knitr]{kable}}. Use \code{NULL} or
#' \code{FALSE} to just return a \code{data.frame} instead.
#' @param .graphical If \code{TRUE}, produce graphical output instead of
#' tabular output. Either one or two variables can be plotted.
#'
#' @details
#' The main uses cases of this function are to quickly explore data
#' interactively in the console, or create simple tabular summaries in R
#' markdown documents. Similar to \code{\link[base]{summary}}, but aims to be
#' as convenient as possible and produce nicer looking outputs.
#'
#' This function does different things depending on its inputs. The first
#' argument \code{data} is always a \code{data.frame} (or \code{NULL}). Next
#' come zero or more vector arguments, typically columns in \code{data} (which
#' do not need to be quoted) or functions thereof. Lastly, some optional
#' arguments that begin with `.` (dot) can be used to control certain aspects
#' of the output.
#'
#' When called with only a \code{data.frame} argument \code{data}, outputs a
#' table summarizing the variables in \code{data} including the columns:
#' \code{variable} (name), \code{label} (only present if at least one variable
#' has a \code{label} atrribute), \code{class}, \code{missing} (count) and
#' \code{example} (a single value from that variable, typically the first
#' nonmissing value).
#'
#' When called with \code{data} and one other argument, if the argument is
#' categorical outputs a frequency table and if it is continuous outputs a few
#' descriptive statistics (mean, standard deviation, median, min and max). The
#' \code{.max} option is used to decide if a numeric argument is continuous or
#' categorical.
#'
#' When called with more than one argument following \code{data}, those
#' arguments should all be categorical (\code{.max} is ignored in this case).
#' A frequency table is produced for the combinations of the categories, nested
#' from left to right. Percentages are not shown, just counts, and no sorting
#' is done (the categories appear in the order of factor levels).
#'
#' By default, the function \code{\link[knitr]{kable}} is used to format the
#' output so you get nice looking tables in both the console and in R markdown
#' documents.
#'
#' If the \code{.gen} argument is \code{TRUE}, then something different
#' happens.  Instead of outputing a table, the function prints code statements
#' into the console: a call to \code{lumos} for each variable in \code{data}.
#' The code can be copied from the console back into the script and used to
#' explore the \code{data.frame} one variable at a time. This is useful because
#' it saves the need to type the code for each variable.
#'
#' @return The value returned depends on the parameters. If \code{.kable} is
#' \code{TRUE} (the default) then an object of class \code{knitr_kable},
#' otherwise a \code{data.frame}. See Details and Examples.
#'
#' @examples
#' lumos(iris)
#' lumos(iris, Species)
#' lumos(iris, .gen=TRUE)  # Generate code statements to call lumos() on each column of iris.
#'
#' lumos(mtcars)
#' lumos(mtcars, wt)
#' lumos(mtcars, cyl)
#' lumos(mtcars, cyl, .pct=FALSE)
#' lumos(mtcars, cyl, gear)
#' lumos(mtcars, cyl, gear, am)
#' lumos(mtcars, cyl, gear, am, .blanks=FALSE, .kable=FALSE)
#' @export
#' @importFrom stats median sd
#' @importFrom utils head
lumos <- function(data=NULL, ..., .drop=TRUE, .max=20, .pct=TRUE, .order.by.freq=.pct, .blanks=TRUE, .recycle=TRUE, .missing=FALSE, .gen=FALSE, .kable=TRUE, .graphical=FALSE) {
    if (getOption("lumos", "off") != "on") {
        return(invisible(NULL))
    }
    if (.gen) {
        name <- deparse(substitute(data))
        labels <- sapply(data, attr, which="label")
        comments <- sapply(labels, function(x) if (is.null(x)) "" else paste0(" # ", x))
        fncalls <- sprintf("lumos(%s$%s)", name, names(data))
        fieldwidth <- max(nchar(fncalls))
        padding <- sprintf(paste0("%-", fieldwidth, "s"), fncalls)
        cat(sprintf("%s%s\n", padding, comments), sep="")
        return(invisible(NULL))
    }

    caption <- NULL

    nm <- as.character(unlist((match.call(expand.dots=FALSE)$...)))
    nm1 <- deparse1(substitute(data))

    if (is.null(data) && length(nm)==0) {
        return(invisible(NULL))
    } else if (!is.null(data) && is.atomic(data) && length(nm)==0) {
        x <- data
        nm1 <- gsub("^.*\\$", "", nm1)
    } else if (!is.null(data) && is.atomic(data)) {
        x <- c(list(data), eval(substitute(list(...)), NULL, enclos=parent.frame()))
        nm <- c(nm1, nm)
    } else {
        data <- as.data.frame(data)
        x <- eval(substitute(list(...)), data, enclos=parent.frame())
    }

    if (!is.atomic(data) && length(nm)==0) {
        if (.missing) {
            nmissing <- function(x) { sum(is.na(x)) }
            x <- sapply(data, nmissing)
            if (.order.by.freq) {
                x <- x[order(x, decreasing=T)]
            }
            if (.drop) {
                x <- x[x > 0]
            }
            tb <- data.frame(variable=names(x), missing=x, `%`=x/nrow(data), check.names=FALSE)
            if (nrow(tb) == 0) {
                return(invisible(tb))
            } else {
                if (.pct) {
                    tb$`%` <- sprintf("%.01f%%", 100*tb$`%`)
                } else {
                    tb$`%` <- NULL
                }
            }
        } else {
            getlabel <- function(x) { ifelse(is.null(y <- attr(x, "label")), nm1, y) }
            getdim   <- function(x) { if (!is.null(d <- dim(x))) sprintf("%d rows, %d columns", d[1], d[2]) }
            caption <- paste0(c(getlabel(data), getdim(data)), collapse="\n")

            nmissing <- function(x) { sum(is.na(x)) }
            nunique <- function(x) { length(unique(x)) }
            firstclass <- function(x) { class(x)[1] } 
            firstvalue <- function(x) { format(x[!is.na(x)][1]) } 
            getlabel <- function(x) { y <- attr(x, "label"); ifelse(is.null(y), NA, y) }
            tb <- data.frame(variable=names(data),
                label=sapply(data, getlabel),
                class=sapply(data, firstclass),
                missing=sapply(data, nmissing),
                unique=sapply(data, nunique),
                example=sapply(data, firstvalue))
            if (all(is.na(tb$label))) {
                tb$label <- NULL
            }
            rownames(tb) <- NULL
        }
    } else if (is.atomic(data) && length(nm)==0) {
        x <- data
        getlabel   <- function(x) { ifelse(is.null(y <- attr(x, "label")), nm1, y) }
        gettype    <- function(x) { paste("Type:", sprintf("%s/%s", class(x), typeof(x))) }
        getmissing <- function(x) {
            paste("Missing:", ifelse(any(is.na(x)),
                    sprintf("%s/%s (%s%%)",
                        sum(is.na(x)),
                        length(x),
                        formatC(100*mean(is.na(x)), format="f", digits=1)),
                    "none"))
        }
        if (.graphical) {
            return(univar(x=x, xlab=getlabel(x)))
        }
        caption <- paste0(c(getlabel(x), gettype(x), getmissing(x)), collapse="\n")
        if ((is.numeric(x) || is.integer(x)) && length(unique(x)) > .max) {
            tb <- c(
                Mean   = format(mean(x, na.rm=T), digit=3),
                SD     = format(sd(x, na.rm=T), digit=3),
                Median = format(median(x, na.rm=T)),
                Min    = format(min(x, na.rm=T)),
                Max    = format(max(x, na.rm=T)))
            tb <- data.frame(Statistic=names(tb), Value=tb)
        } else {
            x <- as.factor(x)
            if (.drop) {
                x <- droplevels(x)
            }
            tb <- table(x, useNA="ifany")
            pct <- as.numeric(prop.table(table(x, useNA="ifany")))
            tb <- data.frame(value=names(tb), N=as.numeric(tb), `%`=pct, check.names=FALSE)
            if (.order.by.freq) {
                tb <- tb[order(tb$N, decreasing=TRUE),]
            }
            tb <- head(tb, .max)
            n.o <- length(x) - sum(tb$N)
            if (n.o > 0) {
                tb <- rbind(tb, data.frame(value="Other", N=n.o, `%`=n.o/length(x), check.names=FALSE))
            }
            if (.pct) {
                tb$`%` <- sprintf("%.01f%%", 100*tb$`%`)
            } else {
                tb$`%` <- NULL
            }
            names(tb)[[1]] <- nm1
        }
    } else {
        if (.graphical) {
            getlabel <- function(x, default) { ifelse(is.null(y <- attr(x, "label")), default, y) }
            .nm <- as.character(nm)
            if (length(x) == 1) {
                return(univar(x=x[[1]], xlab=getlabel(x[[1]], .nm[[1]])))
            } else if (length(x) == 2) {
                return(bivar(x=x[[1]], xlab=getlabel(x[[1]], .nm[[1]]),
                             y=x[[2]], ylab=getlabel(x[[2]], .nm[[2]])))
            } else {
                stop("Can only plot up to 2 variables")
            }
        }
        x <- lapply(x, as.factor)
        if (.drop) {
            x <- lapply(x, droplevels)
        }
        if (.recycle) {
            x <- as.data.frame(x)
        }
        tb <- do.call(table, c(x, list(useNA="ifany")))
        tb <- as.data.frame(tb)
        names(tb) <- c(nm, "N")
        tb <- tb[do.call(order, tb),]
        tb <- tb[tb[[length(tb)]] != 0,]
        if (.blanks) {
            for (i in length(x):1) {
                combine <- function(...) paste(..., sep=";")
                id <- do.call(combine, Map(paste, unname(as.list(tb[, 1:i, drop=FALSE]))))
                id <- factor(id, levels=unique(id))
                y <- as.character(tb[[i]])
                y[duplicated(id)] <- ""
                tb[[i]] <- y
            }
        }
    }
    if (!is.null(.kable) && !isFALSE(.kable)) {
        if (isTRUE(.kable))
            knitr::kable(x=tb, row.names=FALSE, caption=caption)
        else {
            knitr::kable(x=tb, row.names=FALSE, caption=caption, format=.kable)
        }
    } else {
        tb
    }
}

#' @rdname lumos
#' @export
l <- lumos

.onLoad <- function(libname, pkgname) {
    op <- list(
        lumos = "on"
    )
    toset <- !(names(op) %in% names(options()))
    if (any(toset)) options(op[toset])
    invisible()
}

#' Turn \code{lumos} On or Off
#'
#' \code{lumos} can be turned on or off using an option. These are convenience
#' functions for doing so. When \code{lumos} is off, calling it will do
#' nothing; this can be useful to save time when running scripts in batch mode
#' where this output is not needed.
#'
#' @return \code{NULL} Called for its side effects.
#' @export
lumos_on <- function() {
    options(lumos="on")
    invisible(NULL)
}

#' @rdname lumos_on
#' @export
lumos_off <- function() {
    options(lumos="off")
    invisible(NULL)
}

