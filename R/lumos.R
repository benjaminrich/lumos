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
lumos <- function(data=NULL, ..., .drop=TRUE, .max=20, .pct=TRUE, .order.by.freq=.pct, .blanks=TRUE, .recycle=TRUE, .missing=FALSE, .gen=FALSE, .kable=TRUE) {
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
        getlabel   <- function(x) { ifelse(is.null(y <- attr(x, "label")), nm1, paste0(nm1, ": ", y)) }
        gettype    <- function(x) { paste("Type:", sprintf("%s/%s", class(x), typeof(x))) }
        getmissing <- function(x) {
            paste("Missing:", ifelse(any(is.na(x)),
                    sprintf("%s/%s (%s%%)",
                        sum(is.na(x)),
                        length(x),
                        formatC(100*mean(is.na(x)), format="f", digits=1)),
                    "none"))
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

