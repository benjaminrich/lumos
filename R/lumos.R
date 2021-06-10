#' Shed Light on Your Data
#'
#' Like a magic wand that allows you to explore a \code{data.frame}.  It is so
#' powerful, it can be abbreviated to the single letter \code{l}.
#'
#' @param data A \code{data.frame} (or, and object that can be converted to one
#' by call \code{link{as.data.frame}}).
#' @param ... Further argument, evaluated within \code{data} (so they can
#' directly refer to columns in data without quotes).
#' @param .drop If \code{TRUE}, unused factor levels are dropped.
#' @param .max For a single categorical variable, the maximum number of unique
#' categories to show (can be \code{Inf}).  For a single numberic variable the
#' maximum number of unique values for a numeric variable to be treated as
#' categorical.
#' @param .pct If \code{TRUE}, show percents along with counts for single
#' categorical variables.
#' @param .order.by.freq If \code{TRUE}, for a single categorical variable,
#' show the categories in decreasing order of frequency, from top to bottom
#' (i.e. show the most frequent categories on top).
#' @param .blanks If \code{TRUE}, insert blank spaces instead of repeating
#' consecutive values that are identical.
#' @param .gen If \code{TRUE}, instead of the usual output, run a "code
#' generation" procedure and print its output, then return \code{NULL}
#' invisibly. In this case \code{...} is ignored.
#' @param .kable If \code{TRUE}, call \code{\link[knitr]{kable}} on the final
#' object before returning.
#'
#' @return The value returned depends on the parameters. If \code{.kable} is
#' \code{TRUE} (the default) then an object of class \code{knitr_kable},
#' otherwise a \code{data.frame}. See Examples.
#'
#' @examples
#' lumos(iris)
#' lumos(iris, .gen=TRUE)  # Generate code statements to call lumos() on each column of iris.
#' lumos(iris, Species)
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
lumos <- function(data=NULL, ..., .drop=TRUE, .max=20, .pct=TRUE, .order.by.freq=.pct, .blanks=TRUE, .gen=FALSE, .kable=TRUE) {
    if (.gen) {
        name <- deparse(substitute(data))
        labels <- sapply(data, attr, which="label")
        comments <- sapply(labels, function(x) if (is.null(x)) "" else paste0(" # ", x))
        cat(paste0("lumos(", name, ", ", names(data), ")", comments, "\n"),sep="")
        return(invisible(NULL))
    }
    data <- as.data.frame(data)
    x <- eval(substitute(list(...)), data, enclos=parent.frame())
    if (length(x) == 0) {
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
    } else {
        nm <- as.character(unlist((match.call(expand.dots=FALSE)$...)))
        if (length(x) == 1) {
            x <- x[[1]]
            getlabel   <- function(x) { y <- attr(x, "label"); ifelse(is.null(y), nm, paste0(nm, ": ", y)) }
            gettype    <- function(x) { paste("Type:", sprintf("%s/%s", class(x), typeof(x))) }
            getmissing <- function(x) { paste("Missing:", ifelse(any(is.na(x)), sprintf("%s/%s (%s%%)", sum(is.na(x)), length(x), 100*mean(is.na(x))), "none")) }
            caption <- paste0(c(getlabel(x), gettype(x), getmissing(x)), collapse="\n")
            if (is.numeric(x) && length(unique(x)) > .max) {
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
                names(tb)[[1]] <- nm
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
            }
        } else {
            x <- lapply(x, as.factor)
            if (.drop) {
                x <- lapply(x, droplevels)
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
    }
    if (.kable) {
        knitr::kable(tb, row.names=FALSE, caption=get0("caption"))
    } else {
        tb
    }
}

#' @rdname lumos
#' @export
l <- lumos

