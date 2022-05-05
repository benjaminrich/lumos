"%||%" <- function(x, y) {
    if (is.null(x)) y else x
}

univar <- function(x,
    xlab = table1::label(x) %||% deparse1(substitute(x)),
    ...,
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


bivar <- function(x, y, 
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

lumos_plot <- function(...) {
    .call <- match.call()
    .call[[1]] <- `lumos`
    .call[[".graphical"]] <- TRUE
    eval(.call)
}

#' @rdname lumos
#' @export
ll <- lumos_plot

