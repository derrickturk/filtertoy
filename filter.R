# filtertoy
# a thing for filtering time series
# (c) 2016 dwt | terminus data science, LLC

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# relative-residual vs moving window smoothing point filter
filter <- function (ts, window.fn, window.width, threshold.fn, threshold.scalar)
{
    pts.back <- floor(window.width / 2)
    pts.fwd <- window.width - pts.back - 1
    ts.len <- length(ts)
    ts.smoothed <- sapply(seq_along(ts), function (i) {
        begin.idx <- max(1, i - pts.back)
        end.idx <- min(ts.len, i + pts.fwd)
        window.fn(ts[begin.idx:end.idx])
    })
    ts.resid <- (ts - ts.smoothed) / ts.smoothed
    ts.threshold <- threshold.fn(ts.resid, threshold.scalar)
    ts.resid >= ts.threshold[1] & ts.resid <= ts.threshold[2]
}

window.median <- function (ts) median(ts, na.rm=TRUE)
window.mean <- function (ts) mean(ts, na.rm=TRUE)

threshold.iqr <- function (resid, scalar) {
    quartiles <- quantile(resid, c(0.25, 0.75), na.rm=TRUE)
    iqr <- quartiles[2] - quartiles[1]
    c(quartiles[1] - iqr * scalar, quartiles[2] + iqr * scalar)
}

threshold.sd <- function (resid, scalar) {
    sd.resid <- sd(resid, na.rm=TRUE)
    mean.resid <- mean(resid, na.rm=TRUE)
    c(mean.resid - sd.resid * scalar, mean.resid + sd.resid * scalar)
}
