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

library(shiny)
library(scales)
source('filter.R')

valid.data <- function(frame)
{
    if (is.null(frame))
        FALSE
    else if (ncol(frame) < 3)
        FALSE
    else
        TRUE
}

shinyServer(function (input, output, session) {
    reactive.data <- reactive({
        validate(
            need(!is.null(input$datafile$datapath), 'Please upload a file.')
        )

        read.delim(file=input$datafile$datapath, sep=input$separator,
          quote=input$quotechar, header=input$headerrow,
          na.strings=input$missingstr, stringsAsFactors=FALSE)
    })

    reactive.id <- reactive({
        validate(
            need(valid.data(reactive.data()),
              'Data file invalid or has too few columns.'),
            need(!is.null(input$idvar),
              'No ID variable selected.')
        )
        as.character(reactive.data()[[input$idvar]])
    })

    reactive.time <- reactive({
        validate(
            need(valid.data(reactive.data()),
              'Data file invalid or has too few columns.')
        )

        time.data <- reactive.data()[[input$timevar]]
        try <- as.numeric(time.data)
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format='%Y-%m-%d')
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format='%Y/%m/%d')
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format='%m-%d-%Y')
        if (any(!is.na(try)))
            return(try)

        try <- as.Date(time.data, format='%m/%d/%Y')
        if (any(!is.na(try)))
            return(try)

        validate(
            need(FALSE, 'Invalid time column.')
        )
    })

    reactive.value <- reactive({
        validate(
            need(valid.data(reactive.data()),
              'Data file invalid or has too few columns.')
        )

        as.numeric(reactive.data()[[input$valuevar]])
    })

    reactive.valid.data <- reactive({
        !is.null(reactive.id()) && !all(is.na(reactive.id())) &&
          !is.null(reactive.time()) && !all(is.na(reactive.time())) &&
          !is.null(reactive.value()) && !all(is.na(reactive.value()))
    })

    output$filterresults <- renderPlot({
        validate(
            need(reactive.valid.data(), 'Invalid data.')
        )

        id <- reactive.id()
        time <- reactive.time()
        value <- reactive.value()
        sort.order <- order(id, time)
        id <- id[sort.order]
        time <- time[sort.order]
        value <- value[sort.order]

        unique.id <- unique(id)

        n.series <- length(unique.id)

        par(mfrow=c(ceiling(n.series / 2), 2),
          mar=par('mar') / 2)

        window.fn <- switch(input$windowfn,
            median=window.median,
            mean=window.mean,
            NULL
        )
        validate(need(!is.null(window.fn), 'Invalid window function.'))

        threshold.fn <- switch(input$thresholdfn,
            IQR=threshold.iqr,
            SD=threshold.sd,
            NULL
        )
        validate(need(!is.null(threshold.fn), 'Invalid threshold function.'))

        sapply(unique.id, function (this.id) {
            this.time <- time[id == this.id]
            this.value <- value[id == this.id]
            filter.keep <- filter(this.value, window.fn, input$windowwidth,
              threshold.fn, input$threshold.scalar)
            plot(this.value ~ this.time, col=c('red', 'blue')[filter.keep + 1],
              main=this.id, xlab='Time', ylab='Value', cex=2)
        })
    }, height=function () {
        ceiling(length(unique(reactive.id()))) * 200
    })

    output$variableselection <- renderUI({
        if (!valid.data(reactive.data())) {
            p('Invalid data table.')
        } else {
            var.names <- names(reactive.data())
            tagList(
                selectInput('idvar', 'ID Column', var.names, var.names[1]),
                selectInput('timevar', 'Time Column', var.names, var.names[2]),
                selectInput('valuevar', 'Value Column', var.names, var.names[3])
            )
        }
    })
})
