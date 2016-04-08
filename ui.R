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

input.datafile <- fileInput('datafile', 'Data file',
  accept=c('text/plain', 'text/csv', 'text/comma-separated-values',
           'text/tab-separated-values', '.csv', '.tsv', '.txt'))
input.headerrow <- checkboxInput('headerrow', 'Header Row?', TRUE)
input.separator <- radioButtons('separator', 'Separator',
  c(Comma=',', Tab='\t', Semicolon=';', Colon=':', Pipe='|'), ',')
input.quotechar <- radioButtons('quotechar', 'Quote character',
  c(Double='"', Single="'", None=''), '"')
input.missingstr <- textInput('missingstr', 'Missing data indicator', '#N/A!')

input.windowfn <- selectInput('windowfn', 'Moving-window function',
  c('median', 'mean'))
input.windowwidth <- sliderInput('windowwidth', 'Window width',
  min=1, max=24, value=5, step=1, round=TRUE)
input.thresholdfn <- selectInput('thresholdfn', 'Threshold function',
  c('IQR', 'SD'))
input.thresholdscalar <- sliderInput('threshold.scalar', 'Threshold scalar',
  min=0.5, max=6, value=1.5, step=0.1)

shinyUI(fluidPage(
    titlePanel('filtertoy'),
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel('File Upload',
                    h3('File selection'),
                    input.datafile,
                    hr(),
                    h3('File format options'),
                    input.headerrow,
                    input.separator,
                    input.quotechar,
                    input.missingstr,
                    hr(),
                    h3('Column Selection'),
                    uiOutput('variableselection')),
                tabPanel('Filter Options',
                    input.windowfn,
                    input.windowwidth,
                    input.thresholdfn,
                    input.thresholdscalar))),
        mainPanel(
            plotOutput('filterresults')))))
