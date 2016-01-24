---
title       : People Killed by Police 
subtitle    : shiny Data Exploration App
author      : Josh Morel
job         : Data Scientist
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [htmlwidget]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Background

In response to the string of highly publicized police killings of African Americans in the United States, <b>The Guardian</b> initiated a project to build a more comprehensive record of people killed by police in the United States. This project, [The Counted](http://www.theguardian.com/us-news/ng-interactive/2015/jun/01/the-counted-police-killings-us-database), provides a very interesting data set to expand upon for further investigation on a very timely topic - police brutality in the United States.

<div style='text-align: center;'>
<a title="By The All-Nite Images [CC BY-SA 2.0 (http://creativecommons.org/licenses/by-sa/2.0)], via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File%3ABlack_Lives_Matter_protest.jpg">
<img width="512" alt="Black Lives Matter protest" src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Black_Lives_Matter_protest.jpg/512px-Black_Lives_Matter_protest.jpg"/>
</a></div>

--- .class #id 

## The Data

The primary data of interest is <b>Rate of People Killed by Police by Population</b>. As this plot shows, <b>Wyoming, New Mexico & Oklahoma</b> are distinct outliers. Further analysis could identify what confounding variables might be common to these states explaining the higher rate. This plot is rendered in `ggplot2`. I ran into too much difficulty getting `ggvis` to work in `slidify`, unfortunately.

<img src="assets/fig/ggvisdata-1.png" title="plot of chunk ggvisdata" alt="plot of chunk ggvisdata" style="display: block; margin: auto;" />

--- .class #id 

## The App - Front Page

The app is published and available on [Shiny Apps](https://josh-morel.shinyapps.io/killed_by_police/), consisting of three tabs. The front page and first tab hosts an interactive `ggvis` dot-plot of <b>People Killed by Police</b> by state. The interactive features include:

<div style='text-align: center;'><img width="60%" alt="Killed by Police Dot-plot" src="assets/img/dotplot_demo.png"/></div>

--- .class #id 

## The App - Leaflet Map & Downloadable files

The second tab provides an interactive leaflet map. Clicking on the data-point displays a pop-up, which links back to the specific The Counted database entry. 

The third and final tab provides downloadable state-level data and the code book. 

<div style='text-align: center;'><img width="80%" alt="Killed by Police Leaflet" src="assets/img/leaflet_demo.png"/></div>

--- .class #id 

## Acknowledgements

I would like to provide the following acknowledgements for the data used:
* [The Guardian](http://www.theguardian.com/us-news/ng-interactive/2015/jun/01/the-counted-police-killings-us-database) (People Killed by Police)
* [United States Census Bureau](http://www.census.gov/popest/data/index.html) (Population Demographics)
* [Federal Bureau of Investigation](https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2014/crime-in-the-u.s.-2014) (Crime & Police)

And of course to the hard working folks who build, maintain and educate on `R`, `RStudio`, `slidify`, `shiny`, `ggvis`, `leaflet`, `ggplot2`, `data.table`, `knitr`, `pandoc` and more.

For the code used to build the shiny app and to do the initial data processing visit by [github repository](https://github.com/kitjosh1050/killed_by_police).

Author: [Josh Morel](kitjosh1050.github.io)

