---
title: "Network Analysis: Integrating Social Network Theory, Method, and Application with R"
author: "Craig Rawlings, Jeffrey A. Smith, James Moody, and Daniel McFarland"
site: bookdown::bookdown_site
output: 
  bookdown::gitbook:
    config:
      toc:
        collapse: none
        scroll_highlight: true
        before: |
          <li><a href="https://dnac...">
          Network Analysis with R</a></li>
        after: null
    css: style.css
documentclass: book
bibliography: ["references.bib"]
link-citations: yes
description: "This is the website for the R labs associated with Network Analysis Integrating Social Network Theory, Method, and Application with R"
---

# Introduction

<img src="NA_RawlingsSmithMoodyMcFarland3.png"
style="float:right; margin: 0px 0px 0px 10px; width: 35%; height: 35%;" />

Welcome to the website for *Network Analysis: Integrating Social Network Theory, Method, and Application with R*. Here you will find the R tutorials that accompany the printed manuscript, which is available through Cambridge University Press. 

The printed manuscript offers substantive, theoretical and methodological discussions on how to conceptually conduct network analysis. The printed book thus offers the motivation and logic behind asking research questions from a network perspective. These tutorials serve as the practical counterpart, offering detailed examples on how to manipulate, visualize, summarize and analyze network data in R. The tutorials are motivated by substantive problems and include in-depth examples and interpretation. Many, but not all, of the examples are based on adolescents in school, as they serve as a familiar case study useful for drawing out larger, more general themes.

## How to Read the Book

The material on this website is meant to be paired with the printed manuscript. It is not an online version of the printed book. A reader would ideally read a chapter in the manuscript and then walk through the associated online R tutorials step-by-step.  Readers may choose to go through each R tutorial in order or opt to cover specific topics of interest, depending on the goals and experience of the reader. Each tutorial is self-contained, so that more experienced readers could choose to cover the tutorials out of order. For those readers not strictly following the published book, it is important to remember that the R tutorials are numbered to coincide exactly with the chapters in the published manuscript. 

The book covers a wide range of topics related to network analysis. These include, but are not limited to: 

* Data Management
* Missing Data
* Visualization
* Ego Networks
* Dyads and Triads
* Centrality
* Positions and Roles
* Affiliations and Duality
* Networks and Culture
* Statistical Network Models
* Diffusion
* Social Influence

Note that there are often multiple tutorials associated with a given topic. For example, Chapter 3 covers the basics of data management for network data in R, with one tutorial (3.1) covering cross sectional network data and another (3.2) covering longitudinal network data.

## Citations and Use

You can cite the tutorials on this website as: "".  The online R tutorials, like the printed manuscript, should not be reproduced without the written permission of Cambridge University Press & Assessment (copyright © Craig M. Rawlings, Jeffrey A. Smith, James Moody, and Daniel A. McFarland, 2023). The tutorials are, however, made freely available through this site.  

## Updates and Feedback

This book was built using R version 4.3.0 on June 01, 2023. The authors are committed to keeping these chapters as up to date as possible, especially when there are major updates to key packages. It is also possible that future versions of this online book will include additional tutorials on topics not currently covered. If you find errors or breaks in any of the code you can note them here:  https://github.com/JeffreyAlanSmith/Integrated_Network_Science/issues
or contact one of the authors directly. You can also find additional functions and data sets used throughout this book on the following github site: https://github.com/JeffreyAlanSmith/Integrated_Network_Science.

## Acknowledgments

There are many people we would like to thank for providing feedback and suggestions on these tutorials. Special thanks goes to Robin Gauthier and Sela Harcey for their insightful comments and support, as well as graduate students Gabriel Varela, Tom Wolff, and Joe Quinn for reviews and beta testing. These tutorials have also been taught at various network analysis classes at Duke, Stanford and UNL. The advice and suggestions of our students have greatly strengthened the material presented here. 

Many of the R labs presented in this textbook were built off prior versions developed at Stanford University by Daniel McFarland, Solomon Messing, Michael Nowak, Sean J. Westwood, and Sanne Smith. Chapter 5’s lab for NDTV drew on Skye Bender-deMoll’s materials; Chapter 12 on LDA/CA from Love Börjeson and Daniel McFarland; Chapter 13 concerning “ERGM” and “relevant” drew on Carter Butts’ materials; Chapter 15 on SIENA/SAOM drew on ICS materials. Finally, a great many resources from the Duke Network Analysis Center (DNAC) helped us in formulating elements in many of the labs. For example, Chapter 4 on missing data imputation drew on DNAC/James Moody and Jeffrey A. Smith’s work, as did Chapter 14 on diffusion. We are grateful to these institutions and individuals for sharing code and helping us formulate applications for each chapter’s theories.
