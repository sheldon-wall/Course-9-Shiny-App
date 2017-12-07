# Course 9 - Shiny Applicaton README

### Shiny Application with Plot_ly - Version 1.0
### Sheldon Wall
### Coursera class participant

## Course Project Requirements Overview

Your Shiny Application

Write a shiny application with associated supporting documentation. The documentation should be thought of as whatever a user will need to get started using your application.
Deploy the application on Rstudio's shiny server
Share the application link by pasting it into the provided text box
Share your server.R and ui.R code on github

The application must include the following:
- Some form of input (widget: textbox, radio button, checkbox, ...)
- Some operation on the ui input in sever.R
- Some reactive output displayed as a result of server calculations
- You must also include enough documentation so that a novice user could use your application.
- The documentation should be at the Shiny website itself. Do not post to an external link.

The contents within this github are:

1. The raw data used
2. The shiny application code 

The details for these components can be found in the contents section below.

## Summary

The complete project should include the following files: 

### 1. Raw Data

The raw data is stored in the data folder and contains compressed file globalterrorismdb_0617dist.csv.
The data for this project come from this source: <https://www.start.umd.edu/gtd/> 

### 2. app.r 

This code contains the shiny application UI and Server components.  The application contains an input panel where
the user can enter in summarization choice (yearly, monthly or none) and a date range.  When Apply button is pressed
the Shiny application uses those selection to scatterplot the activity and intensity of recorded terrorist activities based
on the date range selected.  If a plot point is clicked then a bar chart is produced to show distribution of intensity 
based on number of killed, wounded and property damage.

### 3. README.md

This file
