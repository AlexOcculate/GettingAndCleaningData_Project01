---
title: "README.md"
date: January 2015
output: html_document
---

## Coursera - Getting and Cleaning Data Course 
### Project Assignment

This file explains how the run_analysis.R script should work.
It assumes you have R and RStudio already installed in your computer and connection to the internet as it may need to download a specific package.

This assignment uses data from <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>


1) Ensure your working dir is the root of this project.
2) In the current directory you should see:
    - UCI HAR Datase - Directory
    - README.md - This flatfile you are reading right now.
    - run_analysis - A ".R" script that will be executed to generate the 'tidy data'. 
    - tidyData.txt - (OPTIONAL) A text flatfile that will contain the 'tidy data' generated by 'run_analysis.R' script.
3) Fire up the RStudio.
4) Execute source("run_analysis.R") within RStudio.  
5) To read the generated text flatfile with the 'tidy data' execute:

```
        tidy.df <- read.table( "tidyData.txt" , header = TRUE , sep = " " )
        dim(tidy.df)
        str(tidy.df)
        summary(tidy.df)
``` 
