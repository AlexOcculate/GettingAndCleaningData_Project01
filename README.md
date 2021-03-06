## Coursera - Getting and Cleaning Data Course

### Overview:

This file explains how the **run_analysis.R** script should work. It assumes you have:  
  - **[R]** and **[RStudio]** already installed in your computer, and
  - **[dplyr]** package already installed.
  - Connection to the internet, as it may need to download some packages.

### Version:

1.0

### Execution Steps:

1) Fire up the **[RStudio]**.  
2) Ensure your working directory is the root of this project.

* Use **getwd()** to verify the current working directory.   
* Use **setwd("...")** to set up the current working directory.   

3) In the current directory you should see:  
* **UCI HAR Dataset** - Directory  
* **CodeBook.md** - The **codebook** describing the whole process from **raw data** to **tidy data**.
* **README.md** - This flatfile you are reading right now.  
* **run_analysis.R** - A ".R" script that will be executed to generate the **tidy data**. 
* **tidyData.txt** - (OPTIONAL) A text flatfile that will contain the **tidy data** generated by **run_analysis.R** script.  

4) Execute **source("run_analysis.R")** within **[RStudio]**.
> This will **automatically load and execute** the complete script.  

5) You should see an output similar to the following:  

> If it is the very first time you execute this steps in your current session, some extra messages might
> appear warning you about some masked objects in the **[dplyr]** library!

```
   + get.clean.and.tidy.df...
   |--- get.and.clean.subject.df...
   |    |--- reading [ subject test ] data...
   |    |--- reading [ subject train ] data...
   |    \--- munging subject data...
   |--- get.and.clean.label.df...
   |    |--- reading [ label test ] data...
   |    |--- reading [ label train ] data...
   |    |--- reading [ activity description ] data...
   |    \--- munging label data...
   |--- get.and.clean.measure.df...
   |    |--- reading [ measure test ] data...
   |    |--- reading [ measure train ] data...
   |    \--- munging measure data...
   |--- get.and.clean.feature.df...
   |    |--- reading [ feature ] data...
   |    \--- munging feature data...
   |--- munging all data...
   |--- tidying all data...
   |--- writing file tidyData.txt with contents of tidy.df...
   |--- housekeepping the environment...
   \--- end.

   Look the contents of [tidy.df] data frame! Bye!
```

5) Now you should have a data frame called **tidy.df** in your current session. To verify it execute:
```
   dim( tidy.df )
   str( tidy.df )
   summary( tidy.df )
``` 

6) A flatfile in the current working directory called **tidyData.txt** should exist with the contents of **tidy.df**, to load and verify it execute:  

```
   df <- read.table( "tidyData.txt" , header = TRUE , sep = " " )
   dim( df )
   str( df )
   summary( df )
```

### Data Sources:

The data sources used in this work originated from:

* [Human Activity Recognition Using Smartphones]

and can be downloaded at:

* <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

[R]:http://www.r-project.org/
[RStudio]:http://www.rstudio.com/
[dplyr]:http://cran.r-project.org/web/packages/dplyr/index.html
[Human Activity Recognition Using Smartphones]:http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
