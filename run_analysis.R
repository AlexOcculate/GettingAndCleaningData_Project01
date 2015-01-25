library(dplyr)
#
read.my.file <- function( type , file.name ) {
   cat("|    |--- reading [", type , "] data...\n")
   fn <- paste( "UCI HAR Dataset" , file.name, sep="/" )
   df <- read.table( fn, sep = "" , h = F , na.strings = "NA" )
}
# Read, clean (normalize the "future" column names) and return a data frame
# with the "feature" data.
get.and.clean.feature.df <- function() {
   cat("|--- get.and.clean.feature.df...\n")
   df <- read.my.file( "feature" , "features.txt" )
   cat("|    \\--- munging feature data...\n")
   df$V2 <- make.names(df$V2, unique=TRUE)
   # some necessary "name" normalization...
   df$V2 <- sub(  "^t"       , "Time."     , df$V2 )
   df$V2 <- sub(  "^f"       , "Freq."     , df$V2 )
   df$V2 <- sub(  "^angle"   , "Angle"     , df$V2 )
   df$V2 <- gsub( "gravity"  , "Gravity"   , df$V2 )
   df$V2 <- gsub( "[Mm]ean"  , ".Mean."    , df$V2 )
   df$V2 <- gsub( "[Ss]td"   , ".Std."     , df$V2 )
   df$V2 <- gsub( "[.]tBody" , ".TimeBody" , df$V2 )
   df$V2 <- gsub( "[.][.]+"  , "."         , df$V2 )
   df$V2 <- gsub( "[.]+$"    , ""          , df$V2 )
   df
}
# Read, bind, rename and extract the "mean" and "std" columns from both:
# "train" and "test" "measure" data; and return it in a data frame.
get.and.clean.measure.df <- function() {
   cat("|--- get.and.clean.measure.df...\n")
   test.df <- read.my.file( "measure test" , "test/X_test.txt" )
   train.df <- read.my.file( "measure train" , "train/X_train.txt" )
   cat("|    \\--- munging measure data...\n")
   total.df <- rbind( test.df, train.df )
   feature.df <- get.and.clean.feature.df()
   colnames(total.df) <- c(feature.df$V2)
   # ( 53 + 33 ) Cols...
   df <- cbind( select( total.df, contains( "mean" ) ) , select( total.df, contains( "std" ) ) )
}
# Read and bind both "train" and "test" "subject" data;
# and return it in a data frame.
get.and.clean.subject.df <- function() {
   cat("|--- get.and.clean.subject.df...\n")
   test.df <- read.my.file( "subject test" , "test/subject_test.txt" )
   train.df <- read.my.file( "subject train" , "train/subject_train.txt" )
   cat("|    \\--- munging subject data...\n")
   total.df <- rbind( test.df, train.df )
}
# Read and bind both "train" and "test" "label" data;
# enrich it decoding the "activity" code into its description.
# Return it in a data frame.
get.and.clean.label.df <- function() {
   cat("|--- get.and.clean.label.df...\n")
   test.df <- read.my.file( "label test" , "test/y_test.txt" )
   train.df <- read.my.file( "label train" , "train/y_train.txt" )
   lookUp.activity <- read.my.file( "activity description" , "activity_labels.txt" )
   cat("|    \\--- munging label data...\n")
   total.df <- rbind( test.df, train.df )
   total.df["ActivityName"] <- lookUp.activity[ total.df$V1 , 2 ]
   total.df <- subset( total.df, select = -V1)
}
get.clean.and.tidy.df <- function() {
   cat("+ get.clean.and.tidy.df...\n")
   subject.df <- get.and.clean.subject.df()
   label.df <- get.and.clean.label.df()
   measure.df <- get.and.clean.measure.df()
   cat("|--- munging all data...\n")
   measure.df <- cbind( SubjectCode = subject.df$V1, ActivityName = label.df$ActivityName , measure.df )
   cat("|--- tidying all data...\n")
   tidy.df <- aggregate( subset(measure.df , select= -ActivityName ) , by = list( measure.df$ActivityName , measure.df$SubjectCode ) , FUN = mean )
   names( tidy.df )[ names( tidy.df ) == 'Group.1' ] <- "ActivityName"
   tidy.df <- subset( tidy.df , select = -Group.2 )
}
#
tidy.df <- get.clean.and.tidy.df()
cat("|--- writing file tidyData.txt with contents of tidy.df...\n")
write.table( tidy.df , "tidyData.txt" , sep = " ", row.name = FALSE , quote = FALSE)
cat("|--- housekeepping the environment...\n")
rm( list = c( "get.and.clean.feature.df" , "get.and.clean.label.df"
            , "get.and.clean.measure.df" , "get.and.clean.subject.df"
            , "get.clean.and.tidy.df"    , "read.my.file"
            )
)
cat("\\--- end.\n")
cat("\nLook the contents of [tidy.df] data frame! Bye!\n\n")
