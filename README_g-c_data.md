### Readme for Getting & Cleaning Data Project
The code reads in the training & test data. The columns are readin as characters. This makes it easy to convert "character" number to a descriptive character, e.g., "1" is replaced with "WALKING"

The variable names are changed to ensure binding. The title "Volunteer" is used in lieu of "Subject"

Once the training & test data have same titles, the lists are binded

The columns to be extracted (20 means & 20 std values) is defined & the columns are subset. 

Average of each variable for each activity and each subject is calculated using the aggregate command.
