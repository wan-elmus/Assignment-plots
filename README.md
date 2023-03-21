## Brief explanation

**Note:** *EDA_PCA.R is the working file, it implements everything*

This program performs exploratory data analysis (EDA) on a dataset named "mydata". The dataset is loaded from a CSV file named "MLData2023.csv" that is assumed to be in the current working directory. The data are split into two classes (Class 0 and Class 1), and 300 samples are randomly selected from each class to create a sub-sample of 600 observations.

The code then provides summary statistics and graphical representations of the data. It prints the following information for each categorical feature in the dataset:

The category name
The count (N) of each category
The percentage of each category
For each numerical feature in the dataset, it prints the following summary statistics:

The feature name
The number of observations
The number of missing values
The minimum value
The maximum value
The mean value
The median value
The skewness value
The code then defines two functions for checking and replacing outliers in the dataset. It applies the outlier detection function to the "DYNRiskA.Score" feature and prints the number of outliers detected.

Finally, the program performs principal component analysis (PCA) on the numerical features and creates a scatter plot of the first two principal components colored by the class label. The code also displays the loadings of the principal components as text labels on the plot.

## After running the code

[Running] Rscript "/home/sensei/Repos/Assignment_plots/1R_sample.R": This line indicates that the R script 1R_sample.R is being run using the Rscript command, and that the script is located in the /home/sensei/Repos/Assignment_plots/ directory.

Attaching package: ‘dplyr’: This shows that the dplyr package has been loaded and is ready for use in the R environment.

The following objects are masked from ‘package:stats’: filter, lag: This line indicates that the filter and lag functions from the stats package have been masked by functions of the same name from the dplyr package. This means that if you try to use filter or lag, you will be using the dplyr versions of those functions.

The following objects are masked from ‘package:base’: intersect, setdiff, setequal, union: This linshows us that the intersect, setdiff, setequal, and union functions from the base package have been masked by functions of the same name from the dplyr package.

[1] 600 14: This line tells us that the data frame that was just created has 600 rows and 14 columns.
'data.frame': 600 obs. of 14 variables:: This line gives us some additional information about the data frame that was just created. It tells us that the data frame has 600 observations (rows) and 14 variables (columns).

The remaining lines provide information about the categorical and numeric features in the data frame, including their categories, counts, and summary statistics.
