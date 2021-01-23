# Shiny Apps for Statistics Education

Below are descriptions of all the apps available in this repo, sorted by topic.


## Probability

**diagnostic-tests**  
Displays a scatterplot comparing healthy and sick patients on a hypothetical biomarker and allows the user to select the diagnostic cutoff for the biomarker expression. The output includes an ROC curve with the sensitivity and specificity for the chosen cutoff marked.

**bayes-theorem**  
Gives a mosaic representation of Bayes' Theorem in the context of a disease/screening test example. Users can input the prevalence of disease in the population (x-axis), sensitivity (left bar), and specificity (right bar). The positive predictive probability is given in the plot's title, calculated from the input parameters.


## Probability Distributions

**normal-dist**  
Allows the user to change the mean and standard deviation of the normal distribution to compare location and spread. Also includes the standard normal distribution for reference and can be used to discuss z-scores/standardization.

**chisq-dist**  
Allows the user to see how the chi-squared density function changes with the degrees of freedom.

**t-dist**  
Allows the user to see how the t distribution density function changes with the degrees of freedom. Includes an option to display the standard normal distribution for reference.


## Statistical Inference

**sampling-dist-p-hat**  
Shows the true sampling distribution of the sample proportion for a specified sample size and true population proportion p (with probabilities from a binomial model). Includes an option to show the normal distribution to approximate the sampling distribution.

**confidence-intervals**  
Plots a histogram of sample data along with a confidence interval. Designed to highlight the difference between confidence coverage (input) and sample coverage (plot title). This is because the confidence interval is a statement about where the *population mean* lies, not about the distribution of observed values. Samples are simulated from a population with mean 20.

**hypothesis-test-power**  
Displays (1) the null distribution and rejection region for a specified null proportion, alpha level, and sample size. Optionally, also displays the alternative distribution and power. Test assumed to be lower tailed.

**anova-sums-squares**  
Allows the user to set the between-group sum of squares (SSB) and within-group sum of squares (SSW or SSE), then displays a scatterplot of the data and an F distribution with calculated test statistic and p-value for the given values of SSB and SSE.


## Models

**regression-residuals**  
Displays a scatterplot of data simulated from a linear model along with a histogram contrasting the distribution of the *y* variable (grey) to the distribution of the residuals within an window around the specified *x* coordinate (light blue).

**leverage**  
Displays a scatterplot with regression line. The user can select the point with the median x value or the outlier and adjust the y value of the point. The regression line will update to show the changes (influence) the point can have.