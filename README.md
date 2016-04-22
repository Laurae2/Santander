# Santander
Kaggle Santander

## TO-DO

Nothing yet.

## Important topics

### Unbalanced data set + Noisy
Cross-Validation issue and Public LB unreliability: https://www.kaggle.com/c/santander-customer-satisfaction/forums/t/19946/who-believe-in-standard-deviation
Game of randomness: https://www.kaggle.com/c/santander-customer-satisfaction/forums/t/20299/does-it-make-sense-to-loop-through-seed-to-get-best
Example of feature selection: https://www.kaggle.com/kobakhit/santander-customer-satisfaction/0-84-score-with-36-features-only/discussion
Public LB unrepresentative of the Private LB: https://www.kaggle.com/c/santander-customer-satisfaction/forums/t/20226/best-way-to-the-top-of-the-leader-board
Duplicated Rows (will add more myself about the repo): https://www.kaggle.com/c/santander-customer-satisfaction/forums/t/20276/duplicated-rows
Data Dictionary: https://www.kaggle.com/c/santander-customer-satisfaction/forums/t/19291/data-dictionary

## To know before you start working on Santander

Major issues:

* There are many duplicated rows
* Many variables are linear combinations of other variables: any machine learning algorithm requiring non ill-conditioned matrix will fail hard (ex: Linear Discriminant Analysis)
* There are many outliers: 99999999 values, etc.
* There are variables that are constant
* There are duplicates where the target is different. I'll compile a list later
