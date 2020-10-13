# Project 1 Notes

46 factor variables out of 80 (not including PID and response variable)

Dimension of `train` = `2051 x 82`

Dimension after turning all factor variables into dummy variables with first level removed (reference) and the original variables removed as well: `2051 x 299`



When removing all factors variables and running glmnet three of the most meaningful variables are latitude, longitude, and KitchenAbv_Gr.  Oddly Kitchen above ground isn't considered a factor variable even though it only has 4 possible values of {1, 2, 3, 0}



## R is stupid

Central Air which is all Y/N is not considered a character variable but is considered a factor variable

Kitchen_AbvGrd which is all 1/2 is not considered a factor variable 



## EDA

Correlation plots didn't like the categorical/factor variables either.  Also didn't like the sheer mass of predictors when converting into dummy variables

Used lasso as a way of EDA by looking at which factors at different levels (permissive, middle, restrictive) where left in.  Used the absolute value and a threshold that kept about 50% of the variables for that beta value so that noes that were shrunk close to zero were tossed from consideration.

Based on a table() the overall_cond and overall_qual seem very similar

## Scores

Using only predictors Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built with lasso and cv.min score of 0.2412681

Try2

- c(Kitchen_AbvGr, Fireplaces, Garage_Cars, Year_Built, Overall_Qual, Sale_Type, Bldg_Type, Bsmt_Exposure, Exter_Qual)
- With lasso and 1se
- Score: 0.1833668
- With lasso.min score went up to: 0.2260386

Try 3

- For some reason scaling the data outside of glmnet was not successful
- Using same predictors but with scaling inside of glmnet
- Lasso and 1se
- Score: 0.1830034
- Believe 'standardizing is on by default'

Try 4

- Using her code and method without windzorization
- Gives nonsensical negative House values
- Tried absolute value of predictions.  Also did not work

Try 5

- Using websites full version.
- Combine train and test data and then turn full matrix into dummies variables so that there isn't a mismatch, then seperate them back into train/test
- Use lambda 1se score: 0.1678892
- Use lambda min score: 0.1595369
- Ridge with lambda min gives almost exact same score:  0.1593003
- Ridge with lambda 1se gives score: 0.1626789

Try 6

- Removing high outliers
- All predictors with dummies
- Lasso with 1se = 0.156592
- Lasso with min = 0.1511794
- Ridge with min = 0.1471808
- Ridge with 1se = 0.1446536
- Removing her columns - ridge - 1se = 0.1433974

Try 7

- Her whole code including windzoriation
- Then combine/split train test for correct levels
- Lasso -selected variables -  1se - ridge = 0.1279549
- Lasso -selected variables -  min - ridge = 0.1188488