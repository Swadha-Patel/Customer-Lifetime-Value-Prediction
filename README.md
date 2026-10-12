# Customer lifetime value

By Swadha Patel

## Business question

An insurance relationship can span several renewals, claims, and policy changes. This project explores how customer and policy characteristics relate to the lifetime-value field in a vehicle-insurance dataset—and how a model could support more thoughtful retention planning.

## Data and scope

The source workbook, CLV.xlsx, contains customer and policy attributes such as coverage, income, monthly premium, policy count, claims, sales channel, vehicle class, and the recorded customer lifetime value. The project predicts that supplied value; it does not build a discounted cash-flow estimate from future premiums and claims.

## Workflow

### Explore the customer base

The R exploration examines the distribution of CLV, compares total value across customer and policy groups, and inspects correlations among numeric features. Group totals reflect both segment size and customer value, so a larger total does not imply a higher-value average customer.

### Compare modeling approaches

The modeling script sets a seed and creates a 70% training / 30% test split. It fits linear regression with backward feature selection, a regression tree, and a random forest to explore linear relationships and nonlinear patterns.

### Evaluate before applying

Linear regression and the tree are evaluated with held-out mean squared error. The forest code examines out-of-bag error and variable importance. Those evaluation sources are different, so they should not be presented as a single comparable accuracy leaderboard.

## Takeaway

The deliverable is an exploratory analysis, modeling scripts, and a written report—not a deployed customer-scoring service. The business opportunity is to combine value estimates with retention cost and customer need when designing outreach. Predicted value alone does not show which customers an intervention will retain.

## Interpretation and limitations

A next iteration would compare every model on the same untouched holdout, exclude the target from tuning predictors, and validate the timing of each feature. The original tuning block uses the full data frame and may include the target; its output should not be treated as independent validation. Sensitive attributes also need a purpose and fairness review before any operational use. No accuracy or realized revenue improvement is claimed here.

Based on the original repository’s EDA and modeling scripts, reviewed October 2026. Model performance was not independently rerun for this portfolio summary.

## Explore the artifacts

- [Read the original report](https://github.com/Swadha-Patel/Customer-Lifetime-Value-Prediction/blob/main/CLV%20Project%20Report.pdf)
- [Explore the R modeling workflow](https://github.com/Swadha-Patel/Customer-Lifetime-Value-Prediction/blob/main/CLV%20Project%20Models.R)

## Reproduction notes

Keep `CLV.xlsx` alongside the R scripts. The modeling script currently reads `Desktop/CLV.xlsx`; adjust it for your environment. Check workbook column names against the dotted names used by the formulas, install the R libraries listed by each script, and run in a fresh R session. Review the tuning limitations above before using any reported model metric. The original analysis files are preserved.
