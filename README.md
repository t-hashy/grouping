# GROUPING WITH COSINE SIMILARITY

## Abstruct
This code is written with a purpose that how to make an optimization of grouping.
The main strategy is too simple to use just cosine similarity, which seems far from enough though, just good for a first step.

## Strategy
1. Get the data (survey with variables and the names of respondentees).
2. Pre-processing the data, mainly for standardization of strings.
3. Create dataframes for each variables with columns of `name` and `variable name`.
4. Convert the dataframes into matrix with usign `table()` function and transpose with `t()`, for making up the matrixes of rows of variable names and columns of names.
5. Calcurate cosine similarity in each matrixes with function of `scales::cosine()` and this would generate names-names matrixes which have values of cosine similarity.
6. Set hyper parameter for the weights (0 ~ 10: how much of the powers of each variables for grouping) and diversity (-1: the more diversity the better, or 1: the less diversity the better).
7. Merge matrixes (precisely `1 - cosine similarity` matries because of converting them into distance) with mulplying weiths and diversity.
8. Clustering from nearest ones to farest ones, one by one.
9. Check conditions, and export if needed.

## Notaions
- Main survey has 4 questions which is included in this formula.
- Sub survey has only 1 question item which is used at this time, because sub-survey has been made for another purpose and I just borrow the part of it for my usage.
- Variable names is not important and for my time saving, I uploaded raghly the same. Please not mind.
