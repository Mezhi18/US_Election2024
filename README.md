# Predicting the Winner of the 2024 USA elections

## Overview
We analyse the probability of Kamala Harris winning in the 7 swing states of USA using baysian modeling;the states we look at are Arizona, Michigan, Pennsylvania, Nevada, Georgia, Wisconsin and North Carolina. USA is a large country with its national economy affecting the global economic conditions, which is why predicting the future President of USA will help understand the future economic condiotion of the world. Through baysian modelling we found that North Carolina (47.26% support for Harris), Nevada(46.43% support for Harris), Wisconsin(48.38% support for Harris), Michigan(47.30% support for Harris), Pennsylvania(48.02% support for Harris) will vote Kamala Harris, while the rest of the swing states might vote Donald Trump, meaning that majority of the swing states will vote Kamala Harris. Provided this we predict that Kamala Harris will win the election. By creating a baysian model based on 'polls of polls'  where we compare the results from different polls, we are able to make this prediction.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from @fivethirtyeight2024.
-   `data/analysis_data` contains the cleaned dataset that was constructed. contains parquet too
-   `model` contains fitted models. 
-   `other` contains relevant literature, details about LLM chat interactions, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

Aspects of the code were written with the help of ChatGPT. It was used to help with anything from finding inspiration for budget allocation, to graphing, to making correct citations. Sakura: https://chatgpt.com/share/67281e8a-c874-8012-a645-ad96b0616a6a, Cristina: https://chatgpt.com/share/67291d47-f470-8004-bb15-2536144a6ae6, Yan:https://chatgpt.com/g/g-TgjKDuQwZ-r-wizard/c/67291058-91c8-8001-b702-4e7dcc146365.
