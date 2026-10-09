IPL MATCH PREDICTION - R PROJECT
================================
Predicting IPL match outcomes with Logistic Regression
(baselines: Linear Regression, Random Forest)

FILES
  ipl_matches.csv                  dataset: 500 matches x 15 columns (2008-2024)
  01_generate_ipl_dataset.R        script that created the dataset
  02_ipl_logistic_regression.R     full analysis: cleaning, EDA, 3 models, evaluation
  console_output.txt               full R console log from running script 02
  model_results.csv                final comparison table
  glm_coefficients.csv             logistic regression coefficients
  plots/                           all charts used in the slides

HOW TO RUN (R 4.x or RStudio)
  1. Set the working directory to this folder:  setwd("path/to/this/folder")
  2. (Optional) Rscript 01_generate_ipl_dataset.R    # recreates ipl_matches.csv
  3. source("02_ipl_logistic_regression.R")          # installs missing packages, runs everything
  Packages: caret, ranger, pROC, ggplot2, corrplot

IMPORTANT - ABOUT THE DATA
  ipl_matches.csv is SYNTHETIC: it follows the column layout of the public Kaggle
  IPL match dataset (team1, team2, toss_winner, toss_decision, venue, winner,
  team1_runs, team2_runs, team1_wickets, ...) but the match results were simulated
  (team strength + home advantage + small toss effect), not taken from real games.
  To use real data: download the Kaggle IPL file, rename/convert it to ipl_matches.csv
  with the same column names, and run script 02 again. Metrics will change.

MODELLING NOTE
  team1_runs, team2_runs, team1_wickets and winner are only known AFTER a match, so
  they are used for EDA but excluded from the models (data leakage). Models use only
  pre-match features: team1, team2, toss_team1, toss_decision, team1_home, team2_home.
