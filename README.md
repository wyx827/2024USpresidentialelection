# 2024 US Election Prediction

## Overview

This repository contains the code, data, and analysis used to forecast the outcome of the 2024 US Presidential Election based on polling data. The analysis is conducted in R, utilizing a publicly available dataset of high-quality polls. The primary objective of this project is to develop a linear or generalized linear model to predict the election results, with a particular emphasis on polls featuring Donald Trump as a candidate.

## File Structure

The repo is structured as:

-   `data/00-simulated_data` contains the simulated dataset.
-   `data/01-raw_data` contains the raw data as obtained from https://projects.fivethirtyeight.com/polls/president-general/2024/national/.
-   `data/02-analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains the RDS file for the fitted model. 
-   `other` contains details about literature, LLM usage, and sketches.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper.
-   `scripts` contains the R scripts used to download, simulate, clean, test data, exploratory data analysis and model the data.

## Statement on LLM usage

The ChatGPT-4 model assisted in writing some aspects of the code and refining the wording. The complete chat history can be found in `other/llm_usage/usage.txt`.
