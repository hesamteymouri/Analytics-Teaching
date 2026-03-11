# README — Choice Modeling and Marketing Experiments

## Overview

This week introduces two important analytical techniques used in marketing analytics:

1. Choice Modeling
2. Marketing Experiments (A/B Testing)

Choice modeling helps firms understand how individuals choose between competing alternatives based on the attributes of each option. In this exercise, we analyze how factors such as travel cost, travel time, and waiting time influence the transportation choices made by individuals.

Marketing experiments allow firms to test different strategies in controlled environments. In the second part of the exercise, we simulate an A/B test comparing two marketing messages and evaluate their effect on conversion rates.

Students will learn how to:

- Estimate a discrete choice model
- Understand how attributes influence decision-making
- Design and analyze A/B marketing experiments
- Conduct statistical tests to evaluate experimental results



## Dataset

This exercise uses the **Travel Mode Choice dataset**, which records individuals choosing between several transportation options:

- Car
- Train
- Bus
- Air

Each transportation alternative has attributes such as travel cost, travel time, and waiting time. The dataset is structured in **long format**, meaning each individual appears multiple times in the dataset—once for each alternative they considered.

The key variable `choice` indicates which option the individual selected.



## Key Variables

Variable: Description

individual: Unique identifier for each decision maker

mode: Transportation alternative (car, train, bus, air)

choice: Indicates whether the alternative was chosen (yes / no)

wait: Waiting time for the transportation option

travel: Travel time

vcost: Travel cost

gcost: Generalized cost

income: Income of the decision maker

size: Travel party size


## Dataset Source

The dataset is available from the **Applied Econometrics with R (AER)** dataset repository.

Travel Mode Choice Dataset  
https://vincentarelbundock.github.io/Rdatasets/csv/AER/TravelMode.csv

This dataset is widely used in econometrics and marketing analytics to demonstrate **multinomial logit and discrete choice models**.



## Use in This Course

In this week's materials, the dataset is used to demonstrate:

- Discrete choice modeling using real decision data
- How attributes such as cost and time influence consumer decisions
- Interpretation of coefficients in a multinomial logit model
- Marketing experimentation through simulated A/B testing
- Statistical tests used to evaluate marketing experiments

Students will first estimate a **choice model** explaining transportation decisions and then conduct a **simulated A/B experiment** to test marketing strategies.

## Dataset Source

Travel Mode Choice Dataset  
Source: Applied Econometrics with R (AER) package

Dataset repository:
https://vincentarelbundock.github.io/Rdatasets/csv/AER/TravelMode.csv

Documentation:
https://vincentarelbundock.github.io/Rdatasets/doc/AER/TravelMode.html

The dataset is distributed through the Rdatasets project, which provides
a curated collection of datasets originally included in many R packages.


## Note for Students

Before running the analysis, ensure that the dataset file is placed in the same directory as the R script.

The script includes step-by-step comments guiding you through:

1. Importing and exploring the dataset
2. Preparing the data for choice modeling
3. Estimating a multinomial logit model
4. Interpreting the results of the choice model
5. Simulating and analyzing an A/B marketing experiment
