# README — Choice Modeling and Marketing Experiments

## Overview

This week introduces two important analytical techniques used in marketing analytics:

1. Choice Modeling
2. Marketing Experiments (A/B Testing)

Choice modeling helps firms understand how product attributes influence consumer decisions. In this exercise, we explore how features such as RAM, battery power, screen resolution, and price category relate to consumer preferences for mobile phones.

Marketing experiments allow firms to test different marketing strategies in controlled environments. In this example, we simulate an A/B test comparing two marketing messages and measure their impact on conversion rates.

Students will learn how to:

- Analyze product attributes in a choice modeling context
- Understand the logic of conjoint-style analysis
- Design and analyze A/B tests
- Conduct statistical tests to evaluate marketing experiments


## Dataset

This exercise uses a mobile phone attributes dataset that contains technical specifications of different smartphones. The dataset includes variables describing device characteristics such as battery capacity, memory, processing speed, screen size, and price category.

These product attributes are commonly used in marketing analytics to understand how consumers evaluate tradeoffs between product features.


## Key Variables

Variable: Description

battery_power: Battery capacity of the phone

ram: RAM size

px_height: Screen pixel height

px_width: Screen pixel width

mobile_wt: Mobile phone weight

talk_time: Maximum talk time

int_memory: Internal memory

price_range: Target variable indicating phone price category


## Dataset Source

The dataset is publicly available on Kaggle:

Mobile Phone Price Dataset  
https://www.kaggle.com/datasets/heeraldedhia/mobile-phone-price-prediction

The dataset was originally designed for predictive modeling but is well suited for demonstrating choice modeling concepts and attribute tradeoffs in marketing analytics.


## Use in This Course

In this week’s materials, the dataset is used to demonstrate:

- Product attribute analysis
- Basic choice modeling intuition
- Logistic regression for predicting choice outcomes
- Marketing experimentation concepts using simulated A/B testing

Students will first explore how product features relate to price categories and then conduct a simulated experiment to test marketing messages.


## Files in This Folder

week09_choice_modeling_ab_testing/

- week09_choice_modeling_ab_testing.R
- mobile_phone_data.csv
- README.md


## Note for Students

Before running the analysis, ensure that the dataset file is placed in the same directory as the R script. The script includes commented steps that guide students through:

1. Importing and exploring the dataset
2. Understanding product attributes in a choice modeling context
3. Building a simple predictive model
4. Simulating and analyzing an A/B marketing experiment
