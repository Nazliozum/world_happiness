
This shiny app shows happiness scores of countries around the world in 2015, 2016 and 2017.

## Overview

Experts across fields such as economics, psychology, and public policy have pointed out that measurements of well-being are important in assessing the progress of nations. Hence, governments, organizations and civil society increasingly use happiness indicators to inform their policy-making decisions. 
I propose building a data visualization app that allows policy-makers and civil society leaders of all levels to visually explore a dataset of factors that contribute to the level of happiness in different countries and regions.

## Data

The visualizations are based on the dataset of 166 countries in three years (2015, 2016, 2017)^1. The data is from [Kaggle - The World Happiness Report](https://www.kaggle.com/unsdsn/world-happiness). 

[The World Happiness Report](http://worldhappiness.report) is a landmark survey of the state of global happiness. The first report was published in 2012, and the rest followed in 2013, 2015, 2016 and 2017. The happiness scores and rankings in the data set are from the Gallup World Poll. Each country in the data sets has 11 associated variables^2 that describe the geographical position (Region), happiness rank and score^3 (Happiness.Rank, Happiness.Score) and various other scores (Economy^4, Family, Health^5, Freedom, Government.Corruption, Generosity) that estimate the extent to which each of six factors contribute to making life evaluations higher in each country than they are in Dystopia, a hypothetical country that has values equal to the world’s lowest national averages for each of the six factors. These other scores do not have an impact on the total happiness score reported for each country, but they can explain why some countries rank higher than others.

---

^1 Countries included in the datasets for different years differ slightly. 2015 data has 158 countries, 2016 has 157 countries, and 2017 has 155 countries. In total, there are 166 unique countries included in the three data sets as a whole.

^2 Excluding the "Country" column which indicates the name of the country.

^3 The scores are based on answers to the main life evaluation question asked in the Gallup World Poll. This question, known as the Cantril ladder, asks respondents to think of a ladder with the best possible life for them being a 10 and the worst possible life being a 0 and to rate their own current lives on that scale. The scores are from nationally representative samples for the years 2013-2016 and use the Gallup weights to make the estimates representative.

^4 GDP per capita

^5 Life expectancy

