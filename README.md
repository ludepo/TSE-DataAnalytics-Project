# TSE-DataAnalytics-Project
This repository contains the code written for a topic modelling project for the class "Data Analytics" during my masters program at the Toulouse School of Economics. The project was done in cooperation with Nikita Marini, Andrew Boomer and Jacob Pichelmann.

## Overview
Statistical modelling of unstructured data offers a great opportunity to retrieve information from vast collections of written text by assigning a set of differing topics to these texts. In the process, however, many choices regarding the form and extend of the text pre-processing have to be undertaken which alter the outcome considerably. In this project we demonstrate the effect of different choices a researcher has to take when conducting topic modelling like the removal of numbers, punctuation and stop words; stemming and lemmatizing; accounting for n-grams or keeping only nouns and names that carry most information. We assess the different choices qualitatively by considering the interpretability and presumed accuracy of topics retreived from all NYT articles between 1981 and 2020 concerned with world affairs. 

## Structure
The 'CODE' folder contains two scripts: 
* _Scraper:_ this script was used to fetch the news articles from the NYT archive API
* _TopicModelling:_ this script contains the analysis presented in the report
