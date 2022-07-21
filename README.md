# R_AgathaChristie

Making Babysteps in R

Main CSV Data taken from https://github.com/JamesJackson1/AgathaChristie. Added columns with Publication Date & Detective. 
Data is mixed. Some Data concerns the entire book and is then the same for all rows of the book, while some Data is specific to the character the row belongs to.

A second Data set was taken from the Books & Editions set from https://www.workwithdata.com/author/agatha-christie-1890, but only used in the last graph, to have more publications available. The difference between the two sets is big.

Goal of this project is to create some fun tables & graphs comparing murderers, weapons, occupations etc. throughout the available Agatha Christie books. 
Maybe a pattern can be found? Maybe Agatha Christie loved making butlers into murderers? Maybe her books became more bloody over the years?
Maybe every third book contains a female murderer that strangels at least 5 people, but only if Hercule Poirot investigates? 
Answers to these highly important questions can soon be found in this R script. Enjoy!



Created for the Course "Datenanalyse mit R" - Sommersemester 2022, taught by Dr. Jürgen Hermes at the University of Cologne


***Some Results so far:***

**Cause of death:**

![Barplot_Cause_of_Death](https://user-images.githubusercontent.com/35702288/180190418-12ef0ec5-690a-4f8d-81a8-840a11307a0d.png)



**Development of killers / killed over publication years:**

![killers_per_year](https://user-images.githubusercontent.com/35702288/180229585-e5ff77a9-e2a3-4543-959c-3d7c52192cee.png)



**Characters in Occupation being killers or not:**

![Barplot_character_Occupation_Nomurder_murder](https://user-images.githubusercontent.com/35702288/180228632-0efaeffe-1574-471a-bcee-16413d9c79a3.png)



**same as bubble plot:**

![bubble_occupation_murderer](https://user-images.githubusercontent.com/35702288/180229132-c0985a6e-21a2-4da8-a530-10172b5107cc.png)

**same as scatter plot:**
![Scatter_murderer_in_occupation](https://user-images.githubusercontent.com/35702288/180229231-9a949431-abe5-46d6-95f6-edf2fd01d286.png)



**Amount killed by each gender in all books:**

![Barplot_killed_victims_per_gender](https://user-images.githubusercontent.com/35702288/180228727-94f7817d-ca4a-420e-9799-65293414f009.png)



**Murders and weapon through the years by franchise (some overlap):**

![bubble_murders_franchise_Year](https://user-images.githubusercontent.com/35702288/180229415-946e300d-d3e2-42ed-96fe-c01054bff0ec.png)



**publications per year (based on 2nd data set):**
![Scatter_publicationDate](https://user-images.githubusercontent.com/35702288/180229743-a264de51-7f52-43e4-a3bf-a8e2a53f1254.png)
