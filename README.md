# IJC437 Project: Predicting Song Popularity: A Statistical Analysis of the Impact of Acoustic and Metadata Features on Modern Music:

### Overview:
In 2023, music labels spent a record high of $8.1 billion US on artist procurement and development, with a new talent failure rate of 80-90% (Rechardt, 2025). Furthermore, from 2014 to 2020, only 1% of artists occupied four-fifths of all streams in the United Kingdom (Hesmondhalgh et al., 2021). As discussed above, the stakes for both producers and artists are incredibly high. It is paramount that the factors that contribute to a song’s commercial success are identified. Therefore, the aim of this study is to determine the key drivers of song popularity for modern music. The MusicOSet (Silva et al., 2019b) was filtered for the years 2000 - 2018 and then logistic regression and random forest models were run using this data. Tempo, energy, danceability, loudness, duration, acousticness, liveness, valence, speechiness, and instrumentalness were the independent acoustic features examined. Song explicitness, song type, and artist type were the independent metadata features examined. *is_pop* (song popularity) was the dependent variable. 

### Research Questions:

1.	what are the primary acoustic features that influence song popularity, and how well do acoustic features overall predict the song popularity of modern music?

2.	what are the primary metadata features that influence song popularity, and how well do metadata features overall predict the song popularity of modern music?

### Findings:

1.	What are the primary acoustic features that influence song popularity, and how well do acoustic features overall predict the song popularity of modern music?

Answer: Duration, valence, and energy (order varies depending on the model) were found to be the primary acoustic features that drive popular modern music. Additionally, the low AUC scores (.56 and .58), poor sensitivity (.023 and .11), and a very low Nagelkerke R2(.027) suggest that hit song research should move away from analysing acoustic features to other, more impactful variables that predict song popularity. 

2.	What are the primary metadata features that influence song popularity, and how well do metadata features overall predict the song popularity of modern music?

Answer: although artist type (the logistic regression identified singers in particular) was found to be the primary metadata feature, the low AUC (.53), zero sensitivity, and a very low Nagelkerke R2(.01) suggest that music labels should look at other variables besides artist type, song type, as well as song explicitness (excluded from stepwise logistic regression) when trying to produce their next hit. 

### Limitations and Weaknesses

* 1. Most hit song science research, including the MusicOSet, is based on music from the United States (Silva et al., 2019), suggesting a lack of cultural diversity (Seufitelli et al., 2023).
* 2. Apple Music had 100 million songs on its platform in 2022 (Apple, 2022), so 5,791 songs over an 18-year span (2000 – 2018) appears to be a small sample size.
* 3. 12% of the data was missing for RQ2, due to missing artist types, leading to bias and reduced model performance.

 ### Assumptions 

* 1. 2000 – 2018 is a big span of time for modern music to fall under, yet any smaller would lead to poor model performance.
  
* 2. All the songs in this dataset are on the charts, meaning that they are all popular, but the derived variable *is_pop* was created to reduce this bias.

### Future Work

* 1. The “underexplored” impact of lyrics on song popularity (Choudhary et al., 2025).

* 2. Exploring genres and their effect on song popularity.

 In conclusion, hit song science should move away from studying audio features and focus more on the roles that genres and lyrics play in predicting song popularity.



