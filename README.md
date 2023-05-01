# Religious and Ethnic Affinity Dataset v. 0.1

Welcome to the Religious and Ethnic Affinity Dataset! This dataset aims to provide a measure of the connection between countries based on the presence of common religious and ethnic groups. Both religious and ethnic affinity scores have been calculated using the following formula

***
$\varTheta_{x, y}$ is set of social identity groups present in any two countries $x$ and $y$, both; $\theta_{i}$ is a particular identity group:
```math
    \varTheta_{x, y} =  \bigl\{ \theta_{a}, \theta_{b}, \theta_{c}, \ldots , \theta_{n} \bigr\}
```

$\gamma^{\omega}_ {\theta_{i}}$ is the population share of those belonging to social identity group $\theta_{i}$ in country $\omega$, and $\varGamma_{x,y}$ is social affinity score of countries $x$ and $y$:

```math
\varGamma_{x,y} = \sum\limits_{i = 1}^{n} \sqrt{\gamma^{x}_{\theta_{i}}\times\gamma^{y}_{\theta_{i}}}
```
***
[The Cline Center's Composition of Religious and Ethnic Groups Dataset](https://clinecenter.illinois.edu/project/Religious-Ethnic-Identity/composition-religious-and-ethnic-groups-creg-project) was utilized for obtaining country-level religious and ethnic group data. The dataset is formatted in country-dyad-year, covering the period from 1945 to 2013. Religious affinity data is available for all countries during this timeframe. For ethnic affinity, data is unavailable for France, Cameroon, Zambia, India, and Papua New Guinea. Dyads involving any of these countries have been excluded.



This project was initiated as part of the **PS549: Conceptualization and Measurement** course, taught by Prof. Livny at University of Illinois Urbana-Champaign Political Science Department during the Spring 2023 semester.
