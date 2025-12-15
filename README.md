# Flight Delay Analysis
## Project Overview
The **aviation industry** significantly influences the social and economic advancement of a nation. However, **airline delays** cause significant losses for the industry, impacting airports, airlines, and passengers.

Punctual flight performance is crucial for **happier customers**, increased profitability, and improved **efficiency and safety**. This project utilizes **data visualization** and analysis to explore distribution features and understand their effect on the occurrence of delays.

## Data Used
The dataset contains information on **539,383 rows** and **9 columns**, detailing whether flights operated by different airlines were delayed. The data was sourced from **Kaggle**.

* **Target Variable:** Delay (0 or 1)
* **Features:** Airline, Flight Number, Airport From, Airport To, DayOfWeek, Time, Length

| id | Airline | Flight | Airport From | Airport To | DayOfWeek | Length | Delay |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| 1 | CO | 269 | SFO | IAH | 3 | 205 | 1 |
| 2 | US | 1558 | PHX | CLT | 3 | 222 | 1 |
| 3 | AA | 2400 | LAX | DFW | 3 | 165 | 1 |
| 4 | AA | 2466 | SFO | DFW | 3 | 195 | 1 |
| 5 | AS | 108 | ANC | SEA | 3 | 202 | 0 |

#### Reason for Study
Worldwide airline delays are a major issue causing enormous losses. Cutting down on flight delays can lessen aviation's **carbon footprint** and benefit the environment. This study aims to analyze factors contributing to flight delays to help create appropriate plans for **smooth operational functioning**.

## Exploratory Data Analysis & Results
We analyzed the distribution of features using **univariate and multivariate plots**. The analysis revealed that approximately **45%** of flights in the dataset are delayed.

| Category | Finding | Metric/Note |
| :--- | :--- | :--- |
| **Most Delayed Airline** | **WN Airlines** | Highest frequency of delays |
| Least Delayed Airline | HA Airlines | Best performance |
| **Worst Days** | **Midweek (Days 3 & 4)** | **17%** delay proportion each |
| Best Day | Day 6 | Only **11%** delay observed |
| Most Popular Route | LAX - SFO | 2,156 combined flights |

*As shown above, **WN Airlines** operated the most delayed flights, and delays were most frequent during **midweek** operations.*

 ## Links  :

  [Application Link](https://mariapault0512.shinyapps.io/ShinyApplication/)

  [Report](https://github.com/intagliated/Analysis-of-Delays-in-Flights-/blob/main/Project_1.pdf)<br><br>
  
  [Code Files](https://github.com/intagliated/Analysis-of-Delays-in-Flights-/blob/main/app.R)<br><br>
  [Youtube](https://youtu.be/TYsCP59EyV0)
