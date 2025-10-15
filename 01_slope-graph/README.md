# 📈 Slope Graph — Life Expectancy by Country

**Central America, 1982 → 2007**

![Slope Graph](slope_graph.png)

---

### 🧮 Description
This graphic shows the evolution of **life expectancy** in Central American countries between 1982 and 2007, using data from the *Gapminder* dataset.

It shows how to build a minimalist **slope graph** in R to visualize relative changes over time.

---

### 🧰 Code Snippet
```r
# Load data and packages
library(gapminder)
library(dplyr)
library(ggplot2)
library(CGPfunctions)

df <- gapminder %>%
  filter(year %in% c(1982, 1987, 1992, 1997, 2002, 2007) &
         country %in% c("Panama", "Costa Rica",
                        "Nicaragua", "Honduras",
                        "El Salvador", "Guatemala",
                        "Belize")) %>%
  mutate(year = factor(year),
         lifeExp = round(lifeExp))

newggslopegraph(df, year, lifeExp, country) +
  labs(title = "Life Expectancy by Country",
       subtitle = "Central America, 1982 → 2007",
       caption = "Source: Gapminder  |  Graphic: Diego M. Macall")
