#Load the libraries

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(sparkline)

#Set a few color variables to make our table more visually appealing

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

#Download the Austin indicator data set
#Original data set from: https://data.austintexas.gov/City-Government/Imagine-Austin-Indicators/apwj-7zty/data

austinData= fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/Austin/Imagine_Austin_Indicators.csv', data.table=FALSE, header = TRUE, stringsAsFactors = FALSE)
head(austinData)

i1 <- austinData %>%
  filter(`Indicator Name` %in% 
           c('Prevalence of Obesity', 'Prevalence of Tobacco Use', 
             'Prevalence of Cardiovascular Disease', 'Prevalence of Diabetes')) %>%
  select(c(`Indicator Name`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`)) %>%
  mutate (Average = round(rowMeans(
    cbind(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`), na.rm=T),2), 
    `Improvement` = round((`2011`-`2016`)/`2011`*100,2))
i1

formattable(i1)

#1)  First Data Table

formattable(i1, 
            align =c("l","c","c","c","c", "c", "c", "c", "r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) 
            ))

#2) Add the color mapping for all 2011 to 2016.
formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0)
))

#3) Add the color bar to the average column

formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0),
  `Average` = color_bar(customRed)
))

#5) For improvement formatter add icons
# Up and down arrow with greater than comparison from the vignette

improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
                                   x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
)


formattable(i1, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  `2011`= color_tile(customGreen, customGreen0),
  `2012`= color_tile(customGreen, customGreen0),
  `2013`= color_tile(customGreen, customGreen0),
  `2014`= color_tile(customGreen, customGreen0),
  `2015`= color_tile(customGreen, customGreen0),
  `2016`= color_tile(customGreen, customGreen0),
  `Average` = color_bar(customRed),
  `Improvement` = improvement_formatter
))


prevalence <- i1
formattable(prevalence)

formattable(prevalence, align = c("l", rep("r", NCOL(prevalence) - 1)))

prevalence[, "Improvement"] = prevalence[, "Improvement"] / 100
formattable(prevalence, 
            align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                 `Average` = color_bar("#FA614B"), 
                 `Improvement` = percent))

prevalence$` ` = c(4.1, -.3, .5, 1.4)
prevalence$`2012` = apply(prevalence[, 2:7], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "line"))))
names(prevalence)[3] = "  "
new.prevalance = prevalence[, c(1, 2, 3, 7, 10)]                          
out = as.htmlwidget(formattable(new.prevalance,
                                align = c("l",rep("r", NCOL(prevalence) - 1)), 
                                list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                                     " " = formatter("span", 
                                                     style = ~ style(color = ifelse(`2016` > `2011`, "green", "red")),                                    
                                                     ~ icontext(sapply(` `, function(x) if (x < -1.96) "arrow-down" else if (x> 1.96) "arrow-up" else ""))))))                          
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out
