library("rcompanion")
library("car")
library("IDPmisc")
library("dplyr")
# Number of suicides by generation by country, with country being the repeated factor

## Check Assumptions

### Normality

plotNormalHistogram(suicide$`suicides/100k pop`)
spec(suicide)
suicide$suicides.100k.popSQRT <- sqrt(suicide$`suicides/100k pop`)
plotNormalHistogram(suicide$'suicides.100k.popSQRT') 
suicide$suicides.100k.popLOG <- log(suicide$`suicides/100k pop`)
suicide4 <- NaRV.omit(suicide)
plotNormalHistogram(suicide4$'suicides.100k.popLOG')


#### Use the log

### Homogeneity of Variance

leveneTest(suicides.100k.popLOG ~ generation, data=suicide4)


#### This failed the assumption, but proceed anyway for learning purposes

### Sample size -you have more than enough data

## Run the analysis

RManova1 <- aov(suicides.100k.popLOG~(generation*year)+Error(ï..country/(year)), suicide4)
summary(RManova1)


### Looks like there is a generational effect to suicide, and an interaction to how the year has affected the generation

## Post hocs

pairwise.t.test(suicide4$suicides.100k.popLOG, suicide4$generation, p.adjust="bonferroni")

### 	Pairwise comparisons using t tests with pooled SD 

## data:  suicide4$suicides.100k.popLOG and suicide4$generation 

## Boomers G.I. Generation Generation X Generation Z Millenials
## G.I. Generation 6.5e-16 -               -            -            -         
##  Generation X    1.2e-13 < 2e-16         -            -            -         
##  Generation Z    < 2e-16 < 2e-16         < 2e-16      -            -         
##  Millenials      < 2e-16 < 2e-16         < 2e-16      < 2e-16      -         
##  Silent          1.2e-05 2.0e-05         < 2e-16      < 2e-16      < 2e-16   

## P value adjustment method: bonferroni 


### Looks like there is a difference in suicide rates among ALL the generations

## Determine Means and Draw Conclusions

suicideMeans <- suicide4 %>% group_by(generation, year) %>% summarize(Mean=mean(`suicides/100k pop`))
