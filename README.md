## This Repository

This respository contains files that predict the number of shares for an Online News Popularity Data Set. It is broken up by day that the articles were published.

 [Monday is available here](Monday.html)  
 [Tuesday is available here](Tuesdday.html)  
 [Wednesday is available here](Wednesday.html)  
 [Thursday is available here](Thursday.html)  
 [Friday is available here](Friday.html)  
 [Saturday is available here](Saturday.html)  
 [Sunday is available here](Sunday.html)  

### Required Packages

`tidyverse`, `corrplot`, `ggplot2`, `GGally`, `tree`, `caret`, `gbm`, `rpart`, `rmarkdown`  

### Automation Code

```markdown
daysofWeek <- unique(popData$weekday)
#create filenames
output_file <- paste0(daysofWeek, ".md")
#create a list for each team with just the team name parameter
params = lapply(daysofWeek, FUN = function(x){list(weekday = x)})

#put into a data frame 
reports <- tibble(output_file, params)
#need to use x[[1]] to get at elements since tibble doesn't simplify
apply(reports, MARGIN = 1, 
      FUN = function(x){
        render(input = "project2.Rmd", output_file = x[[1]], params = x[[2]])
      })
```

