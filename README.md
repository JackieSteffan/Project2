## This Repository

This respository contains files that predict the number of shares for an Online News Popularity Data Set. It is broken up by day that the articles were published.

 [Monday is available here](Monday.md)  
 [Tuesday is available here](Tuesdday.md)  
 [Wednesday is available here](Wednesday.md)  
 [Thursday is available here](Thursday.md)  
 [Friday is available here](Friday.md)  
 [Saturday is available here](Saturday.md)  
 [Sunday is available here](Sunday.md)  

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

