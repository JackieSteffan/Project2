daysofWeek <- unique(popData$weekday)
#create filenames
output_file <- paste0(daysofWeek, ".html")
#create a list for each team with just the team name parameter
params = lapply(daysofWeek, FUN = function(x){list(weekday = x)})

#put into a data frame 
reports <- tibble(output_file, params)
#need to use x[[1]] to get at elements since tibble doesn't simplify
apply(reports, MARGIN = 1, 
      FUN = function(x){
        render(input = "project2.Rmd", output_file = x[[1]], params = x[[2]])
      })

#or with pwalk (args are .l, .f, and ...)
#.l is a list of lists, .f is function, formula, or vector
#pwalk(reports, render, input = "project2.Rmd")