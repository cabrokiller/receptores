library(plotly)
library(tydiverse)


mtcars %>%
    mutate(mpg_na = ifelse(row_number() %in% c(1,5,15),NA,mpg)) %>%
    plot_ly() %>%
        add_trace(
            type = "scatter",
            mode = "markers",
            x = ~wt,
            y = ~qsec,
            size = ~replace(mpg_na, is.na(mpg_na),10),
            color = ~mpg_na,
            symbol = ~factor(cyl),
            text = ~mpg_na
            )
