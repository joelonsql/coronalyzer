library(shinydashboard)
library(tidyverse)
library(drc)
library(lubridate)
library(scales) 

forecast_days <- 14

confirmed_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>%
    rename(province = "Province/State",
           country = "Country/Region") %>%
    pivot_longer(-c(province,country,Lat,Long), names_to = "date", values_to = "cases") %>%
    mutate(date = mdy(date)) %>%
    group_by(country, date) %>%
    summarise(cases = sum(cases)) %>%
    ungroup()

deaths_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
    rename(province = "Province/State",
           country = "Country/Region") %>%
    pivot_longer(-c(province,country,Lat,Long), names_to = "date", values_to = "cases") %>%
    mutate(date = mdy(date)) %>%
    group_by(country, date) %>%
    summarise(cases = sum(cases)) %>%
    ungroup()

# Additional data for Sweden added manually from Folkhälsomyndighet's two sources:

# FHM Excel:
# https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data
fhm <- data.frame(
    country     = "Sweden FHM",
    cases      = cumsum(c(1,0,1,1,2,2,1,6,7,10,7,12,11,20,25,29,32,35,38,44,45,51,67,79,68,81,90,78,102,76,75,74,79,63,60,57,63,30,19,17,2+20))
)
fhm$date <- as.Date("2020-03-10") + 1:length(fhm$cases)
deaths_global <- rbind(fhm, deaths_global)

countries <- unique(deaths_global$country)
populations <- read_csv("populations.csv", col_names=c("country","population"))

ui <- dashboardPage(
    dashboardHeader(title = "Coronalyzer"),
    dashboardSidebar(
        selectInput("country", "Country:", countries, selected = "Sweden FHM"),
        sliderInput("date",
                    "Date:",
                    min = as.Date("2020-01-22"),
                    max = Sys.Date(),
                    value = as.Date("2020-04-07"),
                    animate = animationOptions(interval=1000)
                    ),
        radioButtons("yaxis","Y-axis:", c("Deaths","Confirmed cases")),
        checkboxInput("perCapita","Per capita"),
        radioButtons("scale","Scale:", c("Linear"="lin","Logarithmic"="log")),
        actionButton("lockScales","Lock scales", icon = icon("lock"))
    ),
    dashboardBody(
        fluidRow(
            box(
                plotOutput("graphCurve")
            ),
            box(
                plotOutput("graphRecentTotalCases")
            ),
            box(
                plotOutput("graphRecentNewCases")
            ),
            infoBoxOutput("maxCasesBox"),
            infoBoxOutput("inflectionPointBox"),
            box(
                verbatimTextOutput("modelSummaryBox")
            )
        ),
        hr(),
        print("Author: Joel Jakobsson <joel@compiler.org>")
    )
    
)

setVal <- function(val, key, value) {
    df <- val()
    if (is.null(df)) {
        df <- data.frame()
    }
    df[1,key] <- value
    val(df)
}

getVal <- function(val, key) {
    df <- val()
    df[1,key]
}

server <- function(input, output, session) {
    shinyOptions(cache = memoryCache(max_size = 100e6))
    infoBoxes <- reactiveVal()
    graphDataVal <- reactiveVal()
    graphCurveX <- reactiveVal()
    graphCurveY <- reactiveVal()

    output$graphCurve <- renderCachedPlot({

        input_data <- NA
        if (input$yaxis == "Deaths") {
            input_data <- deaths_global
        } else {
            input_data <- confirmed_global
        }

        data <- input_data %>%
            filter(country == input$country, cases > 0)

        if (input$perCapita) {
            data$cases <- data$cases / (filter(populations, country == input$country))$population
        }
        
        first_case <- as.Date(min(data$date))
        data$day <- as.integer(data$date - first_case + 1)

        updateSliderInput(session, "date", min = min(data$date))
        updateSliderInput(session, "date", max = max(data$date))

        base_model <- drm(cases ~ day, data = data, fct = LL.4(fixed=c(NA,0,NA,NA)))

        model_data <- filter(data, date <= input$date)
        
        model <- drm(cases ~ day, data = model_data, fct = LL.4(fixed=c(NA,0,NA,NA)))
        model_summary <- paste(capture.output(summary(model)),collapse="\n")
        setVal(infoBoxes, paste("summaryVal", input$yaxis, input$country, input$date, input$perCapita, input$scale), model_summary)
        steepness <- model$coefficients["b:(Intercept)"]
        deceased <- model$coefficients["d:(Intercept)"]
        inflection <- model$coefficients["e:(Intercept)"]
        inflection_date <- first_case + as.integer(inflection) - 1
        setVal(infoBoxes, paste("inflectionPoint", input$yaxis, input$country, input$date, input$perCapita, input$scale), inflection_date)
        end_day <- max(as.integer(2*inflection), max(model_data$day) + forecast_days)
        data$type <- "History"
        fits <- expand.grid(country=input$country,date=NA,day=seq(1,end_day),type="Forecast")
        # Formula is: round(deceased - deceased/(1 + (day/inflection)^(-steepness)))
        pm <- predict(model, newdata=fits, interval="confidence", level=0.68)
        pm2 <- predict(model, newdata=fits, interval="confidence", level=0.95)
        if (input$perCapita) {
            setVal(infoBoxes,paste("maxCases", input$yaxis, input$country, input$date, input$perCapita, input$scale),(paste0(round(deceased*100,4),"%")))
            fits$cases <- pm[,1]
        } else {
            setVal(infoBoxes,paste("maxCases", input$yaxis, input$country, input$date, input$perCapita, input$scale),round(deceased))
            fits$cases <- round(pm[,1])
        }
        cur_max <- max(model_data$cases)
        fits$casesmin <- if_else(pm[,2] < cur_max, cur_max, pm[,2])
        fits$casesmax <- if_else(pm[,3] < cur_max, cur_max, pm[,3])
        fits$casesmin2 <- if_else(pm2[,2] < cur_max, cur_max, pm2[,2])
        fits$casesmax2 <- if_else(pm2[,3] < cur_max, cur_max, pm2[,3])
        data$casesmin <- NA
        data$casesmax <- NA
        data$casesmin2 <- NA
        data$casesmax2 <- NA
        data <- rbind(data, fits)
        # Convert day from integer to date
        data$date <- first_case + data$day - 1

        graphDataVal(data)

        data <- data %>%
            filter(date > input$date | type == "History") %>%
            mutate(type = if_else(date > input$date & type == "History", "Pending", type))
                
        plot <- ggplot(data, aes(x=date)) +
            geom_point(aes(y=cases, color=type, alpha=0.8)) +
            geom_ribbon(data=filter(data,type=="Forecast"),aes(ymin=casesmin, ymax=casesmax, fill="68%"), alpha=0.1) +
            geom_ribbon(data=filter(data,type=="Forecast"),aes(ymin=casesmin2, ymax=casesmax2, fill="95%"), alpha=0.2) +
            guides(alpha = FALSE) +
            labs(x = "Date", y = input$yaxis, fill = "Confidence interval") +
            theme_minimal() +
            geom_vline(aes(xintercept = inflection_date, color="Point of inflection")) +
            ggtitle(paste0("COVID-19 - ", input$country, " - Input <= ", input$date))

        labels_f <- NULL
        if (input$perCapita) {
            labels_f <- scales::percent
        } else {
            labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
        }
        
        if (input$scale == "log") {
            plot <- plot + scale_y_log10(labels = labels_f)
        } else {
            plot <- plot + scale_y_continuous(labels = labels_f)
        }

        if (input$lockScales %% 2 == 0) {
            updateActionButton(session, "lockScales", label = "Lock scales", icon = icon("lock"))
            # unlocked
            gpb <- ggplot_build(plot)
            graphCurveX(c(
                as.Date(gpb$layout$panel_scales_x[[1]]$range$range[1],origin="1970-01-01"),
                as.Date(gpb$layout$panel_scales_x[[1]]$range$range[2],origin="1970-01-01")
            ))
            graphCurveY(c(
                gpb$layout$panel_scales_y[[1]]$range$range[1],
                gpb$layout$panel_scales_y[[1]]$range$range[2]
            ))
        } else {
            plot <- plot + coord_cartesian(ylim = graphCurveY(), xlim =graphCurveX()) 
            updateActionButton(session, "lockScales", label = "Unlock scales", icon = icon("lock-open"))
        }

        print(plot)        


    },
    cacheKeyExpr = {list(input$yaxis, input$country, input$date, input$perCapita, input$scale, input$lockScales)},
    sizePolicy = sizeGrowthRatio(width = 1416, height = 640, growthRate = 1.1)
    )

    output$graphRecentTotalCases <- renderCachedPlot({
        
        data <- graphDataVal()

        plot <- ggplot(data %>%
                        mutate(type = if_else(date > input$date & type == "History", "Pending", type)) %>%
                        subset(date > input$date | type == "History") %>%
                        subset(date >= (input$date-forecast_days) & date <= (input$date+forecast_days)), aes(x=date)) +
            geom_col(aes(y=cases, fill=type), position = position_dodge()) +
            theme_minimal() +
            xlab("Date") +
            ylab(input$yaxis) +
            ggtitle(paste0("COVID-19 - Total - Forecast - ", input$country))

        labels_f <- NULL
        if (input$perCapita) {
            labels_f <- scales::percent
        } else {
            labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
            plot <- plot + geom_text(aes(y=cases, label = cases), show.legend = FALSE, check_overlap = TRUE)
        }
        
        if (input$scale == "log") {
            plot <- plot + scale_y_log10(labels = labels_f)
        } else {
            plot <- plot + scale_y_continuous(labels = labels_f)
        }
        
        print(plot)
        
                
    },
    cacheKeyExpr = {list(input$yaxis, input$country, input$date, input$perCapita, input$scale)},
    sizePolicy = sizeGrowthRatio(width = 1416, height = 640, growthRate = 1.1)
    )
    
    output$graphRecentNewCases <- renderCachedPlot({

        data <- graphDataVal()
        
        plot <- ggplot(data %>%
                        group_by(type) %>%
                        mutate(new_cases = c(0,diff(cases))) %>%
                        ungroup() %>%
                        mutate(type = if_else(date > input$date & type == "History", "Pending", type)) %>%
                        subset(date > input$date | type == "History") %>%
                        subset(date >= (input$date-forecast_days) & date <= (input$date+forecast_days)), aes(x=date)) +
            geom_col(aes(y=new_cases, fill=type), position = position_dodge()) +
            theme_minimal() +
            xlab("Date") +
            ylab(input$yaxis) +
            ggtitle(paste0("COVID-19 - New - Forecast - ", input$country))

        labels_f <- NULL
        if (input$perCapita) {
            labels_f <- scales::percent
        } else {
            labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
            plot <- plot + geom_text(aes(y=new_cases, label = new_cases), show.legend = FALSE, check_overlap = TRUE)
        }
        
        if (input$scale == "log") {
            plot <- plot + scale_y_log10(labels = labels_f)
        } else {
            plot <- plot + scale_y_continuous(labels = labels_f)
        }

        print(plot)
        
    },
    cacheKeyExpr = {list(input$yaxis, input$country, input$date, input$perCapita, input$scale)},
    sizePolicy = sizeGrowthRatio(width = 1416, height = 640, growthRate = 1.1)
    )

    output$maxCasesBox <- renderInfoBox({
        infoBox(
            "FINAL CASES PREDICTED", getVal(infoBoxes, paste("maxCases", input$yaxis, input$country, input$date, input$perCapita, input$scale)), icon=icon("skull"),
            color = "black"
        )
    })

    output$inflectionPointBox <- renderInfoBox({
        infoBox(
            "INFLECTION POINT", getVal(infoBoxes, paste("inflectionPoint", input$yaxis, input$country, input$date, input$perCapita, input$scale)), icon=icon("calendar"),
            color = "black"
        )
    })
    
    output$modelSummaryBox <- renderPrint({
        cat(getVal(infoBoxes, paste("summaryVal", input$yaxis, input$country, input$date, input$perCapita, input$scale)))
    })
}

shinyApp(ui = ui, server = server)
