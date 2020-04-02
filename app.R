library(shinydashboard)
library(tidyverse)
library(drc)
library(lubridate)
library(scales) 

country_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>%
    rename(province = "Province/State",
           country = "Country/Region") %>%
    pivot_longer(-c(province,country,Lat,Long), names_to = "date", values_to = "deaths") %>%
    mutate(date = mdy(date)) %>%
    group_by(country, date) %>%
    summarise(deaths = sum(deaths)) %>%
    ungroup() %>%
    filter(country != "Sweden")
# Data for Sweden added manually from Folkhälsomyndigheten's dashboard:
# https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa
fhm <- data.frame(
    country     = "Sweden",
    deaths      = c(1,1,1,2,3,7,8,10,12,16,20,23,33,36,42,66,92,102,110,146,180,239)
)
fhm$date <- as.Date("2020-03-10") + 1:length(fhm$deaths)
country_deaths <- rbind(fhm, country_deaths)
countries <- unique(country_deaths$country)
populations <- read_csv("populations.csv", col_names=c("country","population"))

ui <- dashboardPage(
    dashboardHeader(title = "Coronalyzer"),
    dashboardSidebar(
        selectInput("country", "Country:", countries, selected = "Sweden"),
        sliderInput("date",
                    "Date:",
                    min = as.Date("2020-01-22"),
                    max = Sys.Date(),
                    value = Sys.Date()),
        radioButtons("scale","Scale:", c("Linear"="lin","Logarithmic"="log"))
    ),
    dashboardBody(
        fluidRow(
            box(
                plotOutput("graph")
            ),
            box(
                plotOutput("graphRecentTotalDeaths")
            ),
            box(
                plotOutput("graphRecentNewDeaths")
            ),
            infoBoxOutput("maxDeathsBox"),
            box(
                verbatimTextOutput("modelSummaryBox")
            )
        )
    )
)

server <- function(input, output, session) {
    maxDeaths <- reactiveVal()
    summaryVal <- reactiveVal()
    graphDataVal <- reactiveVal()
    
    output$graph <- renderPlot({
        
        data <- country_deaths %>%
            filter(country == input$country, deaths > 0)
        first_death <- as.Date(min(data$date))
        data$day <- as.integer(data$date - first_death + 1)
        data$predictions <- NA

        updateSliderInput(session, "date", min = min(data$date))
        updateSliderInput(session, "date", max = max(data$date))

        base_model <- drm(deaths ~ day, data = data, fct = LL.4(fixed=c(NA,0,NA,NA)))
        x_limits <- c(first_death, max(
            first_death+as.integer(base_model$coefficients["e:(Intercept)"])*2,
            Sys.Date()+7
        ))

        model_data <- filter(data, date <= input$date)
        
        model <- drm(deaths ~ day, data = model_data, fct = LL.4(fixed=c(NA,0,NA,NA)))
        model_summary <- summary(model)
        summaryVal(model_summary)
        steepness <- model$coefficients["b:(Intercept)"]
        deceased <- model$coefficients["d:(Intercept)"]
        y_limits <- c(1,round(max(deceased, base_model$coefficients["d:(Intercept)"])))
        inflection <- model$coefficients["e:(Intercept)"]
        inflection_date <- first_death + as.integer(inflection) - 1
        maxDeaths(round(deceased))
        data$model <- NA
        predict_day <- max(model_data$day) + 1
        end_day <- max(as.integer(2*inflection), max(model_data$day) + 7)
        data$type <- "History"
        data <- data %>% add_row(
            date = NA,
            day = predict_day:end_day,
            deaths = sapply(predict_day:end_day, function(day) {
                round(deceased - deceased/(1 + (day/inflection)^(-steepness)))
            }),
            type = "Forecast"
        )
        # Convert day from integer to date
        data$date <- first_death + data$day - 1
        predict_day <- first_death + predict_day - 1
        predict_total <- filter(data,date==predict_day & type == "Forecast")$deaths
        predict_new <- predict_total - filter(data,date==predict_day-1 & type == "Forecast")$deaths

        graphDataVal(data %>% filter(date <= input$date | type == "Forecast"))
        
        plot <- ggplot(data, aes(x=date)) +
            geom_point(aes(y=deaths, color=type, alpha=0.8)) +
            guides(alpha = FALSE) +
            theme_minimal() +
            scale_x_date(limits=x_limits) +
            geom_vline(aes(xintercept = inflection_date, color="Point of inflection")) +
            xlab("Datum") +
            ylab("Deaths") +
            ggtitle(paste0("COVID-19 - Total Deaths - ", input$country),
                    subtitle = paste0("Forecast ", predict_day, " : ", predict_total, " total, ", predict_new, " new")
            )

        if (input$scale == "log") {
            print(plot + scale_y_log10(limits=y_limits, labels = comma))
        } else {
            print(plot + scale_y_continuous(limits=y_limits, labels = comma))
        }
                    
    })

    output$graphRecentTotalDeaths <- renderPlot({
        
        data <- graphDataVal()
        
        plot <- ggplot(data %>%
                   subset(date >= (input$date-7) & date <= (input$date+7)), aes(x=date)) +
            geom_col(aes(y=deaths, fill=type)) +
            geom_text(aes(y=deaths, label = deaths),
                      show.legend = FALSE, check_overlap = TRUE) +
            theme_minimal() +
            xlab("Date") +
            ylab("Total Deceased") +
            ggtitle(paste0("COVID-19 - Total Deaths - 7 day forecast - ", input$country))

        if (input$scale == "log") {
            print(plot + scale_y_log10(labels = comma))
        } else {
            print(plot + scale_y_continuous(labels = comma))
        }
        
                
    })
    
    output$graphRecentNewDeaths <- renderPlot({

        data <- graphDataVal()
        
        plot <- ggplot(data %>%
                   mutate(new_deaths = c(0,diff(deaths))) %>%
                   subset(date >= (input$date-7) & date <= (input$date+7)), aes(x=date)) +
            geom_col(aes(y=new_deaths, fill=type)) +
            geom_text(aes(y=new_deaths, label = new_deaths),
                      show.legend = FALSE, check_overlap = TRUE) +
            theme_minimal() +
            xlab("Date") +
            ylab("New Deceased") +
            ggtitle(paste0("COVID-19 - New Deaths - 7 day forecast - ", input$country))

        if (input$scale == "log") {
            print(plot + scale_y_log10(labels = comma))
        } else {
            print(plot + scale_y_continuous(labels = comma))
        }
        
    })

    output$maxDeathsBox <- renderInfoBox({
        infoBox(
            "FINAL DEATHS PREDICTED", maxDeaths(), icon = icon("skull"),
            color = "purple"
        )
    })

    output$modelSummaryBox <- renderPrint({
        print(summaryVal())
    })
}

shinyApp(ui = ui, server = server)
