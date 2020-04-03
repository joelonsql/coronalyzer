library(shinydashboard)
library(tidyverse)
library(drc)
library(lubridate)
library(scales) 

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

# FHM dashboard:
# https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa
fhm <- data.frame(
    country     = "Sweden FHM Arcgis",
    cases      = c(1,1,1,2,3,7,8,10,12,16,20,23,33,36,42,66,92,102,110,146,180,239,282)
)
fhm$date <- as.Date("2020-03-10") + 1:length(fhm$cases)
deaths_global <- rbind(fhm, deaths_global)

# FHM Excel:
# https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data
fhm <- data.frame(
    country     = "Sweden FHM Excel",
    cases      = cumsum(c(1,1,2,2,1,6,7,9,8,11,8,16,22,27,31,26,25,26,26,13,5+9))
)
fhm$date <- as.Date("2020-03-12") + 1:length(fhm$cases)
deaths_global <- rbind(fhm, deaths_global)

countries <- unique(deaths_global$country)
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
        radioButtons("yaxis","Y-axis:", c("Deaths","Confirmed cases")),
        radioButtons("scale","Scale:", c("Linear"="lin","Logarithmic"="log"))
    ),
    dashboardBody(
        fluidRow(
            box(
                plotOutput("graph")
            ),
            box(
                plotOutput("graphRecentTotalCases")
            ),
            box(
                plotOutput("graphRecentNewCases")
            ),
            infoBoxOutput("maxCasesBox"),
            box(
                verbatimTextOutput("modelSummaryBox")
            )
        )
    )
)

server <- function(input, output, session) {
    maxCases <- reactiveVal()
    summaryVal <- reactiveVal()
    graphDataVal <- reactiveVal()
    
    output$graph <- renderPlot({

        input_data <- NA
        if (input$yaxis == "Deaths") {
            input_data <- deaths_global
        } else {
            input_data <- confirmed_global
        }

        data <- input_data %>%
            filter(country == input$country, cases > 0)
        first_case <- as.Date(min(data$date))
        data$day <- as.integer(data$date - first_case + 1)
        data$predictions <- NA

        updateSliderInput(session, "date", min = min(data$date))
        updateSliderInput(session, "date", max = max(data$date))

        base_model <- drm(cases ~ day, data = data, fct = LL.4(fixed=c(NA,0,NA,NA)))

        model_data <- filter(data, date <= input$date)
        
        model <- drm(cases ~ day, data = model_data, fct = LL.4(fixed=c(NA,0,NA,NA)))
        model_summary <- summary(model)
        summaryVal(model_summary)
        steepness <- model$coefficients["b:(Intercept)"]
        deceased <- model$coefficients["d:(Intercept)"]
        y_limits <- c(1,round(max(deceased, base_model$coefficients["d:(Intercept)"])))
        inflection <- model$coefficients["e:(Intercept)"]
        x_limits <- c(first_case, max(
            first_case+as.integer(inflection)*2,
            Sys.Date()+7
        ))
        inflection_date <- first_case + as.integer(inflection) - 1
        maxCases(round(deceased))
        data$model <- NA
        predict_day <- max(model_data$day) + 1
        end_day <- max(as.integer(2*inflection), max(model_data$day) + 7)
        data$type <- "History"
        data <- data %>% add_row(
            date = NA,
            day = predict_day:end_day,
            cases = sapply(predict_day:end_day, function(day) {
                round(predict(model,data.frame(day=day))) # The formula is: round(deceased - deceased/(1 + (day/inflection)^(-steepness)))
            }),
            type = "Forecast"
        )
        # Convert day from integer to date
        data$date <- first_case + data$day - 1
        predict_day <- first_case + predict_day - 1
        predict_total <- filter(data,date==predict_day & type == "Forecast")$cases
        predict_new <- predict_total - filter(data,date==predict_day-1 & type == "Forecast")$cases

        graphDataVal(data %>% filter(date <= input$date | type == "Forecast"))
        
        plot <- ggplot(data, aes(x=date)) +
            geom_point(aes(y=cases, color=type, alpha=0.8)) +
            guides(alpha = FALSE) +
            theme_minimal() +
            scale_x_date(limits=x_limits) +
            geom_vline(aes(xintercept = inflection_date, color="Point of inflection")) +
            xlab("Datum") +
            ylab(input$yaxis) +
            ggtitle(paste0("COVID-19 - Total Cases - ", input$country),
                    subtitle = paste0("Forecast ", predict_day, " : ", predict_total, " total, ", predict_new, " new")
            )

        if (input$scale == "log") {
            print(plot + scale_y_log10(limits=y_limits, labels = comma))
        } else {
            print(plot + scale_y_continuous(limits=y_limits, labels = comma))
        }
                    
    })

    output$graphRecentTotalCases <- renderPlot({
        
        data <- graphDataVal()
        
        plot <- ggplot(data %>%
                   subset(date >= (input$date-7) & date <= (input$date+7)), aes(x=date)) +
            geom_col(aes(y=cases, fill=type)) +
            geom_text(aes(y=cases, label = cases),
                      show.legend = FALSE, check_overlap = TRUE) +
            theme_minimal() +
            xlab("Date") +
            ylab(input$yaxis) +
            ggtitle(paste0("COVID-19 - Total - 7 day forecast - ", input$country))

        if (input$scale == "log") {
            print(plot + scale_y_log10(labels = comma))
        } else {
            print(plot + scale_y_continuous(labels = comma))
        }
        
                
    })
    
    output$graphRecentNewCases <- renderPlot({

        data <- graphDataVal()
        
        plot <- ggplot(data %>%
                   mutate(new_cases = c(0,diff(cases))) %>%
                   subset(date >= (input$date-7) & date <= (input$date+7)), aes(x=date)) +
            geom_col(aes(y=new_cases, fill=type)) +
            geom_text(aes(y=new_cases, label = new_cases),
                      show.legend = FALSE, check_overlap = TRUE) +
            theme_minimal() +
            xlab("Date") +
            ylab(input$yaxis) +
            ggtitle(paste0("COVID-19 - New - 7 day forecast - ", input$country))

        if (input$scale == "log") {
            print(plot + scale_y_log10(labels = comma))
        } else {
            print(plot + scale_y_continuous(labels = comma))
        }
        
    })

    output$maxCasesBox <- renderInfoBox({
        infoBox(
            "FINAL CASES PREDICTED", maxCases(),
            color = "purple"
        )
    })

    output$modelSummaryBox <- renderPrint({
        print(summaryVal())
    })
}

shinyApp(ui = ui, server = server)
