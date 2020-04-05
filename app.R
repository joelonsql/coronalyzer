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

# FHM dashboard:
# https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa
fhm <- data.frame(
    country     = "Sweden FHM Arcgis",
    cases      = c(1,1,1,2,3,7,8,10,12,16,20,23,33,36,42,66,92,102,110,146,180,239,282,333,373,401)
)
fhm$date <- as.Date("2020-03-10") + 1:length(fhm$cases)
deaths_global <- rbind(fhm, deaths_global)

# FHM Excel:
# https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data
fhm <- data.frame(
    country     = "Sweden FHM Excel",
    cases      = cumsum(c(1,0,1,1,2,2,1,6,7,9,8,11,9,16,24,27,32,29,29,30,36,31,43,22,6,1+17))
)
fhm$date <- as.Date("2020-03-10") + 1:length(fhm$cases)
deaths_global <- rbind(fhm, deaths_global)

countries <- unique(deaths_global$country)
populations <- read_csv("populations.csv", col_names=c("country","population"))

ui <- dashboardPage(
    dashboardHeader(title = "Coronalyzer"),
    dashboardSidebar(
        selectInput("country", "Country:", countries, selected = "Sweden FHM Excel"),
        sliderInput("date",
                    "Date:",
                    min = as.Date("2020-01-22"),
                    max = Sys.Date(),
                    value = Sys.Date()-6),
        radioButtons("yaxis","Y-axis:", c("Deaths","Confirmed cases")),
        checkboxInput("per_capita","Per capita"),
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
            infoBoxOutput("inflectionPointBox"),
            box(
                verbatimTextOutput("modelSummaryBox")
            )
        ),
        hr(),
        print("Author: Joel Jakobsson <joel@compiler.org>")
    )
    
)

server <- function(input, output, session) {
    maxCases <- reactiveVal()
    summaryVal <- reactiveVal()
    graphDataVal <- reactiveVal()
    inflectionPoint <- reactiveVal()
    
    output$graph <- renderPlot({

        input_data <- NA
        if (input$yaxis == "Deaths") {
            input_data <- deaths_global
        } else {
            input_data <- confirmed_global
        }

        data <- input_data %>%
            filter(country == input$country, cases > 0)

        if (input$per_capita) {
            data$cases <- data$cases / (filter(populations, country == input$country))$population
        }
        
        first_case <- as.Date(min(data$date))
        data$day <- as.integer(data$date - first_case + 1)

        updateSliderInput(session, "date", min = min(data$date))
        updateSliderInput(session, "date", max = max(data$date))

        base_model <- drm(cases ~ day, data = data, fct = LL.4(fixed=c(NA,0,NA,NA)))

        model_data <- filter(data, date <= input$date)
        
        model <- drm(cases ~ day, data = model_data, fct = LL.4(fixed=c(NA,0,NA,NA)))
        model_summary <- summary(model)
        summaryVal(model_summary)
        steepness <- model$coefficients["b:(Intercept)"]
        deceased <- model$coefficients["d:(Intercept)"]
#        y_limits <- c(1,round(max(deceased, base_model$coefficients["d:(Intercept)"])))
        inflection <- model$coefficients["e:(Intercept)"]
#        x_limits <- c(first_case, max(
#            first_case+as.integer(inflection)*2,
#            Sys.Date()+forecast_days
#        ))
        inflection_date <- first_case + as.integer(inflection) - 1
        inflectionPoint(inflection_date)
        end_day <- max(as.integer(2*inflection), max(model_data$day) + forecast_days)
        data$type <- "History"
        fits <- expand.grid(country=input$country,date=NA,day=seq(1,end_day),type="Forecast")
        # Formula is: round(deceased - deceased/(1 + (day/inflection)^(-steepness)))
        pm <- predict(model, newdata=fits, interval="confidence", level=0.68)
        pm2 <- predict(model, newdata=fits, interval="confidence", level=0.95)
        if (input$per_capita) {
            maxCases(paste0(round(deceased*100,4),"%"))
            fits$cases <- pm[,1]
        } else {
            maxCases(round(deceased))
            fits$cases <- round(pm[,1])
        }
        cur_max <- max(model_data$cases)
        fits$casesmin <- if_else(pm[,2] < cur_max, cur_max, pm[,2])
        fits$casesmax <- pm[,3]
        fits$casesmin2 <- if_else(pm2[,2] < cur_max, cur_max, pm2[,2])
        fits$casesmax2 <- pm2[,3]
        data$casesmin <- NA
        data$casesmax <- NA
        data$casesmin2 <- NA
        data$casesmax2 <- NA
        data <- rbind(data, fits)
        # Convert day from integer to date
        data$date <- first_case + data$day - 1

        graphDataVal(data) # %>% filter(date <= input$date | type == "Forecast"))

        data <- data %>%
            filter(date > input$date | type == "History") %>%
            mutate(type = if_else(date > input$date & type == "History", "Pending", type))
                
        plot <- ggplot(data, aes(x=date)) +
            geom_point(aes(y=cases, color=type, alpha=0.8)) +
            geom_ribbon(data=filter(data,type=="Forecast"),aes(ymin=casesmin, ymax=casesmax, fill="68%"), alpha=0.1) +
            geom_ribbon(data=filter(data,type=="Forecast"),aes(ymin=casesmin2, ymax=casesmax2, fill="95%"), alpha=0.2) +
            guides(alpha = FALSE) +
            labs(x = "Datum", y = input$yaxis, fill = "Confidence interval") +
            theme_minimal() +
#            scale_x_date(limits=x_limits) +
            geom_vline(aes(xintercept = inflection_date, color="Point of inflection")) +
            ggtitle(paste0("COVID-19 - Total - ", input$country))

        labels_f <- NULL
        if (input$per_capita) {
            labels_f <- scales::percent
        } else {
            labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
        }
        
        if (input$scale == "log") {
#            print(plot + scale_y_log10(limits=y_limits, labels = scales::number_format(accuracy = 1, decimal.mark = ',')))
            print(plot + scale_y_log10(labels = labels_f))
        } else {
#            print(plot + scale_y_continuous(limits=y_limits, labels = scales::number_format(accuracy = 1, decimal.mark = ',')))
            print(plot + scale_y_continuous(labels = labels_f))
        }
                    
    })

    output$graphRecentTotalCases <- renderPlot({
        
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
        if (input$per_capita) {
            labels_f <- scales::percent
        } else {
            labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
            plot <- plot + geom_text(aes(y=cases, label = cases), show.legend = FALSE, check_overlap = TRUE)
        }
        
        if (input$scale == "log") {
            print(plot + scale_y_log10(labels = labels_f))
        } else {
            print(plot + scale_y_continuous(labels = labels_f))
        }
        
                
    })
    
    output$graphRecentNewCases <- renderPlot({

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
        if (input$per_capita) {
            labels_f <- scales::percent
        } else {
            labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
            plot <- plot + geom_text(aes(y=new_cases, label = new_cases), show.legend = FALSE, check_overlap = TRUE)
        }
        
        if (input$scale == "log") {
            print(plot + scale_y_log10(labels = labels_f))
        } else {
            print(plot + scale_y_continuous(labels = labels_f))
        }
        
    })

    output$maxCasesBox <- renderInfoBox({
        infoBox(
            "FINAL CASES PREDICTED", maxCases(), icon=icon("skull"),
            color = "black"
        )
    })

    output$inflectionPointBox <- renderInfoBox({
        infoBox(
            "INFLECTION POINT", inflectionPoint(), icon=icon("calendar"),
            color = "black"
        )
    })
    
    output$modelSummaryBox <- renderPrint({
        print(summaryVal())
    })
}

shinyApp(ui = ui, server = server)
