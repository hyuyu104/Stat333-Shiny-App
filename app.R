library(stringr); library(xgboost)
library(tidyverse); library(gridExtra)
library(reshape2); library(car)
library(glmnet); library(lars); library(MASS)

library(shiny)


convert.layer.df = function(yelp.data) {
    word.idx = (1:ncol(yelp.data))[colnames(yelp.data)=="exquisite"]
    ignore.coln = c("Id", "name", "city", "postalCode", "text")
    ignore.idx = (1:ncol(yelp.data))[colnames(yelp.data) %in% ignore.coln]
    layer.df = yelp.data[, 1:word.idx-1]
    if (length(ignore.idx)>0) {layer.df = layer.df[, -ignore.idx]}
    coef.df = read.csv("lasso_coef.csv", row.names = "X"); colnames(coef.df) = "s1"
    c = data.frame(word.idx:ncol(yelp.data)); colnames(c) = "c"
    
    very.positive = rownames(coef.df)[coef.df$s1 >= 0.1]
    c.index = c$c[colnames(yelp.data)[word.idx:length(colnames(yelp.data))] %in% very.positive]
    layer.df$very.positive = apply(yelp.data[, c.index], 1, sum)
    
    some.positive = rownames(coef.df)[(coef.df$s1 < 0.1)&(coef.df$s1 > 0)]
    c.index = c$c[colnames(yelp.data)[word.idx:length(colnames(yelp.data))] %in% some.positive]
    layer.df$some.positive = apply(yelp.data[, c.index], 1, sum)
    
    neutral = rownames(coef.df)[coef.df$s1 == 0]
    c.index = c$c[colnames(yelp.data)[word.idx:length(colnames(yelp.data))] %in% neutral]
    layer.df$neutral = apply(yelp.data[, c.index], 1, sum)
    
    some.negative = rownames(coef.df)[(coef.df$s1 < 0)&(coef.df$s1 >= -0.2)]
    c.index = c$c[colnames(yelp.data)[word.idx:length(colnames(yelp.data))] %in% some.negative]
    layer.df$some.negative = apply(yelp.data[, c.index], 1, sum)
    
    very.negative = rownames(coef.df)[coef.df$s1 < -0.2]
    c.index = c$c[colnames(yelp.data)[word.idx:length(colnames(yelp.data))] %in% very.negative]
    layer.df$very.negative = apply(yelp.data[, c.index], 1, sum)
    
    return (layer.df)
}


single.text.handler = function(yelp.data, text, zipcode) {    
    # Add Word Predictors
    word.idx = (1:ncol(yelp.data))[colnames(yelp.data)=="exquisite"]
    char.idx = (1:ncol(yelp.data))[colnames(yelp.data)=="nChar"]
    all.words = colnames(yelp.data)[word.idx:ncol(yelp.data)]
    text.words = str_split(str_replace_all(tolower(text), "[:punct:]", ""), " ")
    text.df = yelp.data[1, char.idx:ncol(yelp.data)]; text.df[1, ] = 0
    for (word in unlist(text.words)) {
        if (word %in% all.words) {
            idx = (1:ncol(text.df))[colnames(text.df) == word]
            text.df[1, idx] = text.df[1, idx] + 1
        }
    }
    
    # Add Punctuations, nChar, nSentence
    text.df$p. = length(unlist(str_match_all(text, "\\!")))
    text.df$p..1 = length(unlist(str_match_all(text, "\\?")))
    text.df$nChar = nchar(text)
    text.df$nSentence = length(unlist(str_match_all(text, "\\w\\?|\\w\\.|\\w\\!")))
    
    # income, univeristy
    near = c(43201, 43202, 43211, 43210, 43215, 43212, 43221,
             43214, 43224, 43219, 43203, 43205, 43209, 43219,
             43227, 43207, 43215, 43222, 43206, 43223)
    income_df = read.csv("income_dict.csv")
    if (zipcode==43270) {zipcode = 43210}
    text.df$income = income_df$income[income_df$X==zipcode]
    if (zipcode %in% near) {text.df$unviersity = 1}
    
    return(text.df)
}


ui = fluidPage(

    titlePanel("Predict Yelp Ratings Using XGBoost"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("zipcode", "Please select the postal code of the restaurant.",
                        append(c("Postal Code"), sort(append(c("43270"), read.csv("income_dict.csv")$X)))),
            
            textAreaInput("text", "Please enter the yelp comment."),
            
            actionButton("click", "Predict")
        ),
        mainPanel(
            textOutput("boost"), br(),
            strong("Compressed Prediction Data"),
            tableOutput("react"), br(), br(),
            strong("Prediction Result (Star):"),
            textOutput("predicted")
        )
    )
)

server = function(input, output) {
    df = eventReactive(input$click, {
        df = single.text.handler(yelp.data, input$text, as.numeric(input$zipcode))
        layer.pred = convert.layer.df(df)
        output$predicted = renderText(predict(bst, as.matrix(layer.pred)))
        layer.pred
    })
    output$react = renderTable({
        df()
    })
    output$boost = renderText(paste("MSE of XGBoost (35 rounds):", MSE.boost, "\n\n"))
}



yelp.data = read.csv("mini_train.csv")
layer.df = read.csv("layer.csv")

set.seed(100)
random.index = sample(1:nrow(layer.df))
model.sample.index = random.index[1:(nrow(layer.df)*0.8)]
infer.sample.index = random.index[(nrow(layer.df)*0.8+1):length(random.index)]
layer.df.train = layer.df[model.sample.index, ]
layer.df.test = layer.df[infer.sample.index, ]
train = list(data=as.matrix(layer.df.train[, -1]), label=layer.df.train$star)
test = list(data=as.matrix(layer.df.test[, -1]), label=layer.df.test$star)
bst = xgboost(data = train$data, label = train$label, max.depth = 6, 
              eta = 0.7, nthread=2, nrounds = 35)

predicted = predict(bst, test$data)
MSE.boost = sum((predicted-layer.df.test$star)^2)/length(layer.df.test$star)

shinyApp(ui = ui, server = server)
