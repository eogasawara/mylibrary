# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myTSRegression.R")

load_series <- function(name) {
  link <- url(sprintf("https://raw.githubusercontent.com/eogasawara/mylibrary/master/data/time-series/%s.RData", name))
  x <- get(load(link))
  return(x)  
}
x <- load_series("sin")
sahead <- 1
tsize <- 1
swsize <- 10
preproc <- ts_gminmax()
plot(x)


train_test <- function(x, model, sw_size, test_size, steps_ahead) {
  ts <- ts_data(x, sw_size)
  
  samp <- ts_sample(ts, test_size)
  
  io_train <- ts_projection(samp$train)
  
  model <- train(model, x=io_train$input, y=io_train$output)
  
  adjust <- predict(model, io_train$input)
  ev_adjust <- evaluation.tsreg(io_train$output, adjust)
  print(head(ev_adjust$metrics))
  
  io_test <- ts_projection(samp$test)
  
  prediction <- predict(model, x=io_test$input, steps_ahead=steps_ahead)
  output <- as.vector(io_test$output)
  if (steps_ahead > 1)
    output <- output[1:steps_ahead]
  ev_prediction <- evaluation.tsreg(output, prediction)
  print(head(ev_prediction$metrics))
  
  prep <- ""
  if (!is.null(model$preprocess))
    prep <- sprintf("-%s", class(model$preprocess)[1])    
  
  print(sprintf("%s%s %.2f", class(model)[1], prep, 100*ev_prediction$metrics$smape))
  
  yvalues <- c(io_train$output, io_test$output)
  plot(model, y=yvalues, yadj=adjust, ypre=prediction)
  return(model)
}

model <- train_test(x, model=tsreg_arima(), 0, 
           test_size = tsize, steps_ahead = sahead)

# do not set mtry and ntree for hyperparameter optimization
# you can also set a range for them
model <- train_test(x, model=tsreg_rf(preproc, input_size=4, mtry=3, ntree=50), 
                    sw = swsize, test_size = tsize, steps_ahead = sahead)

# do not set decay and set a range for neurons for hyperparameter optimization
# you can also set a range for them
  model <- train_test(x, model=tsreg_mlp(preproc, input_size=4, size=4, decay=0), 
             sw = swsize, test_size = tsize, steps_ahead = sahead)

#do not set epsilon, cost, and  kernel for hyperparameter optimization
# you can also set a range for them
model <- train_test(x, model=tsreg_svm(preproc, input_size=4, epsilon=0.0, cost=80.00), 
           sw = swsize, test_size = tsize, steps_ahead = sahead)

#do not set nhid and actfun for hyperparameter optimization
# you can also set a range for them
model <- train_test(x, model=tsreg_elm(preproc, input_size=4, nhid=3,actfun="purelin"), 
           sw = swsize, test_size = tsize, steps_ahead = sahead)

# do not set neurons and epochs for hyperparameter optimization
# you can also set a range for them
model <- train_test(x, model=tsreg_cnn(preproc, input_size=4, neurons=16,epochs=200), 
           sw = swsize, test_size = tsize, steps_ahead = sahead)

# do not set neurons and epochs for hyperparameter optimization
# you can also set a range for them
model <- train_test(x, model=tsreg_lstm(preproc, input_size=4, neurons=32, epochs=200), 
                    sw = swsize, test_size = tsize, steps_ahead = sahead)


