#We define a generic function that can be re-used to calculate error rate when predicting binary outcome by cutting a continuous independent variable.
PredictErrorByCutContinousVar <- function(threshold, data, predictor, outcome){
    predict <- factor(data[,predictor] < threshold, levels=c("FALSE","TRUE"))
    table <- table(predict = predict, actual = data[,outcome])
    table
    return ((table[1,2]+table[2,1])/sum(table))
}

#Find error rate
ErrorRate <- function(threshold, model, data, field) {
    probs <- predict(model,newdata=data,type="response")
    outcomes <- factor(probs > threshold, levels=c("FALSE","TRUE"))
    table <- table(outcomes,data[,field])
    print(table)
    print ((table[1,2]+table[2,1])/sum(table))
    return((table[1,2]+table[2,1])/sum(table))
}

#This function is to plot the errorrate against different threshold
PlotErrorRateByThreshold<-function(model,data,field) {
    require(ggplot2)
    thresholds<-seq(0,1,0.01)
    errors <- sapply(thresholds,ErrorRate,model,data,field)
    df <- data.frame(Threshold=thresholds, Error.Rate = errors)
    ggplot(df,aes(x=Threshold,y=Error.Rate)) + geom_line()
}

#precision is a function to find the precision of a logistics model with a given threshold
#precision is the fraction of the predicted positives are true positives
precision <- function(threshold,model, data, field){
    response <- predict(model,newdata = data,type="response")
    prediction <- ifelse(response>threshold,"TRAIN","CAR")
    num.predicted.postive <- sum(prediction == "TRAIN")
    num.correct.positive <- sum(prediction == "TRAIN" & prediction == data[,field])
    return (num.correct.positive/num.predicted.postive)
}

#recall is a function to find the recall of a logisitc model with a given threshold
#recall is the fraction of the true positives the classifier finds
recall <- function(threshold,model, data, field){
    response <- predict(model,newdata = data,type="response")
    prediction <- ifelse(response>threshold,"TRAIN","CAR")
    num.predicted.postive <- sum(prediction == "TRAIN" & prediction == data[,field])
    num.true.positive <- sum(data[,field] == "TRAIN")
    return (num.predicted.postive/num.true.positive)
}

#This function is to plot precision and recall against thresholds
plotPrecisionVSRecall <- function(model, data, field){
    require(ggplot2)
    thresholds <- seq(0,1,by=0.01)
    precisions <- sapply(thresholds,precision,model,data,field)
    recalls <- sapply(thresholds,recall,model,data,field)
    df <- data.frame(Threshold=thresholds,Precision=precisions,Recall=recalls)
    library(reshape2)
    df1<-melt(df,id.vars="Threshold",variable.name="Type")
    ggplot(data = df1,aes(x=Threshold, y=value, color=Type,lty = Type)) + geom_line()
}
