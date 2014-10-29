#' Calculate the operating point for a given ROCR perf object.
#' 
#' @param perf The perf object.
#' @param method The confidence estimation method.
#' @param desired.tpr The desired TPR. The operating point returned will have a
#'   95 percent chance of achieving this TPR or higher.
#' 
#' @return A vector containing the operating point, as well as estimates of TPR and FPR.


calculate.operating.parameters = function(
perf, 
method="frequency", 
desired.tpr="0.95"
) {
  op = 2 # filler for data structure construction
  op.param = data.frame(op)
  names(op.param) = c("op")
  if (method == "CI") {
    for (i in unique(perf$alpha.values)) {
      foo = t.test(perf[perf$alpha.values == i, ]$y.values)
      if (i == Inf) {
        next
      }
      if (foo$conf.int[1] > desired.tpr) {
        op.param$op = i
        break
      }
    }
  } else if (method == "confidence.range") {
    for (i in unique(perf$alpha.values)) {
      if (i == Inf) {
        next
      }
      mean.TPR = mean(perf[perf$alpha.values == i, ]$y.values)
      sd.TPR = sd(perf[perf$alpha.values == i, ]$y.values)
      ranged.TPR = qnorm(0.05, mean.TPR, sd.TPR)
      if (ranged.TPR >= desired.tpr) {
        op.param$op = i
        break
      }
      
    }
  } else if (method == "frequency") {
    for (i in unique(perf$alpha.values)) {
      if (i == Inf) {
        next
      }
      valid = sum(perf[perf$alpha.values == i, ]$y.values >= desired.tpr) 
      invalid = sum(perf[perf$alpha.values == i, ]$y.values < desired.tpr)
      total = valid + invalid
      if (valid / total >= 0.95) {
        op.param$op = i
        break
      }
    }
  } else {
    stop("Unknown method")
  }
  foo = t.test(perf[perf$alpha.values == op.param$op, ]$x.values)
  bar = t.test(perf[perf$alpha.values == op.param$op, ]$y.values)
  op.param$xmean = foo$estimate
  op.param$xlower = foo$conf.int[1]
  op.param$xhigher = foo$conf.int[2]
  op.param$ymean = bar$estimate
  op.param$ylower = bar$conf.int[1]
  op.param$yhigher = bar$conf.int[2]
  return(op.param)
}
