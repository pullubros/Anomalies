# Anomaly Detection

Implementation of anomaly detection algorithm to detect anomalous behavior in server computers !
<br><br>

We first start with a 2D dataset of 307 compute servers that allows us to visualize what the algorithm is doing. The dimensions measure throughput (mb/s) and latency (ms) of response of each compute server. While a vast majority of the dataset are non-anomalous servers operating normally, some servers act anomalously. We use a Gaussian model to detect anomalous servers in the dataset and then find examples that have very low probability and hence can be considered anomalies. After that, the anomaly detection algorithm is applied to a larger dataset with many dimensions.
<br><br>

> latency distribution in dataset

<img src="plots\latency.png" width="400">
<br><br>
> throughput distribution in dataset

<img src="plots\throughput.png" width="400">
<br><br>
### Multivariate Gaussian Distribution
The multivariate gaussian distribution of a k-dimensional random vector x = [X1, X2, …, X<sub>k</sub>] can be written in the following notation:

> <img src=\resources\notation.png>

with k-dimensional mean vector

><img src=resources\mean.png>

and k x k covariance matrix

><img src=resources\var.png>

A "non-degenerate" multivariate gaussian distribution has density

> <img src=\resources\equation.png>

where <img src=resources\detSigma.png> is the determinant of <img src=resources\sigma.png>.
<br><br>
The density can be calculated as follows in R.
```r
Gaussian <- function(X, mu, sigma) {
    m = nrow(X)
    n = ncol(X)
    cv = diag(sigma)
    x = as.matrix(X - matrix(rep(mu, m), ncol = n, byrow = TRUE))
    p = (2*pi)^(-n/2) * det(cv)^(-0.5) * exp(-0.5*rowSums((x%*%solve(cv))*x))
    return (p)
}
```
<br>
> gaussian model for dataset

<img src = "plots\surface.png">
<br><br>
### Selecting Threshold

The low probability examples are more likely to be the anomalies in our dataset. One way to determine which examples are anomalies is to select a threshold with the highest F1 score on a labelled cross validation set. If an example x has a low probability p(x) < ε, then it is considered to be an anomaly. For many different values of ε, we compute the resulting F1 score from the number of examples that the current threshold classifies correctly and incorrectly.
The F1 score is computed using precision (prec) and recall (rec).

><img src=\resources\f1.png>

Here,

><img src=resources\prec.png>

where
* <i>tp</i> is the number of true positives - the ground truth label says it’s an anomaly and our algorithm correctly classified it as an anomaly.
* <i>fp</i> is the number of false positives - the ground truth label says it’s not an anomaly, but our algorithm incorrectly classified it as an anomaly.
* <i>fn</i> is the number of false negatives - the ground truth label says it’s an anomaly, but our algorithm incorrectly classified it as not being anomalous.

<br>
The optimal threshold can be calculated as follows in R.

```r
Threshold <- function(y, p) {
    best.ε = 0
    best.f1 = 0
    f1 = 0
    delta = (max(p)-min(p))/1000
    for (ε in seq(min(p), max(p), by = delta)) {
        false.positives = sum(p<ε & val.y==0, na.rm = TRUE)
        false.negatives = sum(p>=ε & val.y==1, na.rm = TRUE)
        true.positives = sum(p<ε & val.y==1, na.rm = TRUE)
        true.negatives = sum(p>=ε & val.y==1, na.rm = TRUE)
        precision = true.positives/(true.positives+false.positives)
        recall = true.positives/(true.positives+false.negatives)
        f1 = 2*precision*recall/(precision+recall)
        if (!is.nan(f1) & f1 > best.f1) {
            best.f1 = f1
            best.ε = ε
        }
    }
    return(list(best.ε, best.f1))
}
```
<br>

> outliers detected in dataset

<img src="plots\outliers.png" width="400">
<br><br>
### High Dimensional Dataset
The algorithm is now run on a dataset where each of the 1000 examples is described by 11 dimensions, capturing many more features of the compute servers.
> With an optimal threshold of 1.38e-18, a total of 117 anomalies are detected.
