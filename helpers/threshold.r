# find the best threshold (epsilon) to use for selecting outliers

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