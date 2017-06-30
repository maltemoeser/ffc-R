require(data.table)
require(caret)

setwd("~/Code/ffc-R/code")

# import data
d <- fread("../data/background.csv")
# d <- fread("background.csv")
setkeyv(d, "challengeID")

ids <- d$challengeID

# remove IDs
noIds <- c("mothid4", "fathid4", "fathid3", "mothid3", "fathid2", "mothid2", "fathid1")
d[, (noIds):=NULL]

# remove character columns
charCols <- colnames(d)[sapply(d, function(x) {class(x) == "character"})]
d[, (charCols) := NULL]

# remove low variance columns
d[, (nearZeroVar(d)):=NULL]

# remove ID
d[, (c("challengeID")):=NULL]

## remove highly correlated columns
x <- d
x[is.na(x)] <- 0
cm <- cor(x, method = "spearman")
cm[upper.tri(cm)] <- 0
diag(cm) <- 0
corTooLarge <- apply(cm, 2, function(x) any(abs(x) > 0.9))
d[, (names(corTooLarge[corTooLarge == TRUE])):=NULL]
rm(x,cm)

# convert NA codes into real NA, except for skips and refuses
d[d < 0 & d != -1 & d != -6] <- NA

# treat numerics different than factors
isNumeric <- sapply(d, function(x) {class(x) == "numeric" | length(unique(x)) > 8})
numericCols <- colnames(d)[isNumeric]
factorCols <- colnames(d)[!isNumeric]

# create extra columns for skips in non-categorical
skip <- data.table(d == -6)
skip[is.na(skip)] <- FALSE
skip <- skip[, (factorCols):=NULL]
## Rename the columns of the missing indicators to start with miss_
colnames(skip) <- paste("skip_", colnames(skip), sep = "")
## Keep only the columns where some are missing
msk <- apply(skip, 2, var, na.rm = T) > 0
skip <- skip[, msk, with = F]
skip$challengeID <- ids

# create extra columns for refuse
refuse <- data.table(d == -1)
refuse[is.na(refuse)] <- FALSE
refuse <- refuse[, (factorCols):=NULL]
## Rename the columns of the missing indicators to start with miss_
colnames(refuse) <- paste("refuse_", colnames(refuse), sep = "")
## Keep only the columns where some are missing
msk <- apply(refuse, 2, var, na.rm = T) > 0
refuse <- refuse[, msk, with = F]
refuse$challengeID <- ids

# set remaining negatives to 0 now that we have additional columns for them
for (col in numericCols) d[get(col) < 0, (col) := NA]

# impute
filled.data <- lapply(
  d, function(x) {
    replacement <- 0
    if(class(x) == "numeric") {
      ## Find the mean for numeric columns
      replacement <- mean(na.omit(x[x>0]))
    } else {
      ## Identify the unique values of that variable
      ux <- unique(na.omit(x[x > 0]))  
      ## Find the mode
      replacement <- ux[which.max(tabulate(match(na.omit(x[x > 0]), ux)))]
    }
    if (is.na(replacement)) replacement <- 1
    ## Replace with the mode if missing
    x[is.na(x)] <- replacement
    return(x)
  }
)
d <- as.data.table(filled.data)

# scale all numeric cols
for (j in numericCols) set(d, j = j, value = scale(d[[j]]))

# convert column type to factor
# https://stackoverflow.com/questions/32940580/convert-some-column-classes-in-data-table
for (j in factorCols) set(d, j = j, value = factor(d[[j]]))

# merge everything back together
d$challengeID <- ids
d <- merge(d, skip, all=TRUE)
d <- merge(d, refuse, all=TRUE)
d[, (c("challengeID")):=NULL]

# another cleanup of zero variance columns
d[, (nearZeroVar(d)):=NULL]

save.image("../data/ffc-impute.RData")
