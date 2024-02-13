# creator: Sarah Berger
# date of creation: 2024 02 05
# Objective: describe and compare two populations


# Load table

getwd()
d <- read.csv("C:/Users/sarah/Documents/professionnel_study/code_with_me/statin/data/simulated_statin_dataset.csv")
head(d)
summary(d)
str(d)
# Number by group
table(d$group)
# Number of missing
apply(is.na(d), 2, sum)

# Describe by group

# -------------------------------------
# option 1 : code 1/5 time 5/5 control 5/5
# Copy in a table then same with other group

names(d)
head(d)
table(d$group)
# Keep group atorvastatin
d_sub <- d[d$group=="atorvastatin",]
head(d_sub)
dim(d_sub)
table(d_sub$group)

#describe group
# quantitative variables
# In method, continuous in mean and sd for normally distributed, 
# otherwise medians and interquartile range
hist(d_sub$age_yr)
hist(d_sub$body_mass_index)

mean(d_sub$age_yr)
round(mean(d_sub$age_yr), 1)
sd(d_sub$age_yr)
round(sd(d_sub$age_yr),1)

round(mean(d_sub$body_mass_index), 1)
round(sd(d_sub$body_mass_index),1)

# qualitative variables
#Nothing in method
# In Table 1 I see N(%)
d_sub$sex
table(d_sub$sex)[2]
round(prop.table(table(d_sub$sex))*100, 1)[2]
table(d_sub$diabetes)[2]
round(prop.table(table(d_sub$diabetes))*100, 1)[2]
table(d_sub$hypertension)[2]
round(prop.table(table(d_sub$hypertension))*100, 1)[2]
table(d_sub$current_smoking)[2]
round(prop.table(table(d_sub$current_smoking))*100, 1)[2]
table(d_sub$previous_mi)[2]
round(prop.table(table(d_sub$previous_mi))*100, 1)[2]
table(d_sub$previous_pci)[2]
round(prop.table(table(d_sub$previous_pci))*100, 1)[2]

# Same with other group
table(d$group)
d_sub <- d[d$group=="rosuvastatin",]
hist(d_sub$age_yr)
hist(d_sub$body_mass_index)

round(mean(d_sub$age_yr), 1)
round(sd(d_sub$age_yr),1)

round(mean(d_sub$body_mass_index), 1)
round(sd(d_sub$body_mass_index),1)

t.test(d$age_yr~d$group, paired=FALSE, var.equal=TRUE)
chi2 <- chisq.test(table(d$sex, d$group), correct = FALSE)
chi2$expected

#-----------------------------------------------
# Option 2: code 1/5 time 1/5 flexibility 3/5 control 3/5
install.packages("table1")
library(table1)
#https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

d <- read.csv("C:/Users/sarah/Documents/professionnel_study/code_with_me/statin/data/simulated_statin_dataset.csv")

colnames(d)
table1(~ age_yr + factor(sex) + factor(race) | group, data=d, overall = NULL)

# Modify feature column
d$sex <- factor(d$sex)
d$race <- factor(d$race)
label(d$age_yr) <- "Age"
units(d$age_yr) <- "yr"
label(d$sex) <- "Sex"
units(d$sex) <- "no. (%)"
label(d$race) <- "White race"
units(d$race) <-  "no. (%)"
table1(~ age_yr + sex + race | group, data=d, overall = NULL)

# Modify the stat for continuous
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
table1(~ age_yr + sex + race | group, data=d, overall = NULL, render.continuous=my.render.cont)

# Add p.value
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g), correct=FALSE)$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ age_yr + sex + race | group, data=d, overall = NULL, 
       render.continuous=my.render.cont,
       extra.col = list(`P-value`=pvalue)
       )
#-------------------------------------------
# Option 3: code your own function

d <- read.csv("data/simulated_statin_dataset.csv")

add_dec0 <- function(vectomodify){
  #vectomodify <- c("203 (39%)", "500 (61.3%)")
  x_nodec <- vectomodify[!stringr::str_detect(vectomodify, "\\.[0-9]%\\)")]
  x_nodec <- stringr::str_replace(x_nodec, "%\\)", "\\.0%\\)") 
  vectomodify[!stringr::str_detect(vectomodify, "\\.[0-9]%\\)")] <- x_nodec
  return(vectomodify)
}

is_quantitative <- function(column) {
  # Try to convert the column to numeric
  numeric_column <- suppressWarnings(as.numeric(column))
  
  # Check if the conversion was successful and if there are more than 10 unique values
  if (!any(is.na(numeric_column)) && length(unique(numeric_column)) > 10) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

describe_2groups <- function(data, group_column, group1_name, group2_name){
  # data = d
  # group_column = "group"
  # group1_name = "atorvastatin"
  # group2_name = "rosuvastatin"
  quantitative_cols <- sapply(data, is_quantitative)
  qualitative_cols <- !quantitative_cols
  qualitative_cols[group_column] <- FALSE
  res_quanti <- data.frame(feature = c(), level=c(), group1 = c(), group2 = c())
  for (col in colnames(data)[quantitative_cols]){
    #col <-  colnames(data)[quantitative_cols][12]
    x1 <- data[data[,group_column] %in% group1_name,col]
    x2 <- data[data[,group_column] %in% group2_name,col]
    res1 <- paste0(round(mean(x1, na.rm=TRUE), 1), " (", round(sd(x1, na.rm=TRUE), 1), ")")
    res2 <- paste0(round(mean(x2, na.rm=TRUE), 1), " (", round(sd(x2, na.rm=TRUE), 1), ")")
    df_2_groups <- data.frame(feature = col, level = "", group1=res1, group2=res2)
    res_quanti <- rbind(res_quanti, df_2_groups)
  }
  res_quali <- data.frame(feature = c(), level = c(), group1 = c(), group2 = c())
  for (col in colnames(data)[qualitative_cols]){
    #col <-  colnames(data)[qualitative_cols][10]
    x1 <- data[data[,group_column] %in% group1_name,col]
    x2 <- data[data[,group_column] %in% group2_name,col]
    
    group1_counts <- table(x1, useNA = "no")
    group1_percents <- round(prop.table(group1_counts) * 100, 1)
    group2_counts <- table(x2, useNA = "no")
    group2_percents <- round(prop.table(group2_counts) * 100, 1)
    
    group1_desc <- paste0(group1_counts, " (", group1_percents, "%)")
    group2_desc <- paste0(group2_counts, " (", group2_percents, "%)")
    
    group1_desc <- as.vector(sapply(group1_desc, add_dec0))
    group2_desc <- as.vector(sapply(group2_desc, add_dec0))
    df_2_groups <- data.frame(feature = col, level = names(group1_counts), group1 = group1_desc, group2 = group2_desc)
    res_quali <- rbind(res_quali, df_2_groups)
  }
  all_results <- rbind(res_quanti, res_quali)
  colnames(all_results) <- c("feature", "level", group1_name, group2_name)
  return(all_results)  
}

all_results <- describe_2groups(data = d, group_column = "group",group1_name = "atorvastatin", group2_name = "rosuvastatin")

#Export features and level to format names
format_df <- data.frame(formatted_name = "", feature = all_results$feature, level = all_results$level)
write.csv(format_df, file = "data/format_results.csv", row.names = FALSE)

# Format names
df_format <- read.csv("data/format_results_filled.csv")
df_format <- df_format[df_format$keep %in% 1, ]
df_format <- df_format[order(df_format$order), ]

table1 <- merge(df_format, all_results, by = c("feature", "level"), all.x = TRUE)
table1 <- table1[order(table1$order),]
table1[,c("formatted_name", "atorvastatin", "rosuvastatin")]
