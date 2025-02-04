########################
## In the name of GOD ##
########################


###########
##  BODY ##
###########

# install.packages("MatchIt")
library(MatchIt)

#install.packages("optmatch")
library(optmatch)

match <- matchit(treat ~ x1 + x2 + x3,
                 data = data, 
                 method = "exact",
                 #	"nearest": تطبیق نزدیکترین همسایه (پیش‌فرض).
                 #	"exact": تطبیق دقیق.
                 #	"optimal": تطبیق بهینه.
                 #	"subclass": تطبیق بر اساس زیر طبقات.
                 #	"genetic": تطبیق ژنتیکی.      pop.size = 100
                 #	"full": تطبیق کامل.
                 distance = "logit", # for nearest
                 #	"logit": پیوند لجستیک.
                 #	"probit": پیوند پروبیت
                 #	"mahalanobis": فاصله محلانوبیس.
                 verbose = TRUE, 
                 #	مشخص می‌کند که آیا باید وضعیت الگوریتم تطبیق چاپ شود یا نه
                 discard = "hull.control", 
                 #  "none": هیچ واحدی دور انداخته نمی‌شود (پیش‌فرض).
                 #	"hull.both": تمام واحدهایی که خارج از بدنه محدب قرار دارند دور ریخته می‌شوند.
                 #  "hull.treat": فقط واحدهای درمان که خارج از بدنه محدب قرار دارند دور ریخته می‌شوند.
                 #	"hull.control": فقط واحدهای کنترل که خارج از بدنه محدب قرار دارند دور ریخته می‌شوند.
                 m.order = "smallest",
                 #	"largest": از بزرگترین مقدار فاصله به کوچکترین مقدار.
                 #	"smallest": از کوچکترین مقدار فاصله به بزرگترین مقدار.
                 #	"random": ترتیب تصادفی.
                 sub.by = "treat",
                 #  . مشخص می‌کند که واحدها براساس کدام معیار طبقه‌بندی شوند 
                 #	"treat": تعداد واحدهای درمانی.
                 #	"control": تعداد واحدهای کنترل.
                 #	"all": تعداد کل واحدها.
                 ratio = 2, # for nearest
                 #  تعداد واحدهای کنترل که باید با هر واحد درمانی تطبیق داده شوند.
                 #  پیش‌فرض 1 است.
                 replace = FALSE,
                 #	مشخص می‌کند که آیا هر واحد کنترل می‌تواند بیش از یک بار با واحد درمان تطبیق داده شود یا خیر. پیش‌فرض 
)
summary(match)
matched.data <- match.data(match)



#############
## MatchIt ##
#############


# داده نمونه
data("lalonde")
colnames(lalonde)

# تطبیق دقیق
exact_match <- matchit(treat ~ age + educ + race + married + re74 + re75,
                 data = lalonde, method = "exact")
summary(exact_match)
e.data <- match.data(exact_match)

# تطبیق کامل
full_match <- matchit(treat ~ age + educ + race + married + re74 + re75,
                   data = lalonde, method = "full")
summary(full_match)
f.data <- match.data(full_match)

# تطبیق نزدیک‌ترین همسایه
nn_match <- matchit(treat ~ age + educ + race + married + re74 + re75,
                 data = lalonde, method = "nearest") 
summary(nn_match)
n.data <- match.data(nn_match)

# تطبیق نزدیک‌ترین همسایه پیوند لجستیک
nn_match_log <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                       data = lalonde, 
                       method = "nearest", 
                       distance = "logit")
summary(nn_match_log)
nn.data <- match.data(nn_match_log)

# تطبیق نزدیک‌ترین همسایه پیوند پروبیت
nn_match_pro <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                       data = lalonde, 
                       method = "nearest", 
                       distance = "probit")
summary(exact_match_pro)
np.data <- match.data(nn_match_pro)

# تطبیق نزدیک‌ترین همسایه فاصله محلانوبیس
nn_match_ma <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                       data = lalonde, 
                       method = "nearest", 
                       distance = "mahalanobis")
summary(exact_match_ma)
nm.data <- match.data(nn_match_ma)

# تطبیق نزدیک‌ترین همسایه نسبت 1 به 1
nn_match1 <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                    data = lalonde, 
                    method = "nearest", 
                    ratio = 1) 
summary(exact_match1)
n1.data <- match.data(nn_match1)

# تطبیق نزدیک‌ترین همسایه نسبت 2 به 1
nn_match2 <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                    data = lalonde, 
                    method = "nearest", 
                    ratio = 2)  
summary(exact_match2)
n2.data <- match.data(nn_match2)

# تطبیق طبقه بندی
subclass_match <- matchit(treat ~ age + educ + race + married + re74 + re75,
                 data = lalonde,
                 method = "subclass")
summary(subclass_match)
sub.data <- match.data(subclass_match)

# تطبیق طبقه بندی با تعداد طبقات تعیین شده
subclass_match5 <- matchit(treat ~ age + educ + race + married + re74 + re75,
                 data = lalonde,
                 method = "subclass" ,
                 subclass = 5)
summary(subclass_match5)
subc.data <- match.data(subclass_match5)

# روش تطبیق ژنتیکی
genetic_match <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                         data = lalonde, 
                         method = "genetic", 
                         pop.size = 100) 
summary(genetic_match)
gen.data <- match.data(genetic_match)

# روش تطبیق بهینه
#install.packages("optmatch")
library(optmatch)
optimal_match <- matchit(treat ~ age + educ + race + married + re74 + re75, 
                         data = lalonde, 
                         method = "optimal")
summary(optimal_match)
opt.data <- match.data(optimal_match)





##################
## matched data ##
##################


# تبدیل به دیتا فریم
#  استخراج داده‌ها برای هر بلوک تطبیق‌شده است.
# اثرات هر واحد در تحلیل را تعدیل کنند.  وزن ها
#  احتمال تعلق یک واحد به گروه درمانی است که با استفاده از
# رگرسیون برای هر واحد محاسبه می‌شود
m.data4 <- match.data(full_match, subclass = "block", weights = "w", distance = "pscore")
m.data4



#############
### Plots ###
#############

# Q-Q plot
plot(full_match, type = "qq", interactive = FALSE)
#نمودار به صورت تعاملی

# jitter
plot(full_match, type = "jitter", interactive = FALSE)

# چگالی
plot(full_match, type = "density", interactive = FALSE)

# هیستوگرام
windows(width = 10, height = 7)
par(mar = c(5, 4, 4, 2) + 0.1)
plot(full_match, type = "hist", interactive = FALSE)


###########
### glm ###
###########

model <- glm(re78 ~ treat + age + educ + race + re74 + re75, 
             data = f.data)
summary(model)



