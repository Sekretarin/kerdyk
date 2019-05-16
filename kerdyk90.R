library(rio) # импорт-экспорт данных
library(tidyverse) # манипуляции с данными
library(skimr) # описательные статистики

d = import("pr201_marks_leaked.csv")
skim(d)
View(d)

c1 = mean(d$k3_final)
c2 = mean(d$k4_final)

d1 = sd(d$k3_final)
n = nrow(d)
t_crit = qt(0.975, df = n - 1) # квантиль t-распределения
left_ci = c1 - t_crit * (d1/ sqrt(n))
left_ci

right_ci = c1 + t_crit * (d1/ sqrt(n))
right_ci
c1
t.test(d$k3_final)
# доверительные интервал для mu руками:
mu_hat = mean(cars$speed) # среднее выборочное
mu_hat
sigma_hat = sd(cars$speed) # корень из несмещённой оценки дисперсии
sigma_hat
n = nrow(cars) # число строк в таблице cars
n

se_mu_hat = sigma_hat / sqrt(n)
t_crit = qt(0.975, df = n - 1) # квантиль t-распределения

left_ci = mu_hat - t_crit * se_mu_hat
right_ci = mu_hat + t_crit * se_mu_hat

left_ci
right_ci

# для проверки гипотезы
mu_zero = 20 # основная гипотеза
zet = (mu_hat - mu_zero)/se_mu_hat
t_crit = qt(0.975, df = n - 1) # выбери уровень доверия, здесь 95% стоит
zet < t_crit