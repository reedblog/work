#
# Read in data from table 47 and 48, assignment 2
# Reed Sorensen, 2-24-15
#
# -- Creates files 'brass_table47.RDS', 'brass_table48.RDS',
#    'input_data_example.RDS' and 'input_data_example.csv'
#

dir <- "C:/Users/rsoren/Dropbox/Notes/GH 590 A/final_project/"


table47_txt <- "family  age_group	index	mortality_ratio	coef_a	coef_b	coef_c
north	15-19	1	q(1)/D(1)	1.1119	-2.9287	0.8507
north	20-24	2	q(2)/D(2)	1.239	-0.6865	-0.2745
north	25-29	3	q(3)/D(3)	1.1884	0.0421	-0.5156
north	30-34	4	q(5)/D(4)	1.2046	0.3037	-0.5656
north	35-39	5	q(10)/D(5)	1.2586	0.4236	-0.5898
north	40-44	6	q(15)/D(6)	1.224	0.4222	-0.5456
north	45-49	7	q(20)/D(7)	1.1772	0.3486	-0.4624
south	15-19	1	q(1)/D(1)	1.0819	-3.0005	0.8689
south	20-24	2	q(2)/D(2)	1.2846	-0.6181	-0.3024
south	25-29	3	q(3)/D(3)	1.2223	0.0851	-0.4704
south	30-34	4	q(5)/D(4)	1.1905	0.2631	-0.4487
south	35-39	5	q(10)/D(5)	1.1911	0.3152	-0.4291
south	40-44	6	q(15)/D(6)	1.1564	0.3017	-0.3958
south	45-49	7	q(20)/D(7)	1.1307	0.2596	-0.3538
east	15-19	1	q(1)/D(1)	1.1461	-2.2536	0.6259
east	20-24	2	q(2)/D(2)	1.2231	-0.4301	-0.2245
east	25-29	3	q(3)/D(3)	1.1593	0.0581	-0.3479
east	30-34	4	q(5)/D(4)	1.1404	0.1991	-0.3487
east	35-39	5	q(10)/D(5)	1.154	0.2511	-0.3506
east	40-44	6	q(15)/D(6)	1.1336	0.2556	-0.3428
east	45-49	7	q(20)/D(7)	1.1201	0.2362	-0.3268
west	15-19	1	q(1)/D(1)	1.1415	-2.707	0.7663
west	20-24	2	q(2)/D(2)	1.2563	-0.5381	-0.2637
west	25-29	3	q(3)/D(3)	1.1851	0.0633	-0.4177
west	30-34	4	q(5)/D(4)	1.172	0.2341	-0.4272
west	35-39	5	q(10)/D(5)	1.1865	0.308	-0.4452
west	40-44	6	q(15)/D(6)	1.1746	0.3314	-0.4537
west	45-49	7	q(20)/D(7)	1.1639	0.319	-0.4435"

table47 <- read.table(text = table47_txt, header = TRUE)


##

table48_txt <- "family  age_group	index	age	parameter_estimate coef_a2 coef_b2 coef_c2
north	15-19	1	1	q(1)	1.0921	5.4732	-1.9672
north	20-24	2	2	q(2)	1.3207	5.3751	0.2133
north	25-29	3	3	q(3)	1.5996	2.6268	4.3701
north	30-34	4	5	q(5)	2.0779	-1.7908	9.4126
north	35-39	5	10	q(10)	2.7705	-7.3403	14.9352
north	40-44	6	15	q(15)	4.152	-12.2448	19.2349
north	45-49	7	20	q(20)	6.965	-13.916	19.9542
south	15-19	1	1	q(1)	1.09	5.4443	-1.9721
south	20-24	2	2	q(2)	1.3079	5.5568	0.2021
south	25-29	3	3	q(3)	1.1573	2.6755	4.7471
south	30-34	4	5	q(5)	1.9399	-2.2739	10.3876
south	35-39	5	10	q(10)	2.6157	-8.4819	16.5153
south	40-44	6	15	q(15)	4.0794	-13.8308	21.1886
south	45-49	7	20	q(20)	7.1796	-15.388	21.7892
east	15-19	1	1	q(1)	1.0959	5.5864	-1.9949
east	20-24	2	2	q(2)	1.2921	5.5897	0.3631
east	25-29	3	3	q(3)	1.5021	2.4692	5.0927
east	30-34	4	5	q(5)	1.9347	-2.6419	10.8533
east	35-39	5	10	q(10)	2.6197	-8.9693	17.0981
east	40-44	6	15	q(15)	4.1317	-14.355	21.8247
east	45-49	7	20	q(20)	7.3657	-15.8083	22.3005
west	15-19	1	1	q(1)	1.097	5.5628	-1.9956
west	20-24	2	2	q(2)	1.3062	5.5677	0.2962
west	25-29	3	3	q(3)	1.5305	2.5528	4.8962
west	30-34	4	5	q(5)	1.9991	-2.4261	10.4282
west	35-39	5	10	q(10)	2.7632	-8.4065	16.1787
west	40-44	6	15	q(15)	4.3468	-13.2436	20.199
west	45-49	7	20	q(20)	7.5242	-14.2013	20.0162"

table48 <- read.table(text = table48_txt, header = TRUE)


##

input_data_txt <- "mother_age index	number_of_women children_ever_born children_dead age_in_life_table
15-19	1	1472 250 14	1
20-24	2	1269 1396	114	2
25-29	3	915	2159 164 3
30-34	4	871	3388 287 5
35-39	5	661	3391 317 10
40-44	6	433	3402 348 15
45-49	7	315	3423 379 20"

input_data <- read.table(text = input_data_txt, header = TRUE)

##


saveRDS(table47, file = paste0(dir, "brass_table47.RDS"))
saveRDS(table48, file = paste0(dir, "brass_table48.RDS"))
saveRDS(input_data, file = paste0(dir, "input_data_example.RDS"))
write.csv(input_data, file = paste0(dir, "input_data_example.csv"), row.names = FALSE)



