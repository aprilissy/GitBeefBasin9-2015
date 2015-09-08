/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
7	1
15	1
5	1
10	1
10	2
5	2
7	1
5	1
10	2
10	2
5	3
8	1
7	1
7	1
5	1
23	1
7	1
18	2
7	1
10	1
15	1
5	1
20	3
3	2
19	1
18	1
7	1
10	1
7	3
5	1
12	2
5	1
3	1
7	2
5	1
7	2
5	3
5	1
5	1
7	2
7	3
7	1
10	1
7	1
10	1
5	1
7	1
10	3
10	1
7	2
38	3
10	1
5	1
7	1
7	1
5	1
7	2
18	1
7	1
7	1
7	1
3	1
5	1
15	2
7	1
7	1
10	2
5	1
5	1
10	1
10	2
5	1
7	1
25	1
7	3
10	1
10	3
15	1
5	3
10	2
7	3
18	3
7	3
7	2
7	1
5	3
7	1
5	1
12	2
5	1
15	2
5	1
23	1
5	2
10	3
20	1
15	1
25	1
;
run;

proc print data=mydata;
run;

/* Look at numerical data summaries */
proc means data=mydata mean;
	class Shrub;
	var Soil;
	title1 'Mean Comparison';
	title2 '(And number of experimental units per shrub class)';
run;

/* Fit model and check assumptions */
proc glm data=mydata plots=diagnostics;
	class Shrub;
	model Soil=Shrub;
	means Shrub / HOVtest=Levene;
	title1 'comparison of all shrub classes';
run;
