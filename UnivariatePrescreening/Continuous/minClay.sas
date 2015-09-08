/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
5	1
5	1
5	1
3	1
3	2
5	2
5	1
5	1
5	2
5	2
3	3
3	1
3	1
3	1
5	1
5	1
5	1
10	2
7	1
5	1
3	1
3	1
5	3
3	2
7	1
5	1
3	1
5	1
5	3
5	1
8	2
5	1
3	1
3	2
5	1
5	2
5	3
5	1
5	1
7	2
7	3
5	1
5	1
5	1
10	1
5	1
5	1
5	3
5	1
7	2
5	3
7	1
5	1
5	1
5	1
5	1
5	2
10	1
7	1
7	1
5	1
3	1
5	1
3	2
7	1
5	1
3	2
5	1
3	1
7	1
5	2
5	1
5	1
7	1
5	3
5	1
5	3
10	1
3	3
7	2
5	3
5	3
5	3
3	2
5	1
3	3
5	1
5	1
3	2
5	1
5	2
3	1
5	1
3	2
5	3
3	1
3	1
7	1
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
