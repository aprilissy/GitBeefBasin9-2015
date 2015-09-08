/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
7.8	1
7.8	1
7.8	1
7.8	1
7.6	2
6.8	2
7.8	1
7	1
7.8	2
7.6	2
7.6	3
6.4	1
7.8	1
7.3	1
7.4	1
7.2	1
7.8	1
8.1	2
7.5	1
7.6	1
7.8	1
7.6	1
7.5	3
7.6	2
7.6	1
7.6	1
7.6	1
7.3	1
7.6	3
7.8	1
8.1	2
7.6	1
7.6	1
7.8	2
7.6	1
7.4	2
7.8	3
7.8	1
6.6	1
7.8	2
7.8	3
7.6	1
6.5	1
7.2	1
7.8	1
7.8	1
6.6	1
7.6	3
6.4	1
7.6	2
7.6	3
7.6	1
7	1
7.6	1
7	1
7.6	1
7.4	2
6.6	1
7.6	1
7.4	1
7.8	1
7.4	1
7.8	1
7.8	2
7.6	1
7.8	1
7.4	2
8.1	1
7.6	1
7.8	1
7.3	2
8	1
7.6	1
6.6	1
7.6	3
7.5	1
8	3
6.6	1
7.6	3
7.4	2
7.6	3
7.4	3
8	3
7.6	2
7.4	1
7.6	3
7	1
7.6	1
7.6	2
8	1
7.4	2
7.4	1
7.8	1
7.8	2
7.6	3
6.8	1
8	1
7.6	1
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
