/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
8.2	1
8.2	1
8.4	1
8.6	1
7.8	2
8.5	2
8.4	1
8.6	1
8.1	2
8.6	2
8.4	3
8.6	1
8.4	1
8.6	1
8.6	1
8.6	1
8.3	1
8.3	2
7.8	1
8.4	1
8	1
8.6	1
8.2	3
8.6	2
8.2	1
8.8	1
8.4	1
8.4	1
8.4	3
7.8	1
8.2	2
7.6	1
8	1
8.6	2
7.8	1
8.5	2
8.2	3
7.8	1
8.2	1
8.1	2
8.2	3
8.6	1
8.4	1
8.3	1
7.8	1
7.8	1
8.4	1
8.4	3
8.6	1
8.2	2
8.2	3
8.6	1
8.2	1
8.1	1
8.6	1
8.4	1
8.4	2
8.2	1
8.3	1
8.1	1
8.4	1
8.4	1
8.4	1
8.1	2
7.8	1
8.1	1
8.2	2
8.1	1
7.8	1
8.6	1
8.1	2
8.2	1
8.4	1
8.2	1
8.4	3
8	1
8	3
8.6	1
8.2	3
8.6	2
8.4	3
7.6	3
8.1	3
8.2	2
8.4	1
8.6	3
8.6	1
8.4	1
8.8	2
8.4	1
8.8	2
8.4	1
8.6	1
8.2	2
8.4	3
8.4	1
8.6	1
8.4	1
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
