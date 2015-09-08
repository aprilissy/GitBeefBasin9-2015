/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
40	1
30	1
60	1
60	1
55	2
55	2
65	1
60	1
60	2
45	2
55	3
45	1
50	1
50	1
60	1
50	1
60	1
50	2
60	1
45	1
45	1
45	1
20	3
65	2
35	1
55	1
65	1
60	1
70	3
70	1
75	2
60	1
60	1
60	2
65	1
75	2
60	3
60	1
60	1
60	2
55	3
50	1
60	1
60	1
65	1
70	1
70	1
50	3
50	1
45	2
40	3
70	1
55	1
40	1
55	1
75	1
60	2
35	1
70	1
60	1
70	1
60	1
65	1
35	2
65	1
75	1
60	2
70	1
65	1
65	1
60	2
70	1
75	1
30	1
70	3
60	1
70	3
65	1
70	3
80	2
60	3
45	3
60	3
60	2
70	1
55	3
60	1
65	1
50	2
70	1
55	2
50	1
40	1
60	2
75	3
60	1
50	1
45	1
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
