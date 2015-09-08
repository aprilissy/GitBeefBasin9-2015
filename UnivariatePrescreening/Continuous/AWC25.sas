/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
16.0846	1
10.23392	1
10.80644	1
10.78164	1
11.34244	2
9.4168	2
11.687	1
8.3102	1
12.49588	2
19.22032	2
11.20348	3
14.97	1
11.64376	1
9.86396	1
12.02732	1
11.687	1
8.44724	1
10.81976	2
15.7154	1
13.36236	1
15.19696	1
12.4908	1
9.84608	3
12.5218	2
10.56256	1
9.57468	1
8.459	1
6.221	1
11.14676	3
9.155	1
11.687	2
9.5878	1
11.6348	1
9.46916	2
7.7004	1
10.68452	2
11.687	3
8.0952	1
11.63808	1
10.11876	2
12.99324	3
10.19508	1
9.7364	1
10.431	1
11.542	1
9.155	1
14.3134	1
11.88536	3
14.67872	1
9.4168	2
9.30996	3
12.76468	1
10.32476	1
19.07176	1
5.55	1
8.15816	1
15.32976	2
9.285	1
10.81976	1
8.409	1
10.261	1
9.155	1
18.8466	1
9.8254	2
5.781217391	1
11.61424	1
9.155	2
10.48192	1
10.87028	1
7.08544	1
9.155	2
5.08	1
10.69764	1
8.26732	1
9.155	3
8.86024	1
9.285	3
10.30828	1
6.06124	3
7.98992	2
7.913	3
7.19424	3
5.24348	3
8.444	2
6.05596	1
12.14076	3
10.6628	1
9.88804	1
14.207	2
7.56812	1
11.17172	2
10.65692	1
9.1966	1
11.4424	2
5.5928	3
10.1984	1
14.10392	1
12.88932	1
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

/* Consider transformation */
proc transreg data=mydata;
	model boxcox(Soil / lambda= 0 to 1 by 0.05)
		=class(Shrub);
	title1 'Box-Cox on Minimum Sand Percentage';
run;

/* Re-fit model on transformed data */
data mydata; set mydata;
	trnsSoil = Soil**0.5;
run;

proc glm data=mydata plots=diagnostics;
	class Shrub;
	model trnsSoil = Shrub / solution;
	output out = out1 p=pred r=resid;
	means Shrub / Bon HOVtest=Levene;
	title1 'Comparison of all Shrub Classes'
	title2 '(lambda 0.5)';
run;
