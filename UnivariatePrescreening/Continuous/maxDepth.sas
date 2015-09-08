/* Enter data */
data mydata;
	input Soil Shrub;
	cards;
188	1
44	1
51	1
200	1
187	2
153	2
59	1
116	1
200	2
180	2
200	3
200	1
200	1
107	1
185	1
200	1
106	1
156	2
27	1
81	1
42	1
67	1
200	3
148	2
178	1
169	1
135	1
95	1
200	3
11	1
56	2
7	1
69	1
153	2
47	1
176	2
200	3
7	1
40	1
95	2
200	3
96	1
121	1
148	1
8	1
9	1
74	1
58	3
200	1
43	2
200	3
59	1
46	1
34	1
154	1
181	1
200	2
146	1
74	1
27	1
106	1
200	1
200	1
25	2
15	1
23	1
200	2
10	1
32	1
182	1
200	2
187	1
171	1
131	1
181	3
200	1
171	3
112	1
200	3
63	2
176	3
186	3
200	3
200	2
180	1
186	3
91	1
170	1
183	2
189	1
186	2
200	1
188	1
176	2
190	3
159	1
73	1
67	1
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
