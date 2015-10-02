PROC IMPORT OUT= WORK.Env 
            DATAFILE= "C:\Users\A00017434\Client work\Darger April\Sage.Env.April~v2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2;
    guessingrows=99; 
RUN;
PROC IMPORT OUT= WORK.Soil 
            DATAFILE= "C:\Users\A00017434\Client work\Darger April\SoilEnvironmentaldataApril~v2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
    guessingrows=99; 
RUN;

proc sgscatter data=soil;
    matrix h1_dryhue -- slope carbonatestage bioticcrustclass;
    run;

proc sgscatter data=soil;
    matrix h1_dryhue -- h1_depth;
    run;
proc freq data=soil;
    table (h1_dryhue h1_dryvalue h1_drychroma h1_moisthue)*(h1_dryvalue h1_drychroma h1_moisthue h1_moistvalue) / measures;
    run;

proc sgscatter data=soil;
    matrix awc25 -- slope;
    run;
proc freq data=soil;
    table (depth50 depth100 depth150 )*(depth100 depth150 depth200);
    run;
proc sgplot data=soil;
    vbox totaldepth / category=depth50;
proc sgplot data=soil;
    vbox totaldepth / category=depth100;
proc sgplot data=soil;
    vbox totaldepth / category=depth150;
proc sgplot data=soil;
    vbox totaldepth / category=depth200;
    run;

/* Merge datasets */
proc sort data=env; by id;
proc sort data=soil; by id;
data sage;
    merge env soil;
    by id;
    run;

data sage;
    set sage;
    if l_relcov=0 then l_relcov_trf = log( (l_relcov+0.3) / (100-(l_relcov+0.3)) );
    else l_relcov_trf = log( (l_relcov) / (100-(l_relcov)) );
    H1_claypercent_trf = log(h1_claypercent);
    run;
proc sgplot data=sage;
    scatter x=l_relcov y=l_relcov_trf;
    run;

proc reg data=sage;
    model l_relcov_trf = h1_dryhue h1_dryvalue h1_drychroma h1_moistvalue h1_moistchroma h1_sandpercent h1_claypercent h1_ph h1_effervescence h1_depth 
        totaldepth awc100 depth200 elevation aspect slope carbonatestage bioticcrustclass
        / collinoint;
        run;
proc reg data=sage;
    model l_relcov_trf = h1_dryhue h1_dryvalue h1_drychroma h1_moistvalue h1_moistchroma h1_sandpercent h1_claypercent h1_ph h1_effervescence h1_depth 
        totaldepth awc100 depth200 elevation aspect slope carbonatestage bioticcrustclass
        / selection=stepwise;
        run;
proc sgscatter data=sage;
    matrix l_relcov_trf h1_claypercent h1_ph totaldepth elevation bioticcrustclass;
    run;

/*  fit a model to nonzero l_relcov */
proc reg data=sage(where= (l_relcov ne 0));
    model l_relcov_trf = h1_dryhue h1_dryvalue h1_drychroma h1_moistvalue h1_moistchroma h1_sandpercent h1_claypercent_trf h1_ph h1_effervescence h1_depth 
        totaldepth awc100 depth200 elevation aspect slope carbonatestage bioticcrustclass
        / selection=stepwise;
        run;
proc hpgenselect data=sage(where= (l_relcov ne 0));
    model l_relcov_trf = h1_dryhue h1_dryvalue h1_drychroma h1_moistvalue h1_moistchroma h1_sandpercent h1_claypercent_trf h1_ph h1_effervescence h1_depth 
        totaldepth awc100 depth200 elevation /*aspect*/ slope carbonatestage bioticcrustclass / distribution=normal;
*   selection method=lasso(choose=aicc)  details=all;
   selection method=stepwise(sle=0.1 sls=0.1)  details=all;
   output out=m1 pred=pred;
   run;
proc sgscatter data=sage(where= (l_relcov ne 0));
    matrix l_relcov_trf h1_claypercent_trf h1_ph bioticcrustclass ;
    run;
proc sgplot data=sage(where= (l_relcov ne 0));
    vbox l_relcov_trf / category=carbonatestage;
proc sgplot data=sage(where= (l_relcov ne 0));
    vbox l_relcov_trf / category=h1_dryvalue;
    run;
data m1plus;
    merge sage(where= (l_relcov ne 0)) m1;
    run;
proc sgpanel data=m1plus;
    panelby bioticcrustclass / columns=7;
    reg x=h1_claypercent_trf y=pred / nomarkers;
    scatter x=h1_claypercent_trf y=l_relcov_trf;
    run;
proc sgpanel data=m1plus;
    panelby h1_claypercent_trf / columns=7;
    reg x=bioticcrustclass y=pred / nomarkers;
    scatter x=bioticcrustclass y=l_relcov_trf;
    run;

/*  fit a model to binary form */
data sage;
    set sage;
    if l_relcov =0 then l_relcov_bin = 0;
    else if l_relcov > 0 then l_relcov_bin = 1;
    run;
proc hpgenselect data=sage;
    model l_relcov_bin = h1_dryhue h1_dryvalue h1_drychroma h1_moistvalue h1_moistchroma h1_sandpercent h1_claypercent_trf h1_ph h1_effervescence h1_depth 
        totaldepth awc100 depth200 elevation /*aspect*/ slope carbonatestage bioticcrustclass / distribution=binary;
*   selection method=lasso(choose=aicc)  details=all;
   selection method=stepwise(sle=0.1 sls=0.1)  details=all;
   run;
proc sgplot data=sage;
    vbox totaldepth / category=l_relcov_bin;
proc sgplot data=sage;
    vbox h1_ph / category=l_relcov_bin;
proc sgplot data=sage;
    vbox bioticcrustclass / category=l_relcov_bin;
proc sgplot data=sage;
    vbox elevation / category=l_relcov_bin;
    run;

data sage;
    set sage;
    d_relcov = l_d_relcov - l_relcov;
    run;
proc sgscatter data=sage;
    matrix l_d_denm2 -- l_relcov d_relcov;
    run;

