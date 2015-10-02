PROC IMPORT OUT= WORK.Env 
            DATAFILE= "\\Client\F$\Darger April\Sage.Env.April~v2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
