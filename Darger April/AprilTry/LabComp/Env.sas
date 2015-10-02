PROC IMPORT OUT= WORK.Env 
            DATAFILE= "E:\Darger April\Sage.Env.April~v2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
