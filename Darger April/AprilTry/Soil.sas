PROC IMPORT OUT= WORK.Soil 
            DATAFILE= "\\Client\F$\Darger April\SoilEnvironmentaldataApr
il~v2.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
