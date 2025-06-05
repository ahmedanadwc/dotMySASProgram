/**
  @file usage_util_xxxxscaproc.sas
  @brief Illustrates how to utilize the %util_initScaproc & %util_termScaproc macros

  <h4> SAS Macros </h4>
  @li util_initscaproc.sas
  @li util_termscaproc.sas
  
  <h4> Related Programs </h4>
 
  @version SAS 9.4
  @author Ahmed Al-Attar

**/

/* Initialize the collection and open the output file */
%util_initScaproc(p_enableFlagName=g_enableScaProc
, p_outFilePath=%str(&g_logsRoot)
, p_outFileName=sca__analyze.txt)


/** FOR CSV Files uploaded from Unix/MacOS **/
FILENAME my_CSV "/home/anawarehousing/dotMySASProgram/output/usa.csv" TERMSTR=LF;

/** Import the CSV file.  **/
PROC IMPORT DATAFILE=my_CSV
		    OUT=WORK.MYCSV
		    DBMS=CSV
		    REPLACE;
RUN;

/** Unassign the file reference.  **/
FILENAME my_CSV;

/** Import an XLSX file.  **/
PROC IMPORT DATAFILE="/home/anawarehousing/large_data/Motor_Vehicle_Collisions_-_Crashes_3.xlsx"
		    OUT=WORK.MYEXCEL
		    DBMS=XLSX
		    REPLACE;
RUN;

/** Print the results. **/
PROC PRINT DATA=WORK.MYEXCEL(obs=10); RUN;

proc summary data=sashelp.prdsale nway;
	class year quarter month country region division;
	var	actual predict;
	output out=work.prdsal_agg(drop=_:) sum=;
run;

%macro split(p_inDsName=work.prdsal_agg, p_outDsName=, p_yearValue=);
	PROC SUMMARY DATA=&p_inDsName(WHERE=(year = &p_yearValue)) NWAY;
		class country region division;
		var	actual predict;
		output out=&p_outDsName(drop=_t:) sum=;
	RUN;
%mend split;

%split(p_inDsName=work.prdsal_agg, p_outDsName=work._1993_agg, p_yearValue=1993)
%split(p_inDsName=work.prdsal_agg, p_outDsName=work._1994_agg, p_yearValue=1994)

DATA work.usa;
	SET work._1993_agg (WHERE=(country='U.S.A.') RENAME=(_freq_=count)) 
		work._1994_agg (WHERE=(country='U.S.A.') RENAME=(_freq_=count))
		;
RUN;

proc export data=work.usa
	outfile="&g_outputRoot/usa.csv"
	dbms=csv replace;
run;

/* Terminate the collection and close the output file */
%util_termScaproc(p_enableFlagName=g_enableScaProc)