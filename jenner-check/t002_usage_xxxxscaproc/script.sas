/* Adapted from code/programs/usage_util_xxxxscaproc.sas ("Illustrates how to utilize
   the %util_initScaproc & %util_termScaproc macros"). Two substitutions from the
   source file:
   (1) The PROC IMPORT block reads two hardcoded external paths
       (/home/anawarehousing/...) that don't exist outside the author's machine and
       whose output (WORK.MYCSV, WORK.MYEXCEL) is never used by the rest of the
       script, so it's dropped here.
   (2) sashelp.prdsale (the source's input table) isn't in this engine's sashelp
       set, so it's replaced by a small mock table with the identical column shape
       (year quarter month country region division actual predict) the script's
       PROC SUMMARY/%split logic reads. Everything downstream — the summary, the
       %split macro caller, the two-year US comparison, and the PROC EXPORT — is
       unchanged from the source file. */

data work.prdsale;
	length country $12 region $12 division $12 month 8 quarter 8 year 8 actual predict 8;
	input year quarter month country $ region $ division $ actual predict;
	datalines;
1993 1 1 U.S.A. EAST WEST 1000 1100
1993 1 1 CANADA EAST WEST 500 550
1993 2 4 U.S.A. WEST EAST 1200 1150
1993 2 4 GERMANY WEST EAST 700 650
1994 1 1 U.S.A. EAST WEST 1050 1120
1994 1 1 CANADA EAST WEST 520 540
1994 2 4 U.S.A. WEST EAST 1250 1200
1994 2 4 GERMANY WEST EAST 720 690
;
run;

%util_initScaproc(p_enableFlagName=g_enableScaProc
, p_outFilePath=%str(&g_logsRoot)
, p_outFileName=sca__analyze.txt)

proc summary data=work.prdsale nway;
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

proc print data=work.usa noobs;
run;

proc export data=work.usa
	outfile="&g_outputRoot/usa.csv"
	dbms=csv replace;
run;

/* Terminate the collection and close the output file */
%util_termScaproc(p_enableFlagName=g_enableScaProc)
