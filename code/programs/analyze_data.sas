%util_initScaproc(p_enableFlagName=g_enableScaProc
, p_outFilePath=%str(&g_logsRoot)
, p_outFileName=sca__analyze.txt)

proc summary data=sashelp.prdsale nway;
	class year quarter month country region division;
	var	actual predict;
	output out=work.prdsal_agg(drop=_:) sum=;
run;

%macro split(p_inDsName=work.prdsal_agg, p_outDsName=, p_yearValue=);
	PROC SUMMARY DATA=&p_inDsName(WHERE=(year = &p_yearValue)) NWAY;
		class country region division;
		var	actual predict;
		output out=&p_outDsName(drop=_:) sum=;
	RUN;
%mend split;

%split(p_inDsName=work.prdsal_agg, p_outDsName=work._1993_agg, p_yearValue=1993)
%split(p_inDsName=work.prdsal_agg, p_outDsName=work._1994_agg, p_yearValue=1994)

DATA work.usa;
	SET work._1993_agg (WHERE=(country='U.S.A.')) 
		work._1994_agg (WHERE=(country='U.S.A.'))
		;
RUN;

%util_termScaproc(p_enableFlagName=g_enableScaProc)