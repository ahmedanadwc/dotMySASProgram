/* Caller for %util_initScaproc / %util_termScaproc (code/macros/util_initscaproc.sas,
   code/macros/util_termscaproc.sas), exercising the pattern shown in
   code/programs/usage_util_xxxxscaproc.sas: bracket a real analysis step between
   %util_initScaproc and %util_termScaproc so the enable-flag gating and the
   PROC SCAPROC RECORD/WRITE calls both run for real, unmodified. sashelp.class
   stands in for the repo's own analysis data (the source program's PROC IMPORT
   step reads a hardcoded external path that doesn't exist outside the author's
   machine). */

data work.class;
	set sashelp.class;
run;

%util_initScaproc(p_enableFlagName=g_enableScaProc
, p_outFilePath=%str(&g_logsRoot)
, p_outFileName=sca__class_analysis.txt)

proc means data=work.class n mean std min max;
	var age height weight;
run;

proc freq data=work.class;
	tables sex;
run;

%util_termScaproc(p_enableFlagName=g_enableScaProc)
