/* cap input rows for the captured run */
options obs=100;

/* util_initScaproc / util_termScaproc: dependencies of usage_util_xxxxscaproc.sas,
   from ahmedanadwc/dotMySASProgram code/macros/ (unmodified) */
%macro util_initScaproc(p_enableFlagName=g_enableScaProc
, p_outFilePath=%str(&g_logsPath)
, p_outFileName=scaproc.txt
, p_options=ATTR OPENTIMES EXPANDMACROS
) / minoperator;

	%LOCAL
		l_outFilePath
		l_outFileName
		;

	%if (&p_enableFlagName = ) %then %return;
	%else %if (%symexist(&p_enableFlagName) = 0) %then %return;
	%if (NOT (%upcase(&&&p_enableFlagName) IN (1 Y VERBOSE))) %then %return;

	%let l_outFileName = %superq(p_outFileName);
	%if (%superq(l_outFileName)= )%then
		%let l_outFileName=&sysuserid._scaproc.txt;

	%let l_outFilePath = %superq(p_outFilePath);
	%if (%superq(l_outFilePath)= )%then
		%let l_outFilePath = %SYSFUNC(PATHNAME(WORK));

	PROC SCAPROC;
		RECORD %unquote("&l_outFilePath/&l_outFileName") &p_options;
	RUN;

%mend util_initScaproc;

%macro util_termScaproc(p_enableFlagName=g_enableScaProc) / minoperator;

	%if (&p_enableFlagName = ) %then %return;
	%else %if (%symexist(&p_enableFlagName) = 0) %then %return;
	%if (NOT (%upcase(&&&p_enableFlagName) IN (1 Y VERBOSE))) %then %return;

	PROC SCAPROC; WRITE; RUN;

%mend util_termScaproc;

/* Session globals this script depends on (normally set by config/setup.sas) */
%global g_enableScaProc g_logsRoot g_outputRoot;
%let g_enableScaProc = 1;
%let g_logsRoot   = %sysfunc(pathname(WORK));
%let g_outputRoot = %sysfunc(pathname(WORK));
