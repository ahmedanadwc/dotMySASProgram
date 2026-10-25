/**
  @file util_scatext2graphvizdot.sas
  @brief Processes a list of scaProc generated text files.
  @details Accepts a list a scaProc (SAS Code Analyzer) generated text files, and dynamically
  composes the required %util_parseScaproc macro call for every file in the list, before executing
  all the generated calls to mass convert/parse sca text to a GraphVis *.dot file

  Example usage:
  
      %util_scaText2graphvizDot (
        p_inTxtFileList=%str(~/dotMySASProgram/output/sca_text_files.txt)
      , p_outDotFilesDir=%str(~/dotMySASProgram/output/dotfiles_wo_attrs)
      , p_includeAttrs_yn=N
      , p_includeSteps_yn=Y
      , p_rankDir=LR)

  @param [in] p_inTxtFileList= File name containing list of scaProc (SAS Code Analyzer) 
                               generated text file names. Required.
  @param [in] p_outDotFilesDir= The GRAPHVIZ (*.dot) output directory. Required.
  @param [in] p_includeAttrs_yn= A Y/N flag to include table columns in the output. Default:Y.
  @param [in] p_includeSteps_yn= A Y/N flag to include processing steps and elapse times in the output. 
                                 Default:Y.
  @param [in] p_rankDir= Sets direction of graph layout. Valid Values:TB|BT|LR|RL.
                         Default:TB.
  @param [in] p_outJson_yn= A Y/N flag to also write a JSON graph (nodes/edges) next to every
                            *.dot file, named after the input file with a .json extension. Default:N.
  @param [in] p_outDot_yn= A Y/N flag to write the *.dot files. Set to N for JSON-only output
                           (requires p_outJson_yn=Y). Default:Y.
  @param [in] p_dotTheme= The format of the *.dot files. Valid Values:DEFAULT|SIMPLE (see %util_parseScaproc).
                          Default:DEFAULT.

  @version 9.4
  @author Ahmed Al-Attar

**/

/* Declare a macro to process the listed *.txt files*/
%macro util_scaText2graphvizDot (p_inTxtFileList=
, p_outDotFilesDir=
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=TB
, p_outJson_yn=N
, p_outDot_yn=Y
, p_dotTheme=DEFAULT) / minoperator;
  
	%LOCAL  
		l_eTime
		l_msg
		l_rc
		l_rTime
		l_sTime
		;

  %LOCAL
    l_dotfilename
		l_includeAttrs_yn
		l_includeSteps_yn
		l_outJson_yn
		l_outDot_yn
		l_dotTheme
		l_rankDir
    ;

  %let l_sTime=%SYSFUNC(TIME());

  /********** BEGIN -- Macro Parameter Validation **********/
  %if (%SUPERQ(p_inTxtFileList) EQ ) %then
  %do;/* Missing p_inTxtFileList value in macro call */
    %let l_rc  = 1;
    %let l_msg = ERROR: util_scaText2graphvizDot: p_inTxtFileList is invalid. Please specify non-missing value;
    %goto exit;
  %end;
	%else %if (%sysfunc(fileexist(&p_inTxtFileList)) = 0) %then 
	%do;
    %let l_rc  = 2;
    %let l_msg = ERROR: util_scaText2graphvizDot: The external file &p_inTxtFileList does not exist;
    %goto exit;
	%end;

	%if (%SUPERQ(p_outDotFilesDir) EQ ) %then
	%do;/* Missing p_outDotFilesDir value in macro call */
		%let l_rc  = 3;
		%let l_msg = ERROR: util_scaText2graphvizDot: p_outDotFilesDir is invalid. Please specify non-missing value;
		%goto exit;
	%end;
	%else %if (%sysfunc(fileexist(&p_outDotFilesDir)) = 0) %then 
	%do;
		%let l_rc  = 4;
		%let l_msg = ERROR: util_scaText2graphvizDot: The external directory &p_outDotFilesDir does not exist;
		%goto exit;
	%end;
  /********** END -- Macro Parameter Validation **********/

	%let l_includeAttrs_yn = Y;
	%if (%superq(p_includeAttrs_yn) NE ) %then
		%let l_includeAttrs_yn = %upcase(&p_includeAttrs_yn);

	%let l_includeSteps_yn = Y;
	%if (%superq(p_includeSteps_yn) NE ) %then
		%let l_includeSteps_yn = %upcase(&p_includeSteps_yn);

	%let l_outJson_yn = N;
	%if (%superq(p_outJson_yn) NE ) %then
		%let l_outJson_yn = %upcase(&p_outJson_yn);

	%let l_dotTheme = DEFAULT;
	%if (%superq(p_dotTheme) NE ) %then
		%let l_dotTheme = %upcase(&p_dotTheme);

	%let l_outDot_yn = Y;
	%if (%superq(p_outDot_yn) NE ) %then
		%let l_outDot_yn = %upcase(&p_outDot_yn);
	%if (&l_outDot_yn NE Y AND &l_outJson_yn NE Y) %then
	%do;
		%let l_rc  = 5;
		%let l_msg = ERROR: util_scaText2graphvizDot: p_outDot_yn=N requires p_outJson_yn=Y (nothing would be written);
		%goto exit;
	%end;

	%let l_rankDir = TB;
	%if (%superq(p_rankDir) NE ) %then
		%let l_rankDir = %upcase(&p_rankDir);
	%if NOT (&l_rankDir IN (TB BT LR RL)) %then
	%do;
		%PUT WARNING: Acceptable p_rankDir values: TB|BT|LR|RL. Defaulting to: TB;
		%let l_rankDir = TB;
	%end;

	/* Set the direction to LR when the columns attributes in included */
	%if (&l_includeAttrs_yn = Y) %then
		%let l_rankDir = LR;

	FILENAME tmplstng TEMP;

	DATA _NULL_;
		LENGTH
			LINE $1000
			FILE_NAME $256
			NEW_NAME $256
			JSON_NAME $256
			DS_NAME $32;

		FILE tmplstng LRECL=2000;
		INFILE "&p_inTxtFileList" LRECL=1000;

		INPUT; /* Read a line */
    IF MISSING(_INFILE_) EQ 0; /* Only process none-blank lines */

    LINE =TRANSLATE(STRIP(COMPBL(_INFILE_)),'/','\'); /* Remove extra Spaces, and convert \ to / */
    FILE_NAME = SCAN(LINE,-1,'/');
    /* Swap only the final extension (names may contain other dots or the text 'txt') */
		_pos = FINDC(FILE_NAME,'.','b');
		if (_pos > 1) then NEW_NAME = CATS(SUBSTR(FILE_NAME,1,_pos-1),'.dot');
		else NEW_NAME = CATS(FILE_NAME,'.dot');
		DS_NAME = CATS('work.dotfile_',PUT(_n_,z4.));
		JSON_NAME = CATS(SUBSTR(NEW_NAME,1,LENGTHN(STRIP(NEW_NAME))-3),'json');

		/* Generate the required macro call dynamically; %nrstr protects paths containing , & % ( ) */
		PUT '%util_parseScaproc(p_inScaprocFileName=%nrstr(' LINE +(-1) ')';
		PUT ", p_outDotFilePath=%nrstr(&p_outDotFilesDir)";
		PUT ', p_outDotFileName=%nrstr(' NEW_NAME +(-1) ')';
		PUT ', p_outDsName=' DS_NAME;
		PUT ", p_includeAttrs_yn=&l_includeAttrs_yn, p_outDot_yn=&l_outDot_yn, p_dotTheme=&l_dotTheme";
	%if (&l_outJson_yn = Y) %then
	%do;
		PUT ", p_includeSteps_yn=&l_includeSteps_yn, p_rankDir=&l_rankDir";
		PUT ', p_outJsonFileName=%nrstr(' JSON_NAME +(-1) '))';
	%end;
	%else
	%do;
		PUT ", p_includeSteps_yn=&l_includeSteps_yn, p_rankDir=&l_rankDir)";
	%end;
	RUN;

	/* Execute the dynamically generated macro calls */
	%include tmplstng;
	FILENAME tmplstng CLEAR;

	/* The per-file parse results are not needed after the .dot files are written */
	PROC DATASETS LIB=WORK NOLIST NOWARN;
		DELETE dotfile_:;
	RUN; QUIT;

	%GOTO finished;

  %exit:
    %put *** ERROR: util_scaText2graphvizDot  ***;
    %put *** l_RC must be zero (0).   ***;
    %put *** l_RC= &l_RC    .         ***;
    %put *** &l_MSG ***;
    %put *** ERROR: util_scaText2graphvizDot  ***;

	%finished:
	  %let l_eTime=%SYSFUNC(TIME());
	  %let l_rTime=%SYSFUNC(PUTN(%SYSEVALF(&l_eTime - &l_sTime),time12.2));
	  %put >>> util_scaText2graphvizDot :>>> Total RunTime = &l_rTime;

%mend util_scaText2graphvizDot;