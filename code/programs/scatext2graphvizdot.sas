/* Declare few global macro variables */
%GLOBAL
	g_txtFileNamePattern
	g_txtFilesDir
	g_txtFileList
	g_outDotFilesDir
	g_listingCmd
	;
 
%let g_txtFileNamePattern = %str(sca__*.txt);

/* ------- Linux ------- */
%let g_txtFilesDir    = ~/dotMySASProgram/logs;
%let g_txtFileList    = ~/dotMySASProgram/output/sca_text_files.txt;
%let g_outDotFilesDir = /tmp/dotfilesdir;
%let g_listingCmd     = %str(ls -1 &g_txtFilesDir/&g_txtFileNamePattern);
/* ------- Linux ------- */

/* ------- Windows ------- */
/*
%let g_txtFilesDir    = [drive:]\<Project\path>;
%let g_txtFileList    = c:\temp\sca_text_files.txt.txt; 
%let g_outDotFilesDir = c:\temp\dotfilesdir;
%let g_listingCmd     = %str(dir /s /b /a &g_txtFilesDir\&g_txtFileNamePattern);
*/
/* ------- Windows ------- */

* Get a list of existing sca output text files;
/* %sysexec %str(&g_listingCmd > &g_txtFileList); */


FILENAME thelist "&g_txtFileList";

DATA _NULL_;
	FILE thelist lrecl=80;
	keep filename;
	length fref $8 filename $80;
	rc=filename(fref, "&g_txtFilesDir");

	if rc=0 then
	do;
		did=dopen(fref);
		rc=filename(fref);
	end;
	else
	do;
		length msg $200.;
		msg=sysmsg();
		put msg=;
		did=.;
	end;

	if did <=0 then
		putlog 'ERR' 'OR: Unable to open directory.';
	dnum=dnum(did);

	do i=1 to dnum;
		filename=dread(did, i);
	
		/* If this entry is a file, then output. */
		fid=mopen(did, filename);
	
		if ((fid > 0) AND (substr(filename,1,5) = 'sca__' ) AND (scan(filename,-1,'.') = 'txt' )) then
		do;
			filename = CATX('/',"&g_txtFilesDir",filename);
			PUT filename;
		end;
	end;
	rc=dclose(did);
RUN;

/* Declare a macro to process the listed *.txt files*/
%macro util_scaText2graphvizDot (p_inTxtFileList=
, p_outDotFilesDir=
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=TB) / minoperator;
  
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
	%if (&p_includeAttrs_yn NE ) %then
		%let l_includeAttrs_yn = %upcase(&p_includeAttrs_yn);

	%let l_includeSteps_yn = Y;
	%if (%superq(p_includeSteps_yn) NE ) %then
		%let l_includeSteps_yn = %upcase(&p_includeSteps_yn);

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
			LINE $400
			FILE_NAME $100
			NEW_NAME $100
			FILE_TYPE $10
			DS_NAME $41;

		FILE tmplstng LRECL=400;
		INFILE "&p_inTxtFileList" LRECL=400;

		if (_N_=1) then
			PUT "LIBNAME _dotLib_ '&p_outDotFilesDir'; ";

		INPUT; /* Read a line */
    IF MISSING(_INFILE_) EQ 0; /* Only process none-blank lines */

    LINE =TRANSLATE(STRIP(COMPBL(_INFILE_)),'/','\'); /* Remove extra Spaces, and convert \ to / */
    FILE_NAME = SCAN(LINE,-1,'/');
    FILE_TYPE = SCAN(FILE_NAME,2,'.');
		NEW_NAME  = TRANWRD(STRIP(FILE_NAME),STRIP(FILE_TYPE),'dot');
		DS_NAME = CATS('_dotlib_.dotfile_',PUT(_n_,z4.));

		/* Generate the required macro call dynamically */
		PUT '%util_parseScaproc(p_inScaprocFileName=' LINE ;
		PUT ", p_outDotFilePath=%str(&p_outDotFilesDir)";
		PUT ', p_outDotFileName=' NEW_NAME ;
		PUT ', p_outDsName=' DS_NAME;
		PUT ", p_includeAttrs_yn=&l_includeAttrs_yn";
		PUT ", p_includeSteps_yn=&l_includeSteps_yn, p_rankDir=&l_rankDir)";
	RUN;

	/* Execute the dynamically generated macro calls */
	%include tmplstng;

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

%util_scaText2graphvizDot (p_inTxtFileList=&g_txtFileList
, p_outDotFilesDir=%str(~/dotMySASProgram/output/dotfiles_wo_attrs)
, p_includeAttrs_yn=N
, p_includeSteps_yn=Y
, p_rankDir=LR)

%util_scaText2graphvizDot (p_inTxtFileList=&g_txtFileList
, p_outDotFilesDir=%str(~/dotMySASProgram/output/dotfiles_w_attrs)
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=LR)
