/**
  @file usage_util_scatext2graphvizdot.sas
  @brief Illustrates how to utilize the %util_scaText2graphvizDot macro

  <h4> SAS Macros </h4>
  @li util_scatext2graphvizdot.sas

  <h4> Related Programs </h4>
 
  @version SAS 9.4
  @author Ahmed Al-Attar

**/

/* Declare few global macro variables */
%GLOBAL
	g_projectRoot
	g_logsRoot
	g_outputRoot
	g_txtFileNamePattern
	g_txtFilesDir
	g_txtFileList
	g_outDotFilesDir
	;

/**
 * Paths are derived from one root. Set g_projectRoot (or g_logsRoot/g_outputRoot)
 * before running this program to relocate everything; the default is
 * <home directory>/dotMySASProgram, which works on Linux and Windows.
*/
%macro usage_setPaths;
	%if (%superq(g_projectRoot) EQ ) %then
		%let g_projectRoot = %sysfunc(translate(%sysfunc(sysget(HOME))/dotMySASProgram,/,\));
	%if (%superq(g_logsRoot) EQ )   %then %let g_logsRoot   = &g_projectRoot/logs;
	%if (%superq(g_outputRoot) EQ ) %then %let g_outputRoot = &g_projectRoot/output;
%mend usage_setPaths;
%usage_setPaths

%let g_txtFileNamePattern = %str(sca__*.txt);
%let g_txtFilesDir    = &g_logsRoot;
%let g_txtFileList    = &g_outputRoot/sca_text_files.txt;
%let g_outDotFilesDir = &g_outputRoot;

/* --- Get a list of existing sca output text files --- */
FILENAME thelist "&g_txtFileList";

DATA _NULL_;
	FILE thelist lrecl=1000;
	keep filename;
	length fref $8 filename $256;
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
		if (fid > 0) then rc=fclose(fid);

		if ((fid > 0) AND (substr(filename,1,5) = 'sca__' ) AND (scan(filename,-1,'.') = 'txt' )) then
		do;
			filename = CATX('/',"&g_txtFilesDir",filename);
			PUT filename;
		end;
	end;
	rc=dclose(did);
RUN;


/**
 * --- Parse the text files into GraphVis *.dot files --- 
*/

/* Create the output folders (the macro requires them to exist) */
%let g_rc = %sysfunc(dcreate(dotfiles_wo_attrs,&g_outputRoot));
%let g_rc = %sysfunc(dcreate(dotfiles_w_attrs,&g_outputRoot));

/* --- Without Column Attributes --- */
%util_scaText2graphvizDot (p_inTxtFileList=&g_txtFileList
, p_outDotFilesDir=%str(&g_outputRoot/dotfiles_wo_attrs)
, p_includeAttrs_yn=N
, p_includeSteps_yn=Y
, p_rankDir=LR)

/* --- With Column Attributes --- */
%util_scaText2graphvizDot (p_inTxtFileList=&g_txtFileList
, p_outDotFilesDir=%str(&g_outputRoot/dotfiles_w_attrs)
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=LR)
