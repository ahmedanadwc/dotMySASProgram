/**
  @file usage_util_scatext2graphvizdot.sas
  @brief Illustrates how to utilize the %util_initScaproc & %util_termScaproc macros

  <h4> SAS Macros </h4>
  @li util_scatext2graphvizdot.sas

  <h4> Related Programs </h4>
 
  @version SAS 9.4
  @author Ahmed Al-Attar

**/

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

/* --- Get a list of existing sca output text files --- */
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


/**
 * --- Parse the text files into GraphVis *.dot files --- 
*/

/* --- Without Column Attributes --- */
%util_scaText2graphvizDot (p_inTxtFileList=&g_txtFileList
, p_outDotFilesDir=%str(~/dotMySASProgram/output/dotfiles_wo_attrs)
, p_includeAttrs_yn=N
, p_includeSteps_yn=Y
, p_rankDir=LR)

/* --- With Column Attributes --- */
%util_scaText2graphvizDot (p_inTxtFileList=&g_txtFileList
, p_outDotFilesDir=%str(~/dotMySASProgram/output/dotfiles_w_attrs)
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=LR)
