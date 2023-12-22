
/* ----------------------------- */
/* Define global macro variables */
/* ----------------------------- */
%GLOBAL
	g_projRootPath  /* Location of EG Project Root/Parent dir */
	g_enableScaProc
	;

%LET g_projRootPath = <Put your value here>;	*<---- Specify installation path. Do not include trailing slash!;

/* ========== Do not modify below this line ========== */

/* ----------------------------- */
/* Define global macro variables */
/* ----------------------------- */
%GLOBAL
	g_slash			/* OS Specific slash representation */
	g_sasauto		/* OS Sepecific SASAUTOS representation */
	g_computeHostName	/* Host name running this SAS session */
	g_sasCodeRoot   /* Location of custom SAS Code files  */
	g_sasProgramRoot /* Location of custom SAS Program files  */
	g_sasMacroRoot  /* Location of custom SAS Macro Files */
	g_logsRoot  	/* Location of logs Files */
	;
/* ------------------------------- */
/* Host and Environment Parameters */
/* ------------------------------- */

/* Declare inner utility macro to Assign host OS specific macro
   variables */
%macro inner_hostMacros;

	%if (&SYSSCP = WIN) %then
	%do; /* Windows Operating System */
		%let g_slash   = %str(\);
		%let g_sasauto = SASAUTOS;
	%end; /* Windows Operating System */
	%else
	%do; /* Otherwise assume it's UNIX */
		%let g_slash   = %str(/);
		%let g_sasauto = "!SASROOT/sasautos";
	%end; /* Otherwise assume it's UNIX */
	%let g_computeHostName = &SYSTCPIPHOSTNAME;

%mend inner_hostMacros;

/* Execute inner utility macro */
%inner_hostMacros;

%let g_sasCodeRoot     = &g_projRootPath.&g_slash.code;
%let g_sasProgramRoot  = &g_sasCodeRoot.&g_slash.programs;
%let g_sasMacroRoot    = &g_sasCodeRoot.&g_slash.macros;
%let g_outputRoot      = &g_projRootPath.&g_slash.output;
%let g_logsRoot        = &g_projRootPath.&g_slash.logs;

OPTIONS 
	SASAUTOS = (&g_sasauto , "&g_sasMacroRoot")
	MAUTOSOURCE ;

OPTIONS 
	FULLSTIMER /* Writes all available system performance statistics to the SAS log */
	SORTEQUALS /* Controls how PROC SORT orders observations with identical BY values in the output data set */
	OBS=MAX    /* Specifies when to stop processing observations or records */
	MSGLEVEL=I /* Controls the level of detail in messages that are written to the SAS log */
	SPOOL      /* Controls whether SAS writes SAS statements to a utility data set in the WORK data library */
	NOSYNTAXCHECK	/* Does not enable syntax check mode for statements that are submitted within a */
					/* non-interactive or batch SAS session.       */
	COMAMID=TCP	/* Identifies the communications access method for connecting a client and a server across */
				/* a network */
	AUTOSIGNON	/* Automatically signs on to the server when the client issues a remote submit request */
				/* for server processing.                      */
	VALIDVARNAME=ANY  /* Allows the use of column names that contain embedded spaces and special characters */
	NOTES
	/*SOURCE
	SOURCE2*/
	NOMPRINT
	DEBUG=DBMS_SELECT	/* shows only the select statements generated */
	sastrace=',,t,dsab' /* Display a summary of all generated SQL code, any threaded read/write operations, */
						/* and timing summary & details. */
	sastraceloc=saslog 
	nostsuffix
	DLCREATEDIR /* Specifies to create a directory for the SAS library that is named in a LIBNAME statement */
				/* if the directory does not exist. */
	;

/*proc options option=sasautos; run;*/
LIBNAME outlib "&g_outputRoot";

%let g_enableScaProc = 1;

