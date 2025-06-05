/**
  @file util_initscaproc.sas
  @brief Initiates the SAS Code Analyzer.
  @details Initiates the SAS Code Analyzer and specifies the filename
  collecting/storing the output of the SAS Code Analyzer.

  The inspiration for this code originted from code developed
  by Philip Mason (phil@woodstreet.org.uk) and presented in
  https://support.sas.com/resources/papers/proceedings17/1104-2017.pdf

  Example usage:
  
      %util_initScaproc(p_enableFlagName=g_enableScaProc
      , p_outFilePath=%str(&g_logsPath)
      , p_outFileName=scaproc.txt
      , p_options=ATTR OPENTIMES EXPANDMACROS)

  @param [in] p_enableFlagName= The name of the macro variable flag to enable SAS Code Analysis logging.
  @param [in] p_outFilePath= The output path for storing the output recording file.
  @param [in] p_outFileName= The name of the output recording file.
  @param [in] p_options= SAS macro variable name to hold the returned value. Required.

  @version 9.4
  @author Ahmed Al-Attar

**/

%MACRO util_initScaproc(p_enableFlagName=g_enableScaProc
, p_outFilePath=%str(&g_logsPath)
, p_outFileName=scaproc.txt
, p_options=ATTR OPENTIMES EXPANDMACROS
) / minoperator;

	%LOCAL
		l_outFilePath
		l_outFileName
		;

	/**
	 * if specified p_enableFlagName macro exists and is set to 1 or Y or VERBOSE, then continue
	*/
	%if (&p_enableFlagName = ) %then %return;
	%else %if (%symexist(&p_enableFlagName) = 0) %then %return;
	%if (NOT (%upcase(&&&p_enableFlagName) IN (1 Y VERBOSE))) %then %return;

	/**
	 * Start the Enhanced SAS Code Analysis
	*/
	%let l_outFileName = %superq(p_outFileName);
	%if (%superq(l_outFileName)= )%then
		%let l_outFileName=&sysuserid._scaproc.txt;

	%let l_outFilePath = %superq(p_outFilePath);
	%if (%superq(l_outFilePath)= )%then
		%let l_outFilePath = %SYSFUNC(PATHNAME(WORK));

	* Run the SCAPROC procedure;
	PROC SCAPROC;
		RECORD %unquote("&l_outFilePath/&l_outFileName") &p_options;
	RUN;

%MEND util_initScaproc;