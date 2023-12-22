/**
  @file util_termscaproc.sas
  @brief Writes out the collected information to the record file.
  @details Writes out the collected information to the record file, 
  and terminate the SAS Code Analyzer. 

  The inspiration for this code originted from code developed 
  by Philip Mason (phil@woodstreet.org.uk) and presented in
  https://support.sas.com/resources/papers/proceedings17/1104-2017.pdf
 
      Usage:
      %util_termScaproc(p_enableFlagName=g_enableScaProc)

  @param [in] p_enableFlagName= The name of the macro variable flag to enable SAS Code Analysis logging.
 
  @version 9.4
  @author Ahmed Al-Attar

**/

%MACRO util_termScaproc(p_enableFlagName=g_enableScaProc) / minoperator;

	/**
	 * if specified p_enableFlagName macro exists and is set to 1 or Y or VERBOSE, then continue 
	*/
	%if (&p_enableFlagName = ) %then %return;
	%else %if (%symexist(&p_enableFlagName) = 0) %then %return;
	%if (NOT (%upcase(&&&p_enableFlagName) IN (1 Y VERBOSE))) %then %return;

	* Write out the collected information to the record file;
	PROC SCAPROC; WRITE; RUN;

%MEND util_termScaproc;