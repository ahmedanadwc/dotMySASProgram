/**
  @file usage_util_parsescaproc.sas
  @brief Illustrates how to use the util_parseScaproc macro

  <h4> SAS Macros </h4>
	@li util_parsescaproc.sas

  <h4> Related Programs </h4>
 
  @version SAS 9.4
  @author Ahmed Al-Attar

**/

%util_parseScaproc(p_inScaprocFileName=%str(&g_logsRoot/sca__analyze.txt)
, p_outDotFilePath=%str(&g_outputRoot)
, p_outDotFileName=analyze.dot
, p_outDsName=work.analyze
, p_includeAttrs_yn=Y, p_includeSteps_yn=Y, p_rankDir=LR)


%util_parseScaproc(p_inScaprocFileName=%str(&g_logsRoot/sca__fuzzy.txt)
, p_outDotFilePath=%str(&g_outputRoot)
, p_outDotFileName=fuzzy.dot
, p_outDsName=work.fuzzy
, p_includeAttrs_yn=Y, p_includeSteps_yn=Y, p_rankDir=LR)

