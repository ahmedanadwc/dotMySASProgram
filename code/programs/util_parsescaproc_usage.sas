
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

