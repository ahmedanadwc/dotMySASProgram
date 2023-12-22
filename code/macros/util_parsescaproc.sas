/**
  @file util_parsescaproc.sas
  @brief Parses out the output generated SAS Code Analyzer.
  @details Parses out the output generated SAS Code Analyzer, 
  and generated *.dot output file, that could be rendered by any 
  Graphviz (https://www.graphviz.org/) Viewer/Renderer be it
    - Web Viewer: http://webgraphviz.com/
    - VS Code extension: Graphviz (dot) language support for Visual Studio Code
      (https://marketplace.visualstudio.com/items?itemName=joaompinto.vscode-graphviz)

  The inspiration for this code originted from  
    - Code presented in
      "Automatically create diagrams showing the structure and performance of your SAS code"
      https://support.sas.com/resources/papers/proceedings17/1104-2017.pdf
      https://github.com/philipmason/Wood-Street-Consultants/tree/main/SCAPROC/SAS
    - Code presented in
      "SCAPROC Impact Analysis - Providing QA and Compliance Insights"
      https://www.lexjansen.com/phuse/2016/tt/TT02.pdf
      Sample 58878: SAS® Life Science Analytics Framework - 
      SCAPROC Impact Analysis: Providing QA and Compliance Insights
      https://support.sas.com/kb/58/878.html
      https://support.sas.com/kb/58/047.html
    - Code presented in
      "QA and Compliance Insights Using the SCAPROC Procedure"
      https://www.pharmasug.org/proceedings/2016/MS/PharmaSUG-2016-MS04.pdf
    - SASjs generated lineage documentation
      https://cli.sasjs.io/doc/
      https://www.youtube.com/watch?v=Sb1DqTLgVyQ&t=182s
      https://www.youtube.com/watch?v=9ea9vn7YmfM
 
      Usage:
      %util_parseScaproc(p_inScaprocFileName=
      , p_outDotFilePath=%sysfunc(pathname(WORK))
      , p_outDotFileName=mydigraph.dot
      , p_outDsName=work.mydigraph
      , p_includeAttrs_yn=Y, p_includeSteps_yn=Y)

  @param [in] p_inScaprocFileName= The name of the SAS Code Analysis recording file. Required.
  @param [in] p_outDotFilePath= The GRAPHVIZ (*.dot) output file path. Required. 
                                Default: %sysfunc(pathname(WORK)).
  @param [in] p_outDotFileName= The GRAPHVIZ (*.dot) output file name. Required.
                                Default: mydigraph.dot
  @param [in] p_outDsName= The output data set name of the parsing results. Optional.
  @param [in] p_includeAttrs_yn= A Y/N flag to include table columns in the output. Default:Y.
  @param [in] p_includeSteps_yn= A Y/N flag to include processing steps and elapse times in the output. 
                                 Default:Y.
  @param [in] p_rankDir= Sets direction of graph layout. Valid Values:TB|BT|LR|RL.
                         Default:TB.

  @version 9.4
  @author Ahmed Al-Attar

**/

%MACRO util_parseScaproc(p_inScaprocFileName=
, p_outDotFilePath=%sysfunc(pathname(WORK))
, p_outDotFileName=mydigraph.dot
, p_outDsName=
, p_includeAttrs_yn=Y
, p_includeSteps_yn=Y
, p_rankDir=TB) / minoperator;

  %LOCAL
		l_eTime
    l_MSG
    l_RC
    l_rTime
    l_sTime
		;

	%LOCAL
		l_inScaprocFileName
		l_scaprocFileName
		l_scaprocFolderName
		l_outDotFilePath
		l_outDotFileName
		l_outDsName
		l_maxStep
		l_stepFmt
		l_includeAttrs_yn
		l_includeSteps_yn
		l_rankDir
		;

	%let l_sTime=%sysfunc(time());

	/********** BEGIN -- Macro Parameter Validation **********/
	/* Check if the p_inScaprocFileName is specified or not */
	%if (%superq(p_inScaprocFileName) EQ ) %then 
	%do;
		%let l_rc  = 1;
    %let l_msg = ERROR: util_parseScaproc: p_inScaprocFileName is invalid. Please specify non-missing value;
    %goto exit;
	%end;
	%else 
	%do;
		/* Convert \ to / for supplied parameter paths */
		%let l_inScaprocFileName = %sysfunc(translate(&p_inScaprocFileName,%str(/),%str(\)));

		%if (%sysfunc(fileexist(&l_inScaprocFileName)) EQ 1) %then 
		%do;
			%let l_scaprocFileName = %scan(&l_inScaprocFileName,-1,%str(/));
			%let l_scaprocFolderName =%sysfunc(prxchange(s/&l_scaprocFileName//,-1,&l_inScaprocFileName));
			%put &=l_scaprocFileName &=l_scaprocFolderName;
		%end;
		%else 
		%do;
			%let l_rc  = 2;
			%let l_msg = ERROR: util_parseScaproc: p_inScaprocFileName is invalid. Please specify an existing file;
			%goto exit;
		%end;
	%end;

	/* Set Defaults */
	%let l_outDotFilePath = &p_outDotFilePath;
	%if (%superq(l_outDotFilePath) EQ ) %then
		%let l_outDotFilePath = %sysfunc(pathname(WORK));
	%else
		%let l_outDotFilePath = %sysfunc(translate(&l_outDotFilePath,%str(/),%str(\)));

	%let l_outDotFileName = &p_outDotFileName;
	%if (%superq(l_outDotFileName) EQ ) %then
		%let l_outDotFileName = mydigraph.dot;

	%let l_outDsName = &p_outDsName;
	%if (%superq(l_outDsName) EQ ) %then
		%let l_outDsName=work.mydigraph;

	%let l_includeAttrs_yn = Y;
	%if (%superq(p_includeAttrs_yn) NE ) %then
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

	/********** END -- Macro Parameter Validation **********/

	/* Read the */
	DATA work.scadata;
		INFILE "&l_inScaprocFileName" LRECL=1000 LENGTH=linelength truncover END=eof;
		LENGTH scaline $1000 linenum step 5;
		INPUT @;
		INPUT scaline $varying1000. linelength;
		linenum+1;
		if (_n_=1 OR index(scaline,'STEP SOURCE FOLLOWS')) then step+1 ;
	RUN;

  /* Ignoring SASTemp outputs and Global Macro Variables */
	DATA work.scadata1;
		SET work.scadata(WHERE=((index(scaline,"JOBSPLIT") > 0) 
		and (index(scaline,"SYMBOL")=0)
		and (index(scaline,"CATALOG")=0)));
	RUN;

	/* Find Concatenated Libraries */
	DATA work.concatmem;
		set work.scadata1;
		LENGTH libname concatmem $8;
		where index(scaline, "/* JOBSPLIT: CONCATMEM ")=1;
		linenum = max(1, linenum -2);
		concatmem = put(scan(scaline, 4, " "), $20.);
		libname = put(scan(scaline, 5, " "), $20.);
	RUN;

	/* Process the lines and extract data set columns/attributes */
	DATA
		work.scadata2(DROP=word1-word2 word4-word7)
	%if (&l_includeAttrs_yn EQ Y) %then
	%do;
		work.attrs(KEEP=word4-word7 lib_dsname linenum step RENAME=(word4=in_out))
	%end;
		;

		LENGTH 
			word1-word7 $50 
			concatmem libname $8 
			dsname $32
			lib_dsname $41
			;

		if (_n_=1) then
		do;
			dcl hash h_ds();
			h_ds.defineKey('word5');
			h_ds.defineData('lib_dsname');
			h_ds.defineDone(); 
		end;

		MERGE	
			scadata1(in=a)
			concatmem(in=b KEEP=linenum concatmem libname 
				RENAME=(concatmem=_concatmem libname=_libname))
		;
		BY linenum;
		if a;

		if (b) then 
		do;
			concatmem=_concatmem;
			libname=_libname;
		end;

		if scaline=:'/* JOBSPLIT: ' ;
		word1=scan(scaline,2,' ') ;
		word2=scan(scaline,3,' ') ;
		word3=scan(scaline,4,' ') ;
		word4=scan(scaline,5,' ') ;
		word5=scan(scaline,6,' ') ;
		word6=scan(scaline,7,' ') ;
		word7=scan(scaline,8,' ') ;

		if (word2 in ('DATASET','ATTR')) then
		do;
			* exclude utility files;
			if (word2 = 'DATASET') then
			do;
				if (index(word5,'UTILITY')=0) then
				do;
					dsname = scan(word5,2,'.');
					if (libname ='') then libname = scan(word5,1,'.');

					lib_dsname = catx('.',libname,dsname);

					/* track unique lib_dsname values */
					h_ds.ref();
				end; /* (index(word5,'UTILITY')=0) */
				else delete;
			end; /* (word2 = 'DATASET') */
			else
			do; /* (word2 = 'ATTR') */
				%if (&l_includeAttrs_yn EQ Y) %then
				%do;
				if (h_ds.find(key:word3) = 0) then OUTPUT work.attrs;
				%end;
			end; /* (word2 = 'ATTR') */
		end;

		if ( (concatmem ^= " ") 
			AND (index(scaline, strip(concatmem))) ) then 
		do;
		  concatmemfl="Y";
		  word5=tranwrd(word5, strip(concatmem), strip(libname));
		end;

		if (word2='DATASET' & word3='INPUT')  then in=strip(word4)||'~'||scan(word5,1,'.')||'.'||scan(word5,2,'.') ;
		if (word2='DATASET' & word3='OUTPUT') then out=strip(word4)||'~'||scan(word5,1,'.')||'.'||scan(word5,2,'.') ;
		if (word2='DATASET' & word3='UPDATE') then out=strip(word4)||'~'||scan(word5,1,'.')||'.'||scan(word5,2,'.') ;
		if (word2='PROCNAME')                 then procname=word3 ;
		if (word2='ELAPSED')                  then elapsed=input(word3,8.3) ;
		if (word2='FILE' & word3='INPUT')     then in=strip(word4) ;
		if (word2='FILE' & word3='OUTPUT')    then out=strip(word4) ;

		OUTPUT work.scadata2;
		DROP _concatmem _libname;
	RUN;

	/* Clean-up */
	PROC DELETE DATA=work.concatmem work.scadata1 work.scadata; RUN;

	PROC SQL NOPRINT ;
		/* Find the Max Step count */
		SELECT MAX(step)
			, CATS('Z',ceil(log10(MAX(step))),'.')
		INTO :l_maxStep TRIMMED
			, :l_stepFmt
		FROM  work.scadata2
		;

		/* Merge the data into one record for each step */
		CREATE TABLE work.flow AS
		SELECT DISTINCT COALESCE(a.step,b.step,c.step) AS step
			,"_"||put(coalesce(a.step,b.step,c.step),&l_stepFmt.)||"_"||strip(a.procname) as procname
			,COALESCE(scan(b.in,1,'~'),scan(c.in,1,'~')) as in_access
			,coalesce(scan(b.out,1,'~'),scan(c.out,1,'~')) as out_access
			,coalesce(scan(b.in,2,'~'),scan(c.in,2,'~')) as in
			,coalesce(scan(b.out,2,'~'),scan(c.out,2,'~')) as out
			,d.elapsed
		FROM work.scadata2(where=(procname ^= '')) as a
		FULL JOIN
			work.scadata2(where=(in ^= '')) as b ON a.step=b.step
		FULL JOIN 
			work.scadata2(where=(out ^= '')) as c ON a.step=c.step 
		LEFT JOIN
			work.scadata2(where=(elapsed>0)) as d ON a.step=d.step 
		ORDER BY CALCULATED step
	  ;
	QUIT;

	/* Ensure unique flow steps */
	PROC SORT DATA=WORK.flow NODUPKEY;
		BY step procname in_access in out;
	RUN;

	* ---------------;
	* Create .DOT directives to make a diagram ;
	* ---------------;
	* First: Produce header lines;
	DATA &l_outDsName ;
		LENGTH line $ 300;

	  line="// Generated by SAS for &l_inScaprocFileName" ;
	  OUTPUT ;
	  line='digraph dag {' ;
	  OUTPUT ;
		line="rankdir=&l_rankDir";
		OUTPUT;
	  /*line="graph [label=""\n\n&l_inScaprocFileName\n%sysfunc(datetime(),datetime.)""]" ;*/
		line="graph [label=""\n&l_inScaprocFileName""]" ;
	  OUTPUT ;
	  line='node [shape=plaintext color=lightblue style=filled]' ;
	  OUTPUT ;
	RUN;

	%if (&l_includeAttrs_yn EQ Y) %then
	%do;
		/* Next: Generate Entity/Table definitions lines */
		DATA work.attrs_lines(KEEP=line);
			LENGTH line $300; 

			DO UNTIL (last.in_out);
				SET work.attrs;
				BY step lib_dsname in_out NOTSORTED;

				/* Output Table name */
				if (first.in_out) then 
				do;
					line =QUOTE(STRIP(lib_dsname))|| ' [label=<<TABLE BORDER="0" CELLSPACING="0"><TR><TD BORDER="0" COLSPAN="2" CELLPADDING="0">' 
								||'<FONT POINT-SIZE="12">'||STRIP(lib_dsname)|| '</FONT></TD></TR>' ;
					OUTPUT;
				end;
				line ='<TR><TD BORDER="0" ALIGN="LEFT"><FONT POINT-SIZE="10">'||STRIP(SCAN(word5,2,':'))
							||'</FONT></TD><TD BORDER="0" ALIGN="RIGHT"><FONT POINT-SIZE="10">'||IFC(scan(word6,2,':')='CHARACTER','char','num')
							||' ('||STRIP(scan(word7,2,':'))||')</FONT></TD></TR>' ;
				OUTPUT;
			END;
			/* Close out the node definition */
			line = '</TABLE>>];';
			OUTPUT;
		RUN;
		PROC APPEND BASE=&l_outDsName DATA=work.attrs_lines FORCE; RUN;

		/* Clean-up */
		PROC DELETE DATA=work.attrs work.attrs_lines; RUN;
	%end;

	/* Next: Generate flow lines */
	DATA work.flow_lines (keep=line) ;
		LENGTH line $300 style $12;

		SET work.flow end=end ;

		in=quote(strip(in)) ;
		out=quote(strip(out)) ;
		procname=quote(strip(procname)) ;
		style='style=solid ' ;

		%if (&l_includeSteps_yn = Y) %then
		%do;
		if ( (compress(in,'"')>'') & (compress(out,'"')>'') ) then
			line=strip(in)||'->'||strip(out)||
					 ' [label=" '||lowcase(strip(dequote(procname)))||
					 ' ('||strip(put(elapsed,8.3))||
					 ')" '||strip(style)||'];' ;
		else if ( (compress(in,'"')>'') & (compress(out,'"')='') ) then
			line=strip(in)||'->'||strip(procname)||
	         ' [label="('||strip(put(elapsed,8.3))||
	         ')" '||strip(style)||'];' ;
		else if ( (compress(in,'"')='') & (compress(out,'"')>'') ) then
			line=strip(procname)||'->'||strip(out)||
			' [label="('||strip(put(elapsed,8.3))||
			')" '||strip(style)||'];' ;
		else line='// '||strip(procname)||' ('||strip(put(elapsed,8.3))||')' ;
		%end;
		%else
		%do;
		if ( ((compress(in,'"')>'') & (compress(out,'"')>'')) 
				AND (strip(in) NE strip(out)) 
			 ) then
			line=strip(in)||'->'||strip(out)||';';
		%end;
		if (strip(line) ne '') then OUTPUT ;

		if (end) then 
		do ;
			line='}' ;
			OUTPUT ;
		end ;
	RUN ;

	DATA _NULL_;
		FILE "&l_outDotFilePath/&l_outDotFileName";
		SET &l_outDsName work.flow_lines;
		PUT line;
	RUN;

	%if (%superq(p_outDsName) NE ) %then
	%do;
		PROC APPEND BASE=&l_outDsName DATA=work.flow_lines FORCE; RUN;
	%end;

	/* Clean-up */
	PROC DELETE DATA=work.flow work.flow_lines; RUN;

	%goto finished;

  %exit:
		%PUT *** ERROR: util_parseScaproc  ***;
		%PUT *** l_RC must be zero (0).   ***;
		%PUT *** l_RC= &l_RC    .         ***;
		%PUT *** &l_MSG ***;
		%PUT *** ERROR: util_parseScaproc  ***;

	%finished:
		%let l_eTime=%sysfunc(time());
		%let l_rTime=%sysfunc(putn(%sysevalf(&l_eTime - &l_sTime),time12.2));
		%PUT >>> util_parseScaproc :>>> Total RunTime = &l_rTime;

%MEND util_parseScaproc;
