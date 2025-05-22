Custom built SAS macro programs to trigger recording and extracting information about input, output, and the use of macro symbols from a SAS program while it is running.

To get started, assuming you already have a directory of all the artifacts in the repo, you'll need to

1. Update your settings by modifying the /config/setup.sas program, and populating the variable listed below, before running it in your SAS session
%LET g_projRootPath = ; *<---- Specify installation path. Do not include trailing slash!;

2. Update your SAS program(s) before running/submitting (it/them) by the adding 
* %util_initScaproc macro call at the beginning of the code
* %util_termScaproc maco call at the end of the code.

Look at [Drive:]\dotMySASProgram-main\programs\analyze_data.sas as an example program. 

3. Parse the generated Code Analysis text file using the %util_parseScaproc macro call
   Look at [Drive:]\dotMySASProgram-main\programs\util_parsescaproc_usage.sas as an example	program.
   
A successful Step #3 execution should generate a file with DOT language syntax, describing graphs textually, so it can be rendered as a graphical representation.

Open the generated DOT file in MS VS Code and use the DOT extension to draw the graph.
