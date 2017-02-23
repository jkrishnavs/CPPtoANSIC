# CPPtoANSIC
a rose translator to convert cpp codes to ANSI C code. All classes will be converted to C style structs with no member functions.

cpptoansic.C can replace the defaultTranslator.C in ROSE directory.

// If trying to use 
This file has been specifically done for GREENMARL graph algorithms (https://github.com/stanford-ppl/Green-Marl).
for generic translator comment #define GREENMARL_CONVERSION


cpptoansic -rose:Cxx11 -rose:skipfinalCompileStep  $1 $2
where $1 is your cpp file name and $2 is your start function. 

if not using greemarl translator skip $2.

for Queries mail jkrishnavs@gmail.com