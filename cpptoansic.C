#include "rose.h"
#include<cstdlib>
#include<iostream>
#include<fstream>

using namespace OmpSupport;
using namespace SageBuilder;
using namespace SageInterface;

#define GREENMARL_CONVERSION
#define GM_GRAPH_NAME "class_gm_graph"
SgName thisName = "this";


std::string the_pragma_skip   = "skip 1 ";
std::string the_pragma_call   = "replace fnCall ";
std::string the_pragma_fnSign = "Fnsignature ";
std::string the_pragma_fnDef  = "skip fnDef "; 



/**
   This vector will contain the names of 
   some system defined functions which can be skipped so as to reduce the complexity of analysis.
 **/
std::string classesTobeSkipped[] = 
  {"vector < int32_t , allocator< int32_t > > ", 
   "__normal_iterator < pointer , vector< edge_dest_t ", 
   "allocator< edge_dest_t > > > ", 
   "vector < edge_dest_t , allocator< edge_dest_t > > ", 
   "_gm_sort_indices < node_t > ", 
   "_Iter_base < pointer , false > ", 
   "__normal_iterator < pointer , vector< edge_dest_t , allocator< edge_dest_t > > > ",
   "__copy_move_backward < true , true , iterator_category > ",
   "list < void * , allocator< void * > > ",
   "_List_iterator < void * > ",
   "list < gm_complex_data_type * , allocator< gm_complex_data_type * > > ",
   "_List_iterator < gm_complex_data_type * > ",
   "gm_map_medium < node_t , int32_t > ",
   "gm_map_medium < node_t , int32_t >  gm_map_medium < node_t , int32_t > ",
   "gm_dfs_template< false , true , true , false >  ",
   "gm_dfs_template< false , true , true , false > "
   
};

std::string functionsTobeSkipped[] = 
  {
    "operator!=",
    "sort < int32_t * , _gm_sort_indices< node_t > > ",
    "__introsort_loop < int32_t * , long , _gm_sort_indices< node_t > >",
    "__final_insertion_sort < int32_t * , _gm_sort_indices< node_t > >",
    "partial_sort < int32_t * , _gm_sort_indices< node_t > >",
    "__unguarded_partition_pivot < int32_t * , _gm_sort_indices< node_t > >",
    "__insertion_sort < int32_t * , _gm_sort_indices< node_t > >",
    "__unguarded_insertion_sort < int32_t * , _gm_sort_indices< node_t > >",
    "__heap_select < int32_t * , _gm_sort_indices< node_t > >",
    "sort_heap < int32_t * , _gm_sort_indices< node_t > >",
    "__move_median_to_first < int32_t * , _gm_sort_indices< node_t > >",
    "__unguarded_partition < int32_t * , int32_t , _gm_sort_indices< node_t > >",
    "move < value_type & >",
    "move_backward < pointer , pointer >",
    "__unguarded_linear_insert < int32_t * , _gm_sort_indices< node_t > >",
    "make_heap < int32_t * , _gm_sort_indices< node_t > >",
    "__pop_heap < int32_t * , _gm_sort_indices< node_t > >",
    "iter_swap < int32_t * , int32_t * >",
    "__copy_move_backward_a2 < true , iterator_type , pointer >",
    "__miter_base < pointer >",
    "__adjust_heap < int32_t * , difference_type , type , _gm_sort_indices< node_t > >",
    "swap < int32_t >",
    "__copy_move_backward_a < true , iterator_type , iterator_type >",
    "__niter_base < pointer >",
    "__push_heap < int32_t * , difference_type , type , _gm_sort_indices< node_t > >",
    "log < int > ",
    "changeValueAtomicAdd",
    "hasMaxValue_seq",
    "getMaxKey_seq",
    "prepare_seq_iteration",
    "has_next",
    "get_next",
    "ATOMIC_ADD < int32_t > ",
    "ATOMIC_ADD < int64_t > ",
    "ATOMIC_ADD < float > ",
    "ATOMIC_ADD < float  > ",
    "Seq_Iterator < iterator , node_t > _has_next",
    "Seq_Iterator < iterator , node_t > _get_next",
    "gm_seq_vec < node_t > _prepare_seq_iteration",
    "_List_iterator < void * > _operator!=",
    "_List_iterator < void * > _operator++",
    "_List_iterator < void * > _operator*",
    "list < void * , allocator< void * > > _clear",
    "list < gm_complex_data_type * , allocator< gm_complex_data_type * > > _begin",
    "_List_iterator < gm_complex_data_type * > _operator!=",
    "_List_iterator < gm_complex_data_type * > _operator++",
    "_List_iterator < gm_complex_data_type * > _operator*",
    "list < gm_complex_data_type * , allocator< gm_complex_data_type * > > _clear",
    "gm_map_medium < node_t , int32_t > _changeValueAtomicAdd",
    "gm_map_medium < node_t , int32_t > _hasMaxValue_seq",
    "gm_map_medium < node_t , int32_t > _getMaxKey_seq",
    "_gm_get_min < int32_t >",
    "ATOMIC_ADD < float > "
  };

int sizeofClassesTobeSkipped = 16;
int sizeofFunctionsTobeSkipped = 53;


bool skipTheFunction(SgName funName);
bool skipTheClass(SgName className);

/**
   These are class declarations which has to be converted to 
   structs.
 **/
struct FunctionClassDetails {
  SgFunctionDeclaration *dec;
  SgClassDeclaration* classSymbol;
  std::vector<SgFunctionCallExp*> callSites;


  FunctionClassDetails(SgFunctionDeclaration* d, SgFunctionCallExp* e, 
		       SgClassDeclaration* s) {
    dec = d;
    ROSE_ASSERT(dec != NULL);
    callSites.push_back(e);
    classSymbol = s;
  }


  SgFunctionDeclaration * getDec() {
    ROSE_ASSERT(dec != NULL);
    return dec;
  }


  bool found(SgFunctionDeclaration* d) {
    if(dec == d)
      return true;
    else
      return false;
  }

  bool isMemberFunction() {
    if(classSymbol ==  NULL)
      return false;
    else
      return true;
  }
  
  void addDetails(SgFunctionCallExp* e) {
    callSites.push_back(e);
  }
};



SgScopeStatement *outermostScope = NULL;
SgFunctionDeclaration* baseFunctionDeclaration = NULL;
std::vector<FunctionClassDetails*> baseFunctionList;
std::vector<SgClassDeclaration*> classSymbolsUsed;
std::ofstream outFile;
std::ofstream decFile;
SgFunctionParameterList* copyFunctionParameterList(SgFunctionParameterList* origList);
void getSuitableFunctionName(FunctionClassDetails* cur, 
			     SgName&  newName);
SgType* getReturnType(SgFunctionDeclaration* curFn);
SgFunctionDeclaration * copyFunctionDeclaration(FunctionClassDetails* cur);
void unparseCPPScopetoCScope( SgBasicBlock *originalScope, SgBasicBlock *newScope);
void unparseCPPtoCandPrint(FunctionClassDetails *cur,
			   SgFunctionDefinition * originalFunction, 
			   SgFunctionDeclaration* newFunctionDeclaration);
bool addToClassDeclarations(SgClassDeclaration* classDec);
SgFunctionParameterList* copyFunctionParameterList(SgFunctionParameterList* origList ) ;
void unparseClasstoStruct(SgClassDefinition* classDef);
void addPargmaforOriginalFunctionDeclaration(FunctionClassDetails *cur, SgBasicBlock* newBody);
bool skipatomicop(SgName funName);


std::vector<SgFunctionDefinition *> deepDeleteList;
SgScopeStatement * dummyScope;
int main ( int argc, char** argv )
{
  dummyScope = NULL;
#ifdef GREENMARL_CONVERSION
  char* fnName = argv[argc-1];
  SgProject* project = frontend(argc-1,argv);
#else
  char* fnName = "main";
  SgProject* project = frontend(argc,argv);
#endif
  //  dummyScope = buildScopeStatement(NULL);
  //ROSE_ASSERT(dummyScope != NULL);
  std::cout<<"\n\n\n************ START OF TRANSFORMATION *****************\n";
  
  outermostScope =  SageInterface::getFirstGlobalScope(project);
  printf("// The Function name is %s \n", fnName);
  SgFunctionDeclaration * fndefinition;  
  std::string filename = fnName + std::string("_cconvert.c");
  std::string tempfile = fnName + std::string("_definitions.c");

  decFile.open(tempfile.c_str());
  outFile.open(filename.c_str());

#ifdef GREENMARL_CONVERSION
  outFile<<"#include<stdio.h> \n";
  // add all .h file in output files as well.
  outFile<<"#pragma globalCodeStart \n#include <assert.h>\n#include <stdlib.h>\n#include <string.h>\n"
	 <<"#include <arpa/inet.h>\n#include <iostream>\n#include <fstream>\n"
	 <<"#include <string>\n#include <string.h>\n#include <sstream>\n"
	 <<"#include <map>\n#include <set>\n#include <vector>\n#include <algorithm>\n"
	 <<"#include <sys/time.h>\n#include <sys/types.h>\n#include <sys/stat.h>\n"
	 <<"#include <unistd.h>\n#include \"gm_mem_helper.h\"\n#include \"gm_graph.h\"\n"
	 <<"#include \"gm_util.h\"\n#include \"gm_lock.h\"\n#include \"gm_file_handling.h\"\n"
	 <<"#include \"common_main.h\"\n#include \"gm_common_neighbor_iter.h\"\n"
	 <<"#include \"gm_runtime.h\"\n#include \"gm_lock.h\"\n#include \"energylib.h\"\n"
	 <<"#pragma globalCodeEnd";
  outFile<<"\ntypedef signed int int32_t; \ntypedef int32_t edge_t;"
	 <<"\ntypedef int32_t node_t; \ntypedef int32_t edge_id; \ntypedef int32_t node_id;"
	 <<"\ntypedef int32_t edge_t; \nstruct vector; \ntypedef struct vector vector; "
	 <<"\nstruct iterator; \ntypedef struct iterator iterator; \ntypedef size_t size_type;"
	 <<"\n#define NIL_NODE (node_t - 1)\n"
	 <<"\ntypedef volatile int32_t gm_spinlock_t;\n"
	 <<"\ntypedef int * uintptr_t;\n"
	 <<"\ntypedef int* seq_iter;\n"
	 <<"\ntypedef unsigned int uint32_t;\n"
	 <<"\ntypedef long long int64_t;\n";
#endif

  SgFilePtrList & ptr_list = project->get_fileList();
  
  for (SgFilePtrList::iterator iter = ptr_list.begin(); iter!=ptr_list.end();
       iter++){ 
    SgFile* sageFile = (*iter);
    SgSourceFile * sfile = isSgSourceFile(sageFile);
    ROSE_ASSERT(sfile);
    SgGlobal *root = sfile->get_globalScope();


    // normalize all for loops if necessary
    
    Rose_STL_Container<SgNode*> forList =
      NodeQuery::querySubTree(root, V_SgForStatement);
    
    Rose_STL_Container<SgNode*>::iterator forItr = forList.begin();
    for (; forItr != forList.end(); forItr++) {
      SgForStatement* loop = isSgForStatement(*forItr);
      ROSE_ASSERT(loop != NULL);
      normalizeForLoopInitDeclaration (loop);
      SgStatement* loopBody = getLoopBody(loop);
      SgBasicBlock * basicBlock = isSgBasicBlock(loopBody);
      if(basicBlock == NULL) {
	basicBlock = buildBasicBlock (loopBody);
	setLoopBody(loop, basicBlock);
      }
    }


    SgDeclarationStatementPtrList& declList = root->get_declarations ();
    bool hasOpenMP= false; 

    

    for (SgDeclarationStatementPtrList::iterator p = declList.begin(); 
	 p != declList.end(); ++p) {
      SgFunctionDeclaration *func = isSgFunctionDeclaration(*p);
      if (func == 0)  continue;
      SgFunctionDefinition *defn = func->get_definition();
      if (defn == 0)  continue;
      if (defn->get_file_info()->get_filename() !=
	  sageFile->get_file_info()->get_filename())
	continue;
      if(SageInterface::get_name(func).compare(fnName) == 0 ) {
	SgBasicBlock *body = defn->get_body();
	ROSE_ASSERT(body != NULL);
	fndefinition = func;	
	FunctionClassDetails* baseDec = new FunctionClassDetails(func, 
								 NULL, NULL);	
	// copy main Function
	SgFunctionDeclaration* newFnDeclaration = 
	  copyFunctionDeclaration(baseDec);
	
	
	unparseCPPtoCandPrint(baseDec, defn, newFnDeclaration);
	removeStatement(defn, true);
	//deepDelete(defn);
	deepDeleteList.push_back(defn);
      }   
    }
  }
  
  int itr = 0;
  
  while(itr < baseFunctionList.size()) {
    FunctionClassDetails* curPtr = baseFunctionList[itr];
    ROSE_ASSERT(curPtr != NULL);
    SgFunctionDeclaration* declaration = curPtr->getDec();
    ROSE_ASSERT(declaration != NULL);
    SgFunctionDefinition* def = declaration->get_definition();
    ROSE_ASSERT(def != NULL);
    SgBasicBlock *body = def->get_body();
    ROSE_ASSERT(body != NULL);
    SgFunctionDeclaration* newFnDeclaration = 
      copyFunctionDeclaration(curPtr);	
    unparseCPPtoCandPrint(curPtr, def, newFnDeclaration);
    ROSE_ASSERT(def != NULL);
    removeStatement(def, false);
    //  deepDelete(def);
    deepDeleteList.push_back(def);
    itr++;
  }

  std::vector<SgClassDeclaration*>::iterator clsitr = classSymbolsUsed.begin();
  while(clsitr != classSymbolsUsed.end()) {
    // std::cout<<"Out class"<<std::endl;
    unparseClasstoStruct((*clsitr)->get_definition());
    clsitr++;
  }
  
#ifdef GREENMARL_CONVERSION
  std::string argumentList = "(";
  SgFunctionParameterList* paramList = fndefinition->get_parameterList();
  ROSE_ASSERT(paramList != NULL);
  SgInitializedNamePtrList list = paramList->get_args();
  //  std::cout<<"The number of argumnets to start function is "<<list.size()<<std::endl;
  SgInitializedNamePtrList::iterator piter = list.begin();
  // skip gm_graph
  piter ++;
  argumentList += "gm";
  for(;piter != list.end();piter ++) {
    argumentList += ", NULL" ;
  }
  argumentList += ")";
  
  outFile<<"\n\n int main() {\n" 
	 <<"   gm_graph gm;\n  "
         << "_"<<fnName << argumentList <<";"
	 <<" \n  return 0; \n\n }";
#endif
  decFile.close();  
  std::ifstream definitionFile;
  definitionFile.open(tempfile.c_str());
  for (std::string str; std::getline(definitionFile, str); ) {
    outFile << str << std::endl;
  }
  outFile.close();
  //  ROSE_ASSERT(outermostScope != NULL);
  // insertStatementBefore(getFirstStatement(outermostScope),
  // 			baseFunctionDeclaration);
  // isSgStatement(fndefinition->get_parent())->remove_statement(fndefinition); 

  std::vector<SgFunctionDefinition *>::iterator dItr =  deepDeleteList.begin();

  for(; dItr != deepDeleteList.end(); dItr++) {
    removeStatement (* dItr, true);
    //   deepDelete(*dItr);
  }

  return backend(project);
  
}


/**
   Copy an function into our output file.
 **/

void unparseCPPtoCandPrint(FunctionClassDetails *cur, SgFunctionDefinition * originalFunction, SgFunctionDeclaration* newFunctionDeclaration) {
  SgBasicBlock *fnBody  = originalFunction->get_body();
  ROSE_ASSERT(fnBody != NULL);
  SgFunctionDefinition *newFnDefinition = newFunctionDeclaration->get_definition();
  SgBasicBlock *newBody  = newFnDefinition->get_body();
  ROSE_ASSERT(newBody != NULL);

  addPargmaforOriginalFunctionDeclaration(cur, newBody);
  unparseCPPScopetoCScope(fnBody, newBody);

  
  // Copy the function back to a file.
  decFile<<newFunctionDeclaration->unparseToCompleteString()<<std::endl;
  return;
}


void getSuitableFunctionName(FunctionClassDetails* cur, SgName&  newName) {
  SgFunctionDeclaration* curFn = cur->dec;
  SgClassDeclaration* classSymbol = cur->classSymbol;
  if(classSymbol != NULL) {
    newName = 
      SgName( classSymbol->get_name()  + "_" +  curFn->get_name());
  } else {
    //added to change the signature of the c function
    newName = SgName("_" + curFn->get_name());
  }
  //  std::cout<<"The newly created function name is "<<newName<<std::endl;
  return;
}

void addPargmaforOriginalFunctionDeclaration(FunctionClassDetails *cur, SgBasicBlock* newBody) {
  SgFunctionDeclaration* curFn = cur->dec;
  
  
  
  SgClassDeclaration* classSymbol = cur->classSymbol;
  SgName newName;
  if(classSymbol != NULL) {
    newName = 
      SgName( "class " + classSymbol->get_name()  + " function " +  curFn->get_name());
  } else {
    newName = SgName( "function " + curFn->get_name());
  }
  std::string pragmaString = the_pragma_fnDef + newName;
  // std::cout<<"the pragma string is "<<pragmaString<<std::endl;
  // ROSE_ASSERT(outermostScope != NULL);
  SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newBody);
  ROSE_ASSERT(pragmaDec!= NULL);
  //  decFile<<pragmaDec->unparseToCompleteString()<<std::endl;
  appendStatement(pragmaDec, newBody);

  
  SgTreeCopy expCopyHelp;

  SgFunctionDeclaration * baseFunctionDeclaration = 
    buildNondefiningFunctionDeclaration(curFn->get_name(), 
					curFn->get_orig_return_type(), 
					curFn->get_parameterList(), 
					newBody);
  ROSE_ASSERT(baseFunctionDeclaration != NULL);

  /*
  SgFunctionDefinition* copyDef  = isSgFunctionDefinition(curFn->get_definition()->copy(expCopyHelp));
  ROSE_ASSERT(copyDef != NULL);
  copyDef->set_parent(newBody);
  //SgScopeStatement* scope = copyDef->get_scope();
  ROSE_ASSERT(scope != NULL);
  //  ROSE_ASSERT(dummyScope !=NULL);
  //copyDef->set_scope(dummyScope);
  SgBasicBlock* basicBlock = buildBasicBlock();
  ROSE_ASSERT(basicBlock != NULL);
  basicBlock->set_parent(copyDef);
  copyDef->set_body(basicBlock); */
  std::string decString = baseFunctionDeclaration->unparseToString();
  //std::cout<<"The functionDefinition is "<<decString<<std::endl;
  std::string pragmaSigString = the_pragma_fnSign + decString;
  SgPragmaDeclaration* pragmaSignature = buildPragmaDeclaration(pragmaSigString, newBody);
  ROSE_ASSERT(pragmaSignature != NULL);
  appendStatement(pragmaSignature, newBody);
  return;
}

void addPragmaforOriginalFunctionCall() {
  // We are going to assume the funcion calls are going to be 
  // in the order of appearance in the list.

  
}


SgType* getReturnType(SgFunctionDeclaration* curFn) {
  // TODO: getReturn type
  return curFn->get_orig_return_type(); 
}


SgFunctionDeclaration* copyFunctionDeclaration(FunctionClassDetails* cur) {

  SgFunctionDeclaration* curFn = cur->dec;
  SgClassDeclaration* classSymbol = cur->classSymbol;


  // Copied Function parameters

  SgFunctionParameterList* paramList = 
    copyFunctionParameterList(curFn->get_parameterList());  
  // TODO: append the class pointer this to the function parameter 
  // list.
 
  if(classSymbol != NULL) { 
    SgInitializedName* initName =
      buildInitializedName (thisName, buildPointerType(buildVoidType()));
    paramList->prepend_arg(initName);
  }
  SgType* returnType =  getReturnType(curFn);
  

  ROSE_ASSERT(outermostScope != NULL);
  // The scope should be global for the member function 
  // since the original scope would be that of the class. 
  SgScopeStatement* parentScope = NULL;
  if(classSymbol == NULL)
    parentScope = curFn->get_scope();
  else
    parentScope = getGlobalScope(classSymbol);

  SgName functionName;
  //  SgMemberFunctionDeclaration* 
  getSuitableFunctionName(cur, functionName);
  ROSE_ASSERT(!functionName.getString().empty());
  ROSE_ASSERT(returnType != NULL);
  ROSE_ASSERT(paramList != NULL);
  ROSE_ASSERT(parentScope != NULL);


  
  //  if(baseFunctionDeclaration == NULL) {
  baseFunctionDeclaration = buildNondefiningFunctionDeclaration(functionName, 
								returnType, paramList, parentScope);
    //}
  ROSE_ASSERT(baseFunctionDeclaration != NULL);
  // TODO if memFn != NULL prepent the struct to the copied list.
  


  outFile<<baseFunctionDeclaration->unparseToString()<<std::endl;
  
  SgFunctionDeclaration* functionDefinition = 
    buildDefiningFunctionDeclaration(functionName, returnType, paramList, 
				     parentScope);
  return functionDefinition;
}

std::string getbasename() {
  return std::string("base");
}

bool addToClassDeclarations(SgClassDeclaration* classDec) {
  SgClassDeclaration* found  =  NULL;
  ROSE_ASSERT(classDec != NULL);
  for(std::vector<SgClassDeclaration*>::iterator itr = classSymbolsUsed.begin();
      itr != classSymbolsUsed.end(); itr++) {
    if(* itr == classDec) {
      found =  *itr;
      break;
    }
  }
  if(found != NULL)
    return false;
  
  classSymbolsUsed.push_back(classDec);
  return true;
}

SgName __unknownFunction = "unknownFunction";

void replaceFunctionCall(SgName functionName, SgFunctionCallExp* callExpr, SgExpression* objectExpr, bool classCall) {	   
  if(classCall)
    ROSE_ASSERT(objectExpr != NULL);
  SgTreeCopy expCopyHelp;
  SgExprListExp* expList = callExpr->get_args();
  SgExprListExp* newExprList = 
    isSgExprListExp(expList->copy(expCopyHelp));
  SgExprStatement *newExpStatement = NULL;
  if(classCall)
    newExprList->prepend_expression(objectExpr);	    
  ROSE_ASSERT(newExprList != NULL);
  SgScopeStatement* scopeStmt = getScope(callExpr);
  ROSE_ASSERT(scopeStmt != NULL);
  SgFunctionCallExp* newfunctionCallExpr = 
    buildFunctionCallExp(functionName, callExpr->get_type(), newExprList, 
			 scopeStmt);
  SgStatement* parentStatement = getEnclosingStatement(callExpr);
  // TODO currently assumes that the call is made only inside 
  // TODO if for init state that in pragma call and add it above parent.
  SgForStatement* forStatement = isSgForStatement(parentStatement);
  // increament expression
  SgForInitStatement* forInit = isSgForInitStatement(parentStatement->get_parent());
  std::string pragmaStr;
  bool set = false;
  if(forStatement != NULL) {
    SgTreeCopy expCopyHelp;
    
    SgExpression* incExp = forStatement->get_increment();
    SgExpression* newCallExpr  = isSgExpression(incExp->copy(expCopyHelp));
    ROSE_ASSERT(newCallExpr != NULL);
    SgExprStatement* expStmt = buildExprStatement(newCallExpr);
    ROSE_ASSERT(expStmt != NULL);
    expStmt->set_parent(forStatement);
    pragmaStr = the_pragma_call + " increment "   + expStmt->unparseToString();
    set = true;
  }else if(forInit != NULL && set == false) {
    // for init
    //   std::cout<<"Inside for init statement.\n";
    pragmaStr = the_pragma_call + "for_init "+ parentStatement->unparseToString();
    parentStatement = isSgStatement(forInit->get_parent());
    ROSE_ASSERT(parentStatement !=NULL);
  } else {
    SgForStatement* ancestor = isSgForStatement(parentStatement->get_parent());
    if(ancestor != NULL) {
      if(ancestor->get_test() ==  parentStatement ) {
	// for test part
	pragmaStr = the_pragma_call + "fortest " + parentStatement->unparseToString();
	set = true;
	parentStatement = isSgStatement(parentStatement->get_parent());
      }
    }
    if(set == false)
      pragmaStr =  the_pragma_call + "normal " + parentStatement->unparseToString();
  }
  ROSE_ASSERT(parentStatement != NULL);
  forStatement = isSgForStatement(parentStatement);
  if(forStatement != NULL) {
    // check if its omp for 
    SgStatement * previousStatement = getPreviousStatement (forStatement, false);
    SgPragmaDeclaration* pragmaStatement = isSgPragmaDeclaration(previousStatement);
    if(pragmaStatement != NULL) {
      // std::cout<<"Previous pragma is "<<pragmaStatement->get_pragma()->get_pragma()<<std::endl;
      // Check if it is omp pragma
      std::string pragmaString = pragmaStatement->get_pragma()->get_pragma();
      if(pragmaString.compare(0,4,"omp ") == 0) {
	//std::cout<<"Found omp pragma ***\n";
	// move above the pragma statement
	parentStatement = pragmaStatement;
      }
      
    }
  }
  SgPragmaDeclaration* pragmaStatement = buildPragmaDeclaration(pragmaStr, parentStatement->get_scope());
  insertStatement(parentStatement, pragmaStatement, true, true);
  replaceExpression(callExpr, newfunctionCallExpr); 
}


void unparseCPPScopetoCScope(SgBasicBlock *originalScope, SgBasicBlock *newScope) {

  /**
     Make a copy of all statements in the original scope into the new scope and
     work on the new scope.
   **/
  SgStatementPtrList  bodyStatements = originalScope->getStatementList();
  SgStatement* newStatement  = NULL;
  for (SgStatementPtrList::iterator k = 
	 bodyStatements.begin(); k < bodyStatements.end(); ++k) {
       SgStatement* curStatement = *k;
       newStatement = copyStatement(curStatement);
       ROSE_ASSERT(newStatement != NULL);
       appendStatement(newStatement, newScope);
  }

  

  Rose_STL_Container<SgNode*> classObjects =
    NodeQuery::querySubTree(newScope,  V_SgVariableDeclaration);

  Rose_STL_Container<SgNode*>::iterator clsItr = classObjects.begin();
  for (; clsItr != classObjects.end(); clsItr++) {
    SgVariableDeclaration * classDec = isSgVariableDeclaration(*clsItr);
    ROSE_ASSERT(classDec != NULL);
    //    SgClassDeclaration * sgclass = isSgClassDeclaration();
    SgType * myType = getFirstVarType (classDec);
    // if(myType != NULL)
    //   std::cout<<"The class used is "<<myType->class_name()<<std::endl;




    SgTypedefType* typedefType = isSgTypedefType(myType);

    if(typedefType != NULL) {
      //   std::cout<<"The class name is "<<typedefType->get_name()<<std::endl;
    }
    
    SgPointerType* ptrType = isSgPointerType(myType);
    
    while(ptrType != NULL) {
      myType = ptrType->get_base_type ();
      ptrType = isSgPointerType(myType);
    }

    SgClassType* classType = isSgClassType(myType);
    if(classType != NULL) {
      //  std::cout<<"The class name is "<<classType->get_name()<<std::endl;
    }
  }


  SgType* voidPtrType = buildPointerType(buildVoidType());

  Rose_STL_Container<SgNode*> templateDec = 
    NodeQuery::querySubTree(newScope, V_SgTemplateVariableDeclaration);

  Rose_STL_Container< SgNode*>::iterator itrT = templateDec.begin();
  for (; itrT != templateDec.end(); itrT++) {
    SgTemplateVariableDeclaration* tempVar = isSgTemplateVariableDeclaration(* itrT);
    std::cout<<" The template declarations are " <<tempVar->unparseToString()<<std::endl; 
  }
  /**
     Collect all class object declartion to equivalent struct
   **/
  Rose_STL_Container<SgNode*> varDecList =
    NodeQuery::querySubTree(newScope, V_SgVariableDeclaration);
  ROSE_ASSERT(voidPtrType != NULL);
  Rose_STL_Container<SgNode*>::iterator itr = varDecList.begin();
  for (; itr != varDecList.end(); itr++) {
    SgVariableDeclaration *varDec = isSgVariableDeclaration(*itr);
    ROSE_ASSERT(varDec != NULL);
    SgVariableDefinition *varDef =  varDec->get_definition();
    ROSE_ASSERT(varDef != NULL);
    
    SgType* declarationType = varDef->get_type();
    SgType* myType = varDef->get_type();
    SgPointerType* ptrType = isSgPointerType(myType);
    
    
    while(ptrType != NULL) {
      myType = ptrType->get_base_type ();
      ptrType = isSgPointerType(myType);
    }
    SgReferenceType* refType = isSgReferenceType(myType);
    while(refType != NULL) {
      myType = refType->get_base_type ();
      refType = isSgReferenceType(myType);
    }
    SgClassType* classType = isSgClassType(myType);
    SgNewExp *newExpr = NULL;


    Rose_STL_Container<SgNode*> newList =
    NodeQuery::querySubTree(varDec, V_SgNewExp);
    Rose_STL_Container<SgNode*>::iterator funItr = newList.begin();
    for (; funItr != newList.end(); funItr++) {
      SgNewExp* newCall  = isSgNewExp(*funItr);
      if(newCall != NULL)
	newExpr = newCall;
    }


    SgConstructorInitializer* constructor = NULL;
    Rose_STL_Container<SgNode*> constructorList =
    NodeQuery::querySubTree(varDec, V_SgConstructorInitializer);
    Rose_STL_Container<SgNode*>::iterator constItr = constructorList.begin();
    for (; constItr != constructorList.end(); constItr++) {
      constructor = isSgConstructorInitializer(*constItr);
    }
    

    std::string originalString  = varDec->unparseToCompleteString();
    SgTemplateVariableDeclaration* sgvar = isSgTemplateVariableDeclaration(varDec);
    
    if(originalString.find("iterator") != std::string::npos) {
      SgUnparse_Info *info = new SgUnparse_Info();
      info->set_inEmbeddedDecl();
      info->set_prefixOperator ();
      info->unset_SkipBaseType ();
      info->set_inAggregateInitializer ();
      info->unset_SkipClassSpecifier ();
      info->SkipFunctionQualifier ();
      info->unset_SkipCPPDirectives ();
      info->set_outputClassTemplateName ();
      info->set_forceQualifiedNames ();
      info->set_useTypeAttributes ();
      info->unset_supressStrippedTypeName ();
      info->set_cxx11_initialization_list ();

      if(originalString.find("/*") != std::string::npos ) {      
	  originalString.erase(originalString.find("/*"), 2);
	  originalString.erase(originalString.find("*/")-1,  4);
	  std::cout<<" the declaration "<<originalString <<std::endl;
	}
      //std::cout<<varDec->get_baseTypeDefiningDeclaration ()->unparseToCompleteString()<<std::endl;
    }
   
    if(originalString.find("\n", 0) != std::string::npos) {

      //      std::cout<<"Multiple lines "<<originalString<<std::endl;
      // if the start of the statement is # or // then we need to skip till last \n.
      std::string::size_type prev = 0;
      std::string str = originalString; 
      while(str.find("\n", prev) != std::string::npos) {
	std::string::size_type pos = str.find("\n", prev);
	prev = pos + 1;
      }
      //      originalString = " garbage skip " + str.substr(prev);
      originalString = str.substr(prev);
    }
 
    std::string pragmaString = the_pragma_skip + originalString;
    SgName varName = varDef->get_vardefn()->get_qualified_name();

    if(classType == NULL && newExpr != NULL) {
      SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
      ROSE_ASSERT(pragmaDec != NULL);
      // std::cout<<" null not null "<<varName<<std::endl;
 
      // TODO add malloc expression here
      SgInitializer* mallocInitializer = NULL;
      SgVariableDeclaration* newVarDec = buildVariableDeclaration(varName, declarationType, mallocInitializer, newScope);
      ROSE_ASSERT(newVarDec != NULL);
      insertStatement(varDec, pragmaDec, true, true);
      replaceStatement(varDec, newVarDec, false);
    } else if(classType != NULL && newExpr == NULL) {
      SgName className = classType->get_name();
      SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
      ROSE_ASSERT(pragmaDec != NULL);
      //  std::cout<<" not null null "<<varName<<std::endl;
      //TODO create a proper struct type instead  of using voidPtrType
      //TODO copy the initializer sageInterface::getInitializerOfExpression (SgExpression *n)
      SgInitializer* defaultInitializer = NULL; 
      SgVariableDeclaration* newVarDec = buildVariableDeclaration (varName, voidPtrType, defaultInitializer, newScope);
      ROSE_ASSERT(newVarDec != NULL);
      insertStatement(varDec, pragmaDec, true, true);
      replaceStatement(varDec, newVarDec, false);
    } else if(classType != NULL && newExpr != NULL) {
      SgName className = classType->get_name();
      SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
      ROSE_ASSERT(pragmaDec != NULL);
      //TODO create a proper struct type instead  of using voidPtrType
      //TODO add  malloc expression here

      //      std::cout<<" not nul not null "<<varName<<std::endl;
      SgInitializer* mallocInitializer = NULL; 
      SgVariableDeclaration* newVarDec = buildVariableDeclaration (varName, voidPtrType, mallocInitializer, newScope);
      ROSE_ASSERT(newVarDec != NULL);
      insertStatement(varDec, pragmaDec, true, true);
      replaceStatement(varDec, newVarDec, false);
    } else if (constructor != NULL) {
      SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
      ROSE_ASSERT(pragmaDec != NULL);
      SgVariableDeclaration* newVarDec = buildVariableDeclaration (varName, voidPtrType, NULL, newScope);
      ROSE_ASSERT(newVarDec != NULL);
      insertStatement(varDec, pragmaDec, true, true);
      replaceStatement(varDec, newVarDec, false);
    }
  }
  

  /**
     TODO Collect all new to malloc
     which are not part of the declaration statements 
   **/
  
  Rose_STL_Container<SgNode*> newList =
    NodeQuery::querySubTree(newScope, V_SgNewExp);
  

  Rose_STL_Container<SgNode*>::iterator funItr = newList.begin();
  for (; funItr != newList.end(); funItr++) {
    SgNewExp* newCall  = isSgNewExp(*funItr);
    ROSE_ASSERT(newCall != NULL);
    SgStatement* parentStatement =  getEnclosingStatement(newCall);
    //  std::cout<<"The original parent statment is"<<parentStatement->unparseToCompleteString()<<std::endl;
    ROSE_ASSERT(parentStatement != NULL);
    std::string originalString  = parentStatement->unparseToCompleteString();
    // std::cout<<originalString<<std::endl;
    std::string pragmaString = the_pragma_skip + originalString;
    SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
    ROSE_ASSERT(pragmaDec != NULL);
    // Should be an Expression statement.
    SgExprStatement* exprStatement = isSgExprStatement(parentStatement);
    ROSE_ASSERT(exprStatement != NULL);
    SgExpression* expr = exprStatement->get_expression();
    // should be assignop
    SgAssignOp* assignOp = isSgAssignOp(expr);
    SgExpression* lhs = assignOp->get_lhs_operand();
    SgTreeCopy expCopyHelp;
    SgExpression* newLhs = isSgExpression(lhs->copy(expCopyHelp));
    ROSE_ASSERT(newLhs != NULL);
    // TODO generate malloc operations
    SgExpression* newRhs = buildNullptrValExp();
    ROSE_ASSERT(newRhs != NULL);
    SgAssignOp* newAssignOp = buildAssignOp(newLhs, newRhs);
    SgExprStatement* newStatement = buildExprStatement(newAssignOp);
    ROSE_ASSERT(newStatement != NULL);
    insertStatement(parentStatement, pragmaDec, true, true);
    replaceStatement(parentStatement, newStatement, false);
  }

  /**
     change delete Expression.
   **/ 

  
  Rose_STL_Container<SgNode*> delList =
    NodeQuery::querySubTree(newScope, V_SgDeleteExp);
  

  funItr = delList.begin();
  for (; funItr != delList.end(); funItr++) {
    SgDeleteExp* delCall  = isSgDeleteExp(*funItr);
    ROSE_ASSERT(delCall != NULL);
    SgStatement* parentStatement =  getEnclosingStatement(delCall);
    ROSE_ASSERT(parentStatement != NULL);
    std::string originalString  = parentStatement->unparseToCompleteString();
    std::cout<<"The main dec "<<originalString<<std::endl;
    std::string pragmaString = the_pragma_skip + originalString;
    SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
    ROSE_ASSERT(pragmaDec != NULL);
    SgExpression* deleteValue = delCall->get_variable ();
    SgExpression* newRhs = buildNullptrValExp();
    ROSE_ASSERT(newRhs != NULL);
    SgAssignOp* newAssignOp = buildAssignOp(deleteValue, newRhs);
    SgExprStatement* newStatement = buildExprStatement(newAssignOp);
    ROSE_ASSERT(newStatement != NULL);
    insertStatement(parentStatement, pragmaDec, true, true);
    replaceStatement(parentStatement, newStatement, false);
  }


  /**
     Collect all  function calls made
   **/

  Rose_STL_Container<SgNode*> funCallList =
    NodeQuery::querySubTree(newScope, V_SgFunctionCallExp);

  /**
     For all function calls made make changes on class member function calls.
   **/
  itr = funCallList.begin();
  for (; itr != funCallList.end(); itr++) {
    SgFunctionCallExp *callExpr = isSgFunctionCallExp(*itr);
    if (callExpr != NULL) {
      
      SgFunctionDeclaration*  fnDec = 
	callExpr-> getAssociatedFunctionDeclaration();

      
      
      SgDeclarationStatement* defDec = fnDec->get_definingDeclaration();
      SgFunctionSymbol* fnSymbol = 
	callExpr->getAssociatedFunctionSymbol();
      SgMemberFunctionSymbol* memFunction = 
	isSgMemberFunctionSymbol(fnSymbol);
      if(fnSymbol->get_name().getString().compare("gettimeofday") == 0) {
	// get time of day change the second argumnet to NULL. 
	//	std::cout<<"The called function name is FOUND "<<fnSymbol->get_name()<<std::endl;
	//	SgTreeCopy expCopyHelp;
	SgExprListExp* expList = callExpr->get_args();
	//	SgExprListExp* newExprList = 
	//  isSgExprListExp(expList->copy(expCopyHelp));
	SgNullptrValExp* nullExpression = buildNullptrValExp();
	//	newExprList->append_expression(nullExpression);
	SgExpressionPtrList& ptrList = expList->get_expressions();
	SgExpressionPtrList::iterator listItr = ptrList.begin();
	listItr++;
	SgExpression* oldExp = *listItr;
	replaceExpression (oldExp, nullExpression, false);
      }
      SgClassDeclaration* classDec = NULL;
      SgTreeCopy expCopyHelp;
      
      //      std::cout<<"Call Expr "<<get_name(callExpr)<<std::endl;

      


      SgClassDeclaration* classDeclaration;

      bool undefined = false;
      if(defDec != NULL) {
	ROSE_ASSERT(fnSymbol != NULL);
	if(memFunction != NULL) {
	  /**
	     What we have is a class member function.
	     Chaneg the function name  to 
	     classname_function name
	     prepent the class object to the function list.
	  **/
	  // std::cout<<"Its a member function "<<get_name(callExpr->get_function())<<std::endl;

	  
	  SgDotExp* dotExpr = isSgDotExp(callExpr->get_function());
	  SgArrowExp* arrowExpr = isSgArrowExp(callExpr->get_function());
	  SgExpression* object = NULL;
	  SgExpression* objectExpr = NULL;
	  if(dotExpr != NULL) {
	    //	    std::cout<<"The statement under consideration is  "
	    //	     <<getEnclosingStatement(dotExpr)->unparseToCompleteString()
	    //	     <<std::endl;
	    object = dotExpr->get_lhs_operand();
	    ROSE_ASSERT(object != NULL);
	    ROSE_ASSERT(object != NULL);
	    SgVarRefExp* refExpr = isSgVarRefExp(object);
	    SgCastExp* castExp = isSgCastExp(object);
	    if(refExpr == NULL && castExp == NULL){
	      // this means standard library function calls
	      // if template then 
	      
	      undefined = true;
	    } else if(castExp == NULL){
	      objectExpr = 
		isSgExpression(refExpr->copy(expCopyHelp));
	    } else {
	      // skip and then undefined 
	      replaceFunctionCall(__unknownFunction, callExpr, NULL, false); 
	      undefined  =true;
	    }
	    
	  } else if(arrowExpr != NULL) {
	    	    
	    object = arrowExpr->get_lhs_operand();
	    ROSE_ASSERT(object != NULL);
	    SgVarRefExp* refExpr = isSgVarRefExp(object);
	    if(refExpr == NULL){
	      // this means standard library function calls
	      undefined = true;
	    } else {
	      objectExpr = 
		isSgExpression(refExpr->copy(expCopyHelp));
	    }

	  } else {
	    // this pointer.
	    // std::cout<<"SgDot Expression is NULL "<<get_name(callExpr)<<std::endl;

	    objectExpr = 
	      buildVarRefExp (thisName, newScope);
	  }
	  


	  /* if the class is one of the system classes, we skip definition.*/
	  if(undefined == false) {
	    SgMemberFunctionDeclaration* classFunctionDec = 
	    isSgMemberFunctionDeclaration(defDec);
	  ROSE_ASSERT(classFunctionDec != NULL);
	    classDec  = 
	      classFunctionDec->get_associatedClassDeclaration();
	    ROSE_ASSERT(classDec != NULL);
	    undefined = skipTheClass(classDec->get_name());
	  }


	  if(undefined == false) {
	    ROSE_ASSERT(objectExpr != NULL);
	    SgMemberFunctionDeclaration* classFunctionDec = 
	      isSgMemberFunctionDeclaration(defDec);
	    ROSE_ASSERT(classFunctionDec != NULL);
	    ROSE_ASSERT(classDec != NULL);
	    classDec  = 
	      classFunctionDec->get_associatedClassDeclaration();
	    
	    SgName functionName = 
	      classDec->get_name() + "_" +  fnSymbol->get_name();
	    bool skipfunction = skipTheFunction(functionName);
	    
	    if(skipfunction) {
	      std::cout<<" function name to unkewnn"<<functionName<<std::endl;
	      replaceFunctionCall(__unknownFunction, callExpr, NULL, false); 
	    }

	    if(skipfunction)
	      undefined = true;

	  }

	  
	  if(undefined == false) {
	    SgName functionName = 
	      classDec->get_name() + "_" +  fnSymbol->get_name();
	    SgMemberFunctionDeclaration* classFunctionDec = 
	      isSgMemberFunctionDeclaration(defDec);
	    
	    addToClassDeclarations(classDec);
	    
	    

	    // std::cout<< "The newly created function name is "<< functionName 
	    // 	     << " from original function name "<< fnSymbol->get_name()
	    // 	     << " and className "<< classDec->get_name()<<std::endl;
	    
	    replaceFunctionCall(functionName, callExpr, objectExpr, true); 
	    

	  }
	} else {
	  // non class member function
	  SgName funName  =  fnDec->get_name();
	  undefined = skipTheFunction(funName);
	  bool skip = skipatomicop(funName);
	  if(skip) {
	    std::cout<<" function name to unkewnn"<<funName<<std::endl;
	    replaceFunctionCall(__unknownFunction, callExpr, NULL, false); 
	    undefined = true;
	  }
	  if(undefined == false) {
	    // rewrite the function call
	    SgName newName = "_" + funName;
	    replaceFunctionCall(newName, callExpr, NULL, false); 
	  }
	}
	
	
	
	/* Add details for copying the referred function codes. 
	   Keep a structure to avoid copying more than once.
	*/
   	
	if(undefined != true) {
	  SgFunctionDeclaration*
	    newfnDec = isSgFunctionDeclaration(defDec); 
	  ROSE_ASSERT(fnDec != NULL);
	
	  if(newfnDec == NULL) {
	    // std::cout<<"The function without declaration is "<<fnDec->get_name()<<std::endl;
	  }
	  
	  
	  FunctionClassDetails* found  = NULL;
	  std::vector<FunctionClassDetails*>::iterator it = 
	    baseFunctionList.begin();
	  for(; it != baseFunctionList.end(); it++) {
	    if((*it)->found(newfnDec)) {
	      found = *it;
	      break;
	    }
	  }
    
	  if(found == NULL) {
	    ROSE_ASSERT(fnDec != NULL);
	    ROSE_ASSERT(newfnDec != NULL);
	    // std::cout<<"The new function added "<<fnDec->get_name()<<std::endl;
	    found  = new  FunctionClassDetails(newfnDec, callExpr, classDec);
	    ROSE_ASSERT(found != NULL);
	    baseFunctionList.push_back(found);
	  } else {
	    found->addDetails(callExpr);
	  }
	}
      }
    }
  }
}



SgFunctionParameterList* copyFunctionParameterList(SgFunctionParameterList* origList ) { 
  SgFunctionParameterList *newFunParamsList = new SgFunctionParameterList();
  SgInitializedNamePtrList& arglist = origList->get_args();
  SgInitializedNamePtrList::iterator origFunArgsIter = arglist.begin();
  for(; origFunArgsIter != arglist.end(); ++origFunArgsIter) {
    ROSE_ASSERT((*origFunArgsIter) != NULL);
    SgType *varType = (*origFunArgsIter)->get_type();
    ROSE_ASSERT(varType != NULL);
    SgReferenceType *refType = isSgReferenceType(varType);
    if(refType != NULL) {
      varType = refType->get_base_type();
    }
    if(isSgClassType(varType) == NULL) {
      SgInitializedName* classArg = buildInitializedName( (* origFunArgsIter)->get_name(),
     							  (* origFunArgsIter)->get_type());
      appendArg(newFunParamsList, classArg);
    } else {
      SgInitializedName *classArg = buildInitializedName(GM_GRAPH_NAME
     							 ,buildPointerType(buildVoidType()));
      appendArg(newFunParamsList, classArg); 
    }
  }
  return newFunParamsList;
}



void unparseClasstoStruct(SgClassDefinition* classDef) {
  
  std::string className = classDef->get_declaration()->get_name();
  // ROSE_ASSERT(!className.empty());
  // std::cout<<"The class unparsed is "<<className<<std::endl;
  
  // SgScopeStatement* outerMostScope = classDef->get_scope(); 
  // ROSE_ASSERT(outerMostScope != NULL);
  // SgTemplateInstantiationDefn* tempDefinition = isSgTemplateInstantiationDefn(classDef);
  // SgTemplateClassDefinition* templateDef = isSgTemplateClassDefinition(classDef);
  
  // SgClassDeclaration* definingDec = isSgClassDeclaration(classDef->get_declaration()->get_declaration_associated_with_symbol()->get_definingDeclaration());
  
  // ROSE_ASSERT(definingDec != NULL);
  // //std::cout<<"The class data is \n"<<definingDec->unparseToCompleteString()<<std::endl;
  
  // if(tempDefinition != NULL || templateDef != NULL)
  //   return;

  // SgDeclarationStatementPtrList  decStatements = classDef->get_members();
  // if(decStatements.size() == 0)
  //   return;

  // SgClassDeclaration* classtoStructDecl = NULL;
  // // classtoStructDecl = buildStructDeclaration(className, outermostScope);
  // ROSE_ASSERT(classtoStructDecl!= NULL);
  

  // SgClassDefinition *structDef = classtoStructDecl->get_definition();
  // ROSE_ASSERT(structDef != NULL);
  // SgScopeStatement* structDefScope = isSgScopeStatement(structDef);
  // ROSE_ASSERT(structDefScope!= NULL);


  
  // SgDeclarationStatementPtrList::iterator itr = decStatements.begin();
  // SgStatement* newStatement = NULL; 
  // for(; itr != decStatements.end(); ++itr) {
  //   SgDeclarationStatement* newDec = *(itr);
  //   // std::cout<<"the declaration to be added is "<<newDec->unparseToCompleteString()<<std::endl;
  //   SgVariableDeclaration* varDec =  isSgVariableDeclaration(newDec);
  //   if(varDec != NULL) {
  //     newStatement = copyStatement(varDec);
  //     ROSE_ASSERT(newStatement != NULL);
  //     //      appendStatement(newStatement, structDefScope);
  //   }
  // }
  // decFile<<classtoStructDecl->unparseToCompleteString()<<std::endl;
  outFile<<"struct "<<className<<";\n";
  outFile<<"typedef struct "<<className<<" "<<className<<";\n";
  
}

bool skipatomicop(SgName funName) {
  if(funName.getString().compare("ATOMIC_ADD < float > ") == 0 ){
        std::cout<<"The function to be skipped is "<<funName.getString()<<"|"<<std::endl;
      std::cout<<"SKIPPEDD   D"<<std::endl;
    return true;
  } 
  return false;
}

bool skipTheFunction(SgName funName) {
  for(int i=0; i < sizeofFunctionsTobeSkipped; i++) {
    if(functionsTobeSkipped[i].compare(funName.getString()) == 0) {
      std::cout<<"The function skipped is "<<funName.getString()<<std::endl;
      return true;
    }
  }
  return false;
}

bool skipTheClass(SgName className) {
  //  std::cout<<"The class to be skipped is "<<className.getString()<<std::endl;
  for(int i=0; i < sizeofClassesTobeSkipped; i++) {
    if(classesTobeSkipped[i].compare(className.getString()) == 0) {
      return true;
    }
  }
  return false;
}
