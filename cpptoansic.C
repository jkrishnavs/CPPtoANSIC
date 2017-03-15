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
  "__copy_move_backward < true , true , iterator_category > "};

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
    "log < int > "
  };

int sizeofClassesTobeSkipped = 8;
int sizeofFunctionsTobeSkipped = 26;


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



int main ( int argc, char** argv )
{

#ifdef GREENMARL_CONVERSION
  char* fnName = argv[argc-1];
  SgProject* project = frontend(argc-1,argv);
#else
  char* fnName = "main";
  SgProject* project = frontend(argc,argv);
#endif

  std::cout<<"\n\n\n************ START OF TRANSFORMATION *****************\n";
  
  outermostScope =  SageInterface::getFirstGlobalScope(project);
  printf("// The Function name is %s \n", fnName);
  SgFunctionDeclaration * fndefinition;  
  std::string filename = fnName + std::string("_cconvert.c");
  std::string tempfile = fnName + std::string("_definitions.c");

  decFile.open(tempfile.c_str());
  outFile.open(filename.c_str());

#ifdef GREENMARL_CONVERSION
  outFile<<"#include<stdio.h> \ntypedef signed int int32_t; \ntypedef int32_t edge_t;"
	 <<"\ntypedef int32_t node_t; \ntypedef int32_t edge_id; \ntypedef int32_t node_id;"
	 <<"\ntypedef int32_t edge_t; \nstruct vector; \ntypedef struct vector vector; "
	 <<"\nstruct iterator; \ntypedef struct iterator iterator; \ntypedef size_t size_type;"
	 <<"\n#define NIL_NODE (node_t - 1)\n";
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
      
      //      std::cout<<loop->unparseToCompleteString()<<std::endl;
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
  SgInitializedNamePtrList::iterator piter = list.begin();
  piter ++;
  argumentList += "gm";
  for(;piter != list.end();piter ++) {
    argumentList += ", NULL" ;
  }
  argumentList += ")";
  
  outFile<<"\n\n int main() {\n" 
	 <<"   gm_graph gm;\n  "
         << fnName << argumentList <<";"
	 <<" \n  return 0; \n\n }";
#endif
  decFile.close();  
  std::ifstream definitionFile;
  definitionFile.open(tempfile.c_str());
  for (std::string str; std::getline(definitionFile, str); ) {
    outFile << str << std::endl;
  }
  outFile.close();
  ROSE_ASSERT(outermostScope != NULL);
  insertStatementBefore(getFirstStatement(outermostScope),
  			baseFunctionDeclaration);
  isSgStatement(fndefinition->get_parent())->remove_statement(fndefinition); 
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
    newName = SgName(curFn->get_name());
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
  std::cout<<"the pragma string is "<<pragmaString<<std::endl;
  // ROSE_ASSERT(outermostScope != NULL);
  SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newBody);
  ROSE_ASSERT(pragmaDec!= NULL);
  //  decFile<<pragmaDec->unparseToCompleteString()<<std::endl;
  appendStatement(pragmaDec, newBody);
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
  
  SgInitializedName* initName =
    buildInitializedName (thisName, buildPointerType(buildVoidType()));
  //(SgType *base_type=NULL)Type());
  //classSymbol->get_type());  
  paramList->prepend_arg(initName);
    
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


  /**
     Collect all class object declartion to equivalent struct
   **/
  Rose_STL_Container<SgNode*> varDecList =
    NodeQuery::querySubTree(newScope, V_SgVariableDeclaration);
  SgType* voidPtrType = buildPointerType(buildVoidType());
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
    

    std::string originalString  = varDec->unparseToCompleteString();
    std::string pragmaString = the_pragma_skip + originalString;
    SgName varName = varDef->get_vardefn()->get_qualified_name();

    if(classType == NULL && newExpr != NULL) {
      SgPragmaDeclaration* pragmaDec = buildPragmaDeclaration(pragmaString, newScope);
      ROSE_ASSERT(pragmaDec != NULL);
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
      SgInitializer* mallocInitializer = NULL; 
      SgVariableDeclaration* newVarDec = buildVariableDeclaration (varName, voidPtrType, mallocInitializer, newScope);
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
    std::cout<<"The original parent statment is"<<parentStatement->unparseToCompleteString()<<std::endl;
    ROSE_ASSERT(parentStatement != NULL);
    std::string originalString  = parentStatement->unparseToCompleteString();
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
      SgClassDeclaration* classDec = NULL;
      SgTreeCopy expCopyHelp;
      
      //      std::cout<<"Call Expr "<<get_name(callExpr)<<std::endl;

      SgClassDeclaration* classDeclaration;

      bool undefined = false;
      if(defDec != NULL) {
	ROSE_ASSERT(fnSymbol != NULL);
	//	std::cout<<"The called function name is "<<fnSymbol->get_name()<<std::endl;
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
	    
	    object = dotExpr->get_lhs_operand();
	    ROSE_ASSERT(object != NULL);
	    ROSE_ASSERT(object != NULL);
	    SgVarRefExp* refExpr = isSgVarRefExp(object);
	    if(refExpr == NULL){
	      // this means standard library function calls
	      undefined = true;
	    } else {
	      objectExpr = 
		isSgExpression(refExpr->copy(expCopyHelp));
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
	    undefined = skipTheFunction(functionName);
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
	    
	    
	    
	    
	    SgExprListExp* expList = callExpr->get_args();
	    SgExprListExp* newExprList = 
	      isSgExprListExp(expList->copy(expCopyHelp));
	    SgExprStatement *newExpStatement = NULL;
	    newExprList->prepend_expression(objectExpr);	    
	    ROSE_ASSERT(newExprList != NULL);
	    // SgType * newType = new SgType(*callExpr->get_type());
	    // ROSE_ASSERT(newType != NULL);
	    SgScopeStatement* scopeStmt = getScope(callExpr);
	    ROSE_ASSERT(scopeStmt != NULL);
	    SgFunctionCallExp* newfunctionCallExpr = 
	      buildFunctionCallExp(functionName, callExpr->get_type(), newExprList, 
				   scopeStmt);

	    SgStatement* parentStatement = getEnclosingStatement(callExpr);
	    // TODO currently assumes that the call is made only inside 
	    // TODO if for init state that in pragma call and add it above parent.
	    SgForInitStatement* forInit = isSgForInitStatement(parentStatement->get_parent());
	    std::string pragmaStr;
	    if(forInit != NULL) {
	      std::cout<<"Inside for init statement.\n";
	      pragmaStr = the_pragma_call + "for_init "+ parentStatement->unparseToString();
	      parentStatement = isSgStatement(forInit->get_parent());
	      ROSE_ASSERT(parentStatement !=NULL);
	    } else {
	      pragmaStr =  the_pragma_call; // + parentStatement->unparseToString();
	    }
	      //    if(parentStatement)
	    ROSE_ASSERT(parentStatement != NULL);
	    SgPragmaDeclaration* pragmaStatement = buildPragmaDeclaration(pragmaStr, parentStatement->get_scope());
	    insertStatement(parentStatement, pragmaStatement, true, true);
	    replaceExpression(callExpr, newfunctionCallExpr);
	  }
	} else {
	  SgName funName  = fnDec->get_name();
	  undefined = skipTheFunction(funName);
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
  ROSE_ASSERT(!className.empty());
  std::cout<<"The class unparsed is "<<className<<std::endl;
  
  SgScopeStatement* outerMostScope = classDef->get_scope(); 
  ROSE_ASSERT(outerMostScope != NULL);
  SgTemplateInstantiationDefn* tempDefinition = isSgTemplateInstantiationDefn(classDef);
  SgTemplateClassDefinition* templateDef = isSgTemplateClassDefinition(classDef);
  
  SgClassDeclaration* definingDec = isSgClassDeclaration(classDef->get_declaration()->get_declaration_associated_with_symbol()->get_definingDeclaration());
  
  ROSE_ASSERT(definingDec != NULL);
  //std::cout<<"The class data is \n"<<definingDec->unparseToCompleteString()<<std::endl;
  
  if(tempDefinition != NULL || templateDef != NULL)
    return;

  SgDeclarationStatementPtrList  decStatements = classDef->get_members();
  if(decStatements.size() == 0)
    return;

  SgClassDeclaration* classtoStructDecl = NULL;
  classtoStructDecl = buildStructDeclaration(className, outermostScope);
  ROSE_ASSERT(classtoStructDecl!= NULL);
  

  SgClassDefinition *structDef = classtoStructDecl->get_definition();
  ROSE_ASSERT(structDef != NULL);
  SgScopeStatement* structDefScope = isSgScopeStatement(structDef);
  ROSE_ASSERT(structDefScope!= NULL);


  
  SgDeclarationStatementPtrList::iterator itr = decStatements.begin();
  SgStatement* newStatement = NULL; 
  for(; itr != decStatements.end(); ++itr) {
    SgDeclarationStatement* newDec = *(itr);
    // std::cout<<"the declaration to be added is "<<newDec->unparseToCompleteString()<<std::endl;
    SgVariableDeclaration* varDec =  isSgVariableDeclaration(newDec);
    if(varDec != NULL) {
      newStatement = copyStatement(varDec);
      ROSE_ASSERT(newStatement != NULL);
      appendStatement(newStatement, structDefScope);
    }
  }
  decFile<<classtoStructDecl->unparseToCompleteString()<<std::endl;
  outFile<<"struct "<<className<<";\n";
  outFile<<"typedef struct "<<className<<" "<<className<<";\n";
  
}

bool skipTheFunction(SgName funName) {
  // std::cout<<"The function to be skipped is "<<funName.getString()<<std::endl;
  for(int i=0; i < sizeofFunctionsTobeSkipped; i++) {
    if(functionsTobeSkipped[i].compare(funName.getString()) == 0)
      return true;
  }
  return false;
}

bool skipTheClass(SgName className) {
  //  std::cout<<"The class to be skipped is "<<className.getString()<<std::endl;
  for(int i=0; i < sizeofClassesTobeSkipped; i++) {
    if(classesTobeSkipped[i].compare(className.getString()) == 0)
      return true;
  }
  return false;
}

