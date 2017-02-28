// ROSE is a tool for building preprocessors, this file is an example preprocessor built with ROSE.

#include "rose.h"
#include<cstdlib>
#include<iostream>
#include<fstream>
// Main Function for default example ROSE Preprocessor
// This is an example of a preprocessor that can be built with ROSE
// This example can be used to test the ROSE infrastructure

using namespace OmpSupport;
using namespace SageBuilder;
using namespace SageInterface;

#define GREENMARL_CONVERSION

#define GM_GRAPH_NAME "class_gm_graph"

SgName thisName = "this";



/**
   These are class declarations which has to be converted to 
   structs.
 **/
std::vector<SgClassDeclaration*> classSymbolsUsed;


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
std::ofstream outFile;
std::ofstream decFile;

SgFunctionParameterList* copyFunctionParameterList(SgFunctionParameterList* origList);
void getSuitableFunctionName(FunctionClassDetails* cur, 
			     SgName&  newName);
SgType* getReturnType(SgFunctionDeclaration* curFn);
SgFunctionDeclaration * copyFunctionDeclaration(FunctionClassDetails* cur);
void unparseCPPScopetoCScope(SgBasicBlock *originalScope, SgBasicBlock *newScope);
void unparseCPPtoCandPrint(SgFunctionDefinition * originalFunction, 
			   SgFunctionDeclaration* newFunctionDeclaration);


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
  SgScopeStatement* outerMostScope = classDef->get_scope(); 
  ROSE_ASSERT(outerMostScope != NULL);
  SgTemplateInstantiationDefn* tempDefinition = isSgTemplateInstantiationDefn(classDef);
  SgTemplateClassDefinition* templateDef = isSgTemplateClassDefinition(classDef);
  
  if(tempDefinition != NULL || templateDef != NULL)
    return;
  SgDeclarationStatementPtrList  decStatements = classDef->get_members();
  if(decStatements.size() == 0)
    return;

  std::string className = classDef->get_declaration()->get_name();
  ROSE_ASSERT(!className.empty());
  std::cout<<"The class unparsed is "<<className<<std::endl;

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
    std::cout<<"the declaration to be added is "<<newDec->unparseToCompleteString()<<std::endl;
    SgVariableDeclaration* varDec =  isSgVariableDeclaration(newDec);
    if(varDec != NULL) {
      newStatement = copyStatement(varDec);
      ROSE_ASSERT(newStatement != NULL);
      //      appendStatement(newStatement, structDefScope);
    }
  }
  
  decFile<<classtoStructDecl->unparseToCompleteString()<<std::endl;  

  decFile<<"typedef struct "<<className<<" "<<className<<";\n"; 
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
  

  /**
     Collect all  function calls made
   **/

  Rose_STL_Container<SgNode*> funCallList =
    NodeQuery::querySubTree(newScope, V_SgFunctionCallExp);

  /**
     For all function calls made make changes on class member function calls.
   **/
  Rose_STL_Container<SgNode*>::iterator itr = funCallList.begin();
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
      bool undefined = false;
      if(defDec != NULL) {
	ROSE_ASSERT(fnSymbol != NULL);
	//std::cout<<"The called function name is "<<fnSymbol->get_name()<<std::endl;
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
	    std::cout<<"SgDot Expression is NULL "<<get_name(callExpr)<<std::endl;

	    objectExpr = 
	      buildVarRefExp (thisName, newScope);
	    //buildStringVal(std::string value=""
	  }


	 
	  if(undefined == false) {
	  ROSE_ASSERT(objectExpr != NULL);
	  SgMemberFunctionDeclaration* classFunctionDec = 
	    isSgMemberFunctionDeclaration(defDec);
	  ROSE_ASSERT(classFunctionDec != NULL);
	  classDec  = 
	    classFunctionDec->get_associatedClassDeclaration();
	  ROSE_ASSERT(classDec != NULL);
	  
	  SgName functionName = 
	    classDec->get_name() + "_" +  fnSymbol->get_name();
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
	  replaceExpression(callExpr, newfunctionCallExpr);
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
	    std::cout<<"The function without declaration is "<<fnDec->get_name()<<std::endl;
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
	    //  std::cout<<"The new function added "<<fnDec->get_name()<<std::endl;
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

//   for (SgStatementPtrList::iterator k = 
// 	 bodyStatements.begin(); k < bodyStatements.end(); ++k) {
//     SgStatement* curStatement = *k;
//     SgBasicBlock* scopeStatement = isSgBasicBlock(curStatement);   
//     bool handled = false;
    
    
//     if(scopeStatement != NULL) {
//       SgBasicBlock *newScopeStatement = buildBasicBlock();
//       unparseCPPScopetoCScope(scopeStatement, newScopeStatement);
//       newStatement = newScopeStatement;
//       handled = true;
//     }
    
//     SgExprStatement* exprStatement = isSgExprStatement(curStatement);
    
//     if(exprStatement != NULL) {

//       SgFunctionCallExp* functionCall = 
// 	isSgFunctionCallExp(exprStatement->get_expression());
//       std::cout<<"The expression is of type "
// 	       <<exprStatement->get_expression()->class_name()<<std::endl;
//       SgAssignOp* assignOp = isSgAssignOp(exprStatement->get_expression());
//       SgExpression* lhsOp = NULL;
//       if(assignOp != NULL) {
// 	functionCall = 
// 	  isSgFunctionCallExp(assignOp->get_rhs_operand());
// 	lhsOp = assignOp->get_lhs_operand();
// 	if(functionCall != NULL) {
// 	  ROSE_ASSERT(lhsOp != NULL);
// 	  std::cout<<"The RHS expression type is"
// 		   <<assignOp->get_rhs_operand()->class_name()<<std::endl;
// 	} else {
// 	  lhsOp = NULL;
// 	}
//       }
//       return;
// }





int main ( int argc, char** argv )
{

#ifdef GREENMARL_CONVERSION
  char* fnName = argv[argc-1];
  SgProject* project = frontend(argc-1,argv);
#else
  char* fnName = "main";
  SgProject* project = frontend(argc,argv);
#endif

  
  
  outermostScope =  SageInterface::getFirstGlobalScope(project);
  printf("// The Function name is %s \n", fnName);
  SgFunctionDeclaration * fndefinition;  
  std::string filename = fnName + std::string("_cconvert.c");
  std::string tempfile = fnName + std::string("_definitions.c");

  decFile.open(tempfile.c_str());
  outFile.open(filename.c_str());
  outFile<<"/*** HELLO WORLD ***/\n#include<stdio.h>\n#include<stdlib.h>\n #include <omp.h>\n";

  // SgClassDeclaration* classtoStructDecl = NULL;
  // classtoStructDecl = buildStructDeclaration("hello", outermostScope);
  // ROSE_ASSERT(classtoStructDecl!= NULL);
  
  // outFile<<classtoStructDecl->unparseToString()<<std::endl;
  
  //** Collect all Class declarations and append.


  SgFilePtrList & ptr_list = project->get_fileList();

  
  for (SgFilePtrList::iterator iter = ptr_list.begin(); iter!=ptr_list.end();
       iter++){ 
    SgFile* sageFile = (*iter);
    std::cout<<"The filename is "<<sageFile->getFileName ()<<std::endl;
  }
  

  /*
  for (SgFilePtrList::iterator iter = ptr_list.begin(); iter!=ptr_list.end();
       iter++){ 
    SgFile* sageFile = (*iter);
    std::cout<<"The filename is "<<sageFile->getFileName ()<<std::endl;
    SgSourceFile * sfile = isSgSourceFile(sageFile);
    ROSE_ASSERT(sfile);
    SgGlobal *root = sfile->get_globalScope();
    SgDeclarationStatementPtrList& classDeclList = root->get_declarations ();
    bool hasOpenMP = false;

    for (SgDeclarationStatementPtrList::iterator p = classDeclList.begin(); 
	 p != classDeclList.end(); ++p) {
      SgClassDeclaration* classDec = isSgClassDeclaration(*p);
      if(classDec != NULL) {
	SgClassDefinition* classDef = classDec->get_definition();
	if(classDef != NULL) {
	  unparseClasstoStruct(classDef);
	}
      }
    } 
  }
  */
  
  for (SgFilePtrList::iterator iter = ptr_list.begin(); iter!=ptr_list.end();
       iter++){ 
    SgFile* sageFile = (*iter);
    SgSourceFile * sfile = isSgSourceFile(sageFile);
    ROSE_ASSERT(sfile);
    SgGlobal *root = sfile->get_globalScope();
    SgDeclarationStatementPtrList& declList = root->get_declarations ();
    bool hasOpenMP= false; // flag to indicate if omp.h is needed in this file
    
    //For each function body in the scope
    for (SgDeclarationStatementPtrList::iterator p = declList.begin(); 
	 p != declList.end(); ++p) {
      SgFunctionDeclaration *func = isSgFunctionDeclaration(*p);
      // 
      if (func == 0)  continue;
      SgFunctionDefinition *defn = func->get_definition();
      if (defn == 0)  continue;
      //ignore functions in system headers, Can keep them to test robustness
      if (defn->get_file_info()->get_filename() !=
	  sageFile->get_file_info()->get_filename())
	continue;
      // std::cout<<"The function name is "
      // 	       <<SageInterface::get_name(func)<<std::endl;  
      if(SageInterface::get_name(func).compare(fnName) == 0 ) {
	//	std::cout<<" Found it"<<std::endl; 
	SgBasicBlock *body = defn->get_body();
	ROSE_ASSERT(body != NULL);
	fndefinition = func;
	
	FunctionClassDetails* baseDec = new FunctionClassDetails(func, 
								 NULL, NULL);	
	SgFunctionDeclaration* newFnDeclaration = 
	  copyFunctionDeclaration(baseDec);
	
	unparseCPPtoCandPrint(defn, newFnDeclaration);
	
      }   
    }
  }
  
  int itr = 0;
  
  while(itr < baseFunctionList.size()) {
    // if(baseFunctionList.empty() == false) {
    //   std::vector<FunctionClassDetails*>::iterator it =  
    //                               begin();
    //   for(; it != baseFunctionList.end(); it++) {
    // ROSE_ASSERT(*it != NULL);
    // ROSE_ASSERT((*it)->getDec() != NULL);
    FunctionClassDetails* curPtr = baseFunctionList[itr];
    ROSE_ASSERT(curPtr != NULL);
    SgFunctionDeclaration* declaration = curPtr->getDec();
    ROSE_ASSERT(declaration != NULL);
    // std::cout<<"The function to find name is "
    // 	       <<declaration->get_name()<<std::endl;
    SgFunctionDefinition* def = declaration->get_definition();
    
    ROSE_ASSERT(def != NULL);
    SgBasicBlock *body = def->get_body();
    ROSE_ASSERT(body != NULL);
    SgFunctionDeclaration* newFnDeclaration = 
      copyFunctionDeclaration(curPtr);	
    unparseCPPtoCandPrint(def, newFnDeclaration);
    itr++;
  }

#ifdef GREENMARL_CONVERSION
  //}
  // Adding main file
  // having a base function call.
  //
  
  std::string argumentList = "(";
  // TODO add argument list;

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
    outFile << str;
  }

  outFile.close();
  definitionFile.close();
  
  ROSE_ASSERT(outermostScope != NULL);
  insertStatementBefore(getFirstStatement(outermostScope),
  			baseFunctionDeclaration);
 
  isSgStatement(fndefinition->get_parent())->remove_statement(fndefinition); 

  return backend(project);

}


/**
   Copy an function into our output file.
 **/

void unparseCPPtoCandPrint(SgFunctionDefinition * originalFunction, SgFunctionDeclaration* newFunctionDeclaration) {
  SgBasicBlock *fnBody  = originalFunction->get_body();
  ROSE_ASSERT(fnBody != NULL);
  SgFunctionDefinition *newFnDefinition = newFunctionDeclaration->get_definition();
  SgBasicBlock *newBody  = newFnDefinition->get_body();
  ROSE_ASSERT(newBody != NULL);
  


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
