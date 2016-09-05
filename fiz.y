/*
 * CS-252 Spring 2015
 * fiz.y: parser for the FIZ interpreter
 *
 * This parser compiles the following grammar:
 * (inc (inc 2))
 *
 */

/********************************************************************************
 * Beginning of Section 1: Definition of tokens and non-terminal in the grammar *
 ********************************************************************************/ 

// Tokens are defined here.
// The lexical analyzer, specified in fiz.l, will read input and generate a stream of tokens
// More tokens need to be added
%token <number_val> NUMBER 
%token INC OPENPAR CLOSEPAR
%token DEC HALT IFZ DEFINE
%token <string_val> ID

// This defines what value will be returned after parsing an expression
%type <node_val> expr 

%union	{
		char   *string_val;				// Needed when identifier is used
		int		number_val;
		struct TREE_NODE *node_val;
}

%{
/********************************************************************************
 * Beginning of Section 2: C data type and global variable definitions to be    *
 *  included in the generated y.tab.c file                                      *
 ********************************************************************************/ 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void yyerror(const char * s);
void prompt();
int yylex();
//void free_node(struct TREE_NODE *node);

// Maximum number of function definitions the interpreter allow
#define MAX_FUNCTIONS 1000

// Maximum number of arguments a function can have
#define MAX_ARGUMENTS 10


// Allowed node types in the syntax tree; more need to be added for the full language
enum NODE_TYPE
{
	NUMBER_NODE,
	ID_NODE,
	INC_NODE,		// corresponds to (inc exp)	
	DEC_NODE,
	IFZ_NODE,
	HALT_NODE,
	DEFINE_NODE,
	CALL_NODE,
};

// Below is the data type for a node in the syntax tree
struct TREE_NODE
{
	enum NODE_TYPE type;
	int nArgs;
	struct TREE_NODE *args[MAX_ARGUMENTS];
	int resolvedArgs[MAX_ARGUMENTS];

	//FOR ID_NODE
	char *idValue;
	int indexValue;

	//FOR NUMBER_NODE
	int intValue;

	//FOR DEFINE_NODE
	struct FUNC_DECL *func;

	//FOR CALL_NODE
	struct TREE_NODE *def;
};

// Information we maintain for each defined function
struct FUNC_DECL	
{
	char *name;				// Function name
	int numArg;
	char* funcArg[MAX_ARGUMENTS];
	struct TREE_NODE *definition;
	int resolved;				//>0 is resolved
	int defined;				//>0 is defined
};

char* ids[10];
int numIDs = 0;

struct TREE_NODE *nodes[10];
int nodeCount = 0;
int nodePlace = 0;

struct FUNC_DECL functions[MAX_FUNCTIONS];
int numFuncs = 0;

int err_value = 0;

struct FUNC_DECL * find_function(char *name);

// Resolve the usage of functions and arguments in an expression
void resolve(struct TREE_NODE *node, struct FUNC_DECL *cf);

// Evaluate an expression in a particular environment (which provides values for the actual arguments)
int eval(struct TREE_NODE * node, int *env);

%}

%%
/********************************************************************************
 * Beginning of Section 3: Grammar production rule definitions and associated   *
 *  C code                                                                      *
 ********************************************************************************/ 

statements:
 statement 
|
 statement statements
;

statement:
  expr
  {
	err_value = 0;
	resolve($1, NULL);
	if (err_value == 0) {
		int num = eval($1, NULL);
		if (num != -1) {
			printf ("%d\n", num);
		}
	}
	numIDs = 0;
	nodeCount = 0;
	prompt();
  }
;

lists:
 ID {
	ids[numIDs++] = $1;
 }
|
 ID {
	ids[numIDs++] = $1;
 } lists
;

exprs:
 expr {
	nodes[nodeCount++] = $1;
 } 
|
 expr {
	nodes[nodeCount++] = $1;
 } exprs
;

expr: 
  NUMBER
  {
		struct TREE_NODE * node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node -> type = NUMBER_NODE;
		node -> intValue = $1;
		$$ = node;
  }
|
  ID
  {
		struct TREE_NODE * node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node -> type = ID_NODE;
		node -> idValue = $1;
		$$ = node;
  }
|
  OPENPAR INC expr CLOSEPAR
    {   
		struct TREE_NODE *node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node-> type = INC_NODE;
		node-> args[0] = $3;
		node -> nArgs = 1;
		$$ = node;   
	}
|
  OPENPAR DEC expr CLOSEPAR
    {   
		struct TREE_NODE *node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node-> type = DEC_NODE;
		node-> args[0] = $3;
		node -> nArgs = 1;
		$$ = node;     
	}
|
  OPENPAR IFZ expr expr expr CLOSEPAR
    {
    		struct TREE_NODE *node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node-> type = IFZ_NODE;
		node-> args[0] = $3;
		node-> args[1] = $4;
		node-> args[2] = $5;
		node -> nArgs = 3;
		$$ = node;
	}
|
  OPENPAR HALT CLOSEPAR
    {   
		struct TREE_NODE *node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node-> type = HALT_NODE;
		$$ = node;  
	}
|
  OPENPAR DEFINE OPENPAR ID lists CLOSEPAR expr CLOSEPAR
	{
		struct TREE_NODE *node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node-> type = DEFINE_NODE;

		struct FUNC_DECL *f = find_function($4);
		if (f == NULL) {
			if (numFuncs == MAX_FUNCTIONS) {
				printf("Too many defined functions\n");
				exit(0);
			}
			f = (struct FUNC_DECL *) malloc(sizeof(struct FUNC_DECL));
			f-> name = $4;
			f->numArg = numIDs;
		} else {
			//error and exit
			printf("Already defined\n");
			exit(0);
		}

		//lists
		int i = 0;
		for (i; i < f->numArg; i++) {
			f-> funcArg[i] = ids[i];
		}
		node->nArgs = f->numArg;
		numIDs = 0;

		//expr
		f-> definition = $7;
		f->resolved = 0;
		node->func = f;
		functions[numFuncs++] = *f;
		$$ = node;
	}
|
  OPENPAR ID exprs CLOSEPAR
	{
		struct TREE_NODE *node = (struct TREE_NODE *) malloc(sizeof(struct TREE_NODE));
		node-> type = CALL_NODE;
		node-> idValue = $2;

		struct FUNC_DECL *f = find_function($2);
		if (f != NULL) {
			node->nArgs = f->numArg;
		} else {
			node->nArgs = nodeCount;
		}
		int i = 0;
		for (i; i < node->nArgs; i++) {
			node->args[i] = nodes[nodeCount - node->nArgs + i];
		}
		nodeCount = nodeCount - node->nArgs;

		$$ = node;
	}
;

%%
/********************************************************************************
 * Beginning of Section 4: C functions to be included in the y.tab.c.           *
 ********************************************************************************/ 

struct FUNC_DECL * find_function(char *name)
{
    int i;
	for (i=0; i<numFuncs; i++) {
		if (! strcmp(functions[i].name, name))
			return &functions[i];
	}
	return NULL;
} 

void resolve(struct TREE_NODE *node, struct FUNC_DECL *cf)
{
	int i = 0;
	switch(node->type)
	{
		case ID_NODE:
			for(i; i < cf->numArg; i++) {
				if (strcmp(cf->funcArg[i], node->idValue) == 0) {
					node->indexValue = i;
					return;
				}
			}
			printf("ID not specified.\n");
			exit (0);
			return;

		case INC_NODE:
			resolve(node->args[0], cf);
			return;

		case DEC_NODE:
			resolve(node->args[0], cf);
			return;

		case IFZ_NODE:
			resolve(node->args[0], cf);
			resolve(node->args[1], cf);
			resolve(node->args[2], cf);
			return;

		case DEFINE_NODE:
			node->func->resolved = 1;
			resolve(node->func->definition, node->func);
			return;

		case CALL_NODE:
			i = 0;
			for (i; i < node->nArgs; i++) {
				resolve(node->args[i], cf);
			}
			return;
	}
	return;
}

//Evaluates an expression node
int eval(struct TREE_NODE * node, int *env)
{
	int i = 0;
	struct FUNC_DECL *f;
	switch(node->type)
	{
		case NUMBER_NODE:
			return node->intValue;

		case ID_NODE:
			return env[node->indexValue];

		case INC_NODE:
			return eval(node->args[0], env) + 1;

		case DEC_NODE:
			if (eval(node->args[0], env) == 0) {
				printf("Attempt to (dec 0).\n");
				exit (0);
			}
			return eval(node->args[0], env) - 1;

		case IFZ_NODE:
			if (eval(node->args[0], env) == 0) {
				return eval(node->args[1], env);
			} else {
				return eval(node->args[2], env);
			}

		case HALT_NODE:
			printf("Halted.\n");
			exit (0);
			return;

		case DEFINE_NODE:
			return -1;

		case CALL_NODE:
			f = find_function(node->idValue);
			if (f != NULL) {
				if (f->numArg == node->nArgs) {
					for (i; i < node->nArgs; i++) {
						node->resolvedArgs[i] = (eval(node->args[i], env));
					}
					return eval(f->definition, node->resolvedArgs);
				} else {
					printf("Not the same number of parameters\n");
					exit(0);
				}
			} else {
				//error and exit
				printf("Not defined yet\n");
				exit(0);
			}
	}
}


void yyerror(const char * s)
{
	fprintf(stderr,"%s", s);
}

void prompt()
{
    printf("fiz> ");
}

main()
{
    prompt();
    yyparse();
    return 0;
}
