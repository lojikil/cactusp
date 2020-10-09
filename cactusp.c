/* Cactus+ is a tiny, public-domain, Q-like system with some ideas from Logo
 * & a smattering of IDL. It was written in less than four hours, has many warts, and
 * is not really the best language to use. It was really meant as a venting from Digamma
 * & all the fun that it entails. Enjoy!
 * Like K/Q, user-defined functions are monadic, but lambdas do a destructuring bind
 * on vectors passed in, like Q. So:
 * ---------------------
 * > f : {[x y] + x y}
 * f : closure
 * > f[3 4]
 * 7
 * ---------------------
 * () represent cons-able lists, [] represent vectors, { } closures.
 * I like Scheme better (homoiconicity is great), but this was a fun
 * write.
 */
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <string.h>

#define nil NULL
#define nul '\0'

#define hmalloc GC_MALLOC
#define hmalloc_atomic GC_MALLOC_ATOMIC
#define hfree GC_FREE
#define hrealloc GC_REALLOC

typedef enum
{
	NUMBER, LATOM, STRINGVEC, OVEC, CVEC, OPAR, CPAR, LCHAR, BOOL, OCUR, CCUR, EOL
} LexType;

typedef struct
{
	char *datum;
	LexType type;
} Lexeme;

typedef enum 
{
	INTEGER, BIT, REAL, RATIONAL, CHAR, VECTOR, ATOM, CLOSURE
} ResultType;
/* no closure holder yet... */
typedef struct _RESULT
{
	ResultType type;
	union
	{
		int z;
		float r;
		char c;
		char *atom;
		struct
		{
			int numerator;
			int denomentator;
		} ratio;
		struct
		{
			struct _RESULT **vdata;
			int vlen;
		} vec;
	} object;
} Result;


typedef struct _SYM
{
	int nlen;
	char *name;
	Result *res;
	struct _SYM *next;
} Symbol;

typedef struct _ESTK
{
	int len;
	union
	{
		Result *res;
		char *name;
	} object;
	struct _ESTK *next;
} EvalStack;

static int note = 0;
static char *ebuf = nil;

Lexeme* _lex(FILE *);
Result *eval(FILE *, Symbol *);
void princ(Result *);
char *hstrdup(char *); /* hmalloc-based strdup */
EvalStack *push(EvalStack *,Result *,char *);
EvalStack *pop(EvalStack *,Result **, char **);
Result *makenumber(Lexeme *);
Result *makevector(Lexeme *); /* string vector & regular? */
Result *makeatom(Lexeme *);

int
main()
{
	Result *h = nil;
	Symbol *cenv = nil;
	GC_INIT();
	if((ebuf = hmalloc(sizeof(char) * 64)) == nil)
	{
		printf("%%main-e-malloc: cannot gc_malloc ebuf!\n");
		return 1;
	}
	printf("   _  _\n\
  | || | _\n\
 -| || || |\n\
  | || || |-\n\
   \\_  || |\n\
     |  _/\n\
    -| | \n\
     |_|-\n\
     Cactus+ v 0.1\n");

	while(1)
	{
		if(note != 0)
		{
			if(note == 1)
				break;
			else /* one signals quit */
			{
				printf(" Error %d: %s\n",note, ebuf);
				note = 0;
				continue;
			}
		}
		puts("> ");
		break;
		h = eval(stdin,cenv);
		princ(h);
	}
	return 0;
}
Lexeme *
_lex(FILE *fdin)
{
	int iter = 0, state = 0, inp = 0;
	static char *buf = nil;
	Lexeme *ret = nil;
	if(buf == nil)
	{
		if((buf = hmalloc(sizeof(char) * 2048)) == nil)
		{
			printf("_lex-e-hmalloc: cannot gc_malloc buf!\n");
			return nil;
		}
	}
	if((ret = (Lexeme *)hmalloc(sizeof(Lexeme))) == nil)
	{
		printf("%%_lex-e-hamlloc: cannot gc_malloc ret!\n");
		return nil;
	}
	while(1)
	{
		switch(state)
		{
			case 0:
				inp = fgetc(fdin);
				while(inp == ' ' || inp == '\t') inp = fgetc(fdin);
				if(inp == '$')
				{
					while(inp != '\n') inp = fgetc;
					state = 0;
				}
				else if(inp >= '0' && inp <= '9')
					state = 1;
				else if(inp == '"')
					state = 2;
				else if(inp == '[')
				{
					ret->type = OVEC;
					return ret;
				}
				else if(inp == ']')
				{
					ret->type = CVEC;
					return ret;
				}
				else if(inp == '{')
				{
					ret->type = OCUR;
					return ret;
				}
				else if(inp == '}')
				{
					ret->type = CCUR;
					return ret;
				}
				else if(inp == '(')
				{
					ret->type = OPAR;
					return ret;
				}
				else if(inp == ')')
				{
					ret->type = CPAR;
					return ret;
				}
				else if(inp == '\n' || inp == ';')
				{
					ret->type = EOL;
					return ret;
				}
				else if(inp == '#')
					state = 3;
				else /* an atom of some sort */
					state = 4;
				break;
			case 1: /* a supposed number */
				buf[iter++] = inp;
				inp = fgetc(fdin);
				while((inp >= '0' && inp <= '9') || inp == '.') 
				{
					buf[iter++] = inp;
					inp = fgetc(fdin);
				}
				buf[iter] = nul;
				ret->datum = hstrdup(buf);
				ret->type = NUMBER;
				return ret;
				break;
			case 2:
				inp = fgetc(fdin);
				while(inp != '\"')
				{
					buf[iter++] = inp;
					inp = fgetc(fdin);
				}
				buf[iter] = nul;
				ret->datum = hstrdup(buf);
				ret->type = STRINGVEC;
				return ret;
				break;
			case 3:
				inp = fgetc(fdin);
				switch(inp)
				{
					case 't':
						/* true */
						ret->type = BOOL;
						ret->datum = hstrdup("t");
						return ret;
						break;
					case 'f':
						/* false */
						ret->type = BOOL;
						ret->datum = hstrdup("f");
						return ret;
						break;
					default:
						/* error */
						break;
				}
				break;
			case 4:
				buf[iter++] = inp;
				inp = fgetc(fdin);
				while((inp >= 'a' && inp <= 'z') || (inp >= 'A' && inp <= 'Z') || inp == '_' || (inp >= '<' && inp <= '@') || inp == '|' || inp == '+' || inp == '*' || inp == '-' || inp == ':')
				{
					buf[iter++] = inp;
					inp = fgetc(fdin);
				}
				buf[iter] = nul;
				ret->type = LATOM;
				ret->datum = hstrdup(buf);
				return ret;
				break;
		}
	}
}
/* Recursive Descent Evaluation ^_^ */
Result *
eval(FILE *fdin, Symbol *env)
{
	Result *ret = nil, *rtmp = nil;
	Lexeme *lex = nil;
	Symbol *tmp = nil;
	EvalStack ev = nil;
	int state = 0, sig = 0;
	while(!sig)
	{
		lex = _lex(fdin);
		printf("%s %d\n",lex->datum,lex->type);
		switch(state)
		{
			case 0:
				switch(lex->type)
				{
					case NUMBER:
						/* make a number, push onto the eval stack */
						rtmp = makenumber(lex);
						ev = push(ev,rtmp,nil);
						state = 0;
						break;
					case STRINGVEC:
						/* push onto stack */
						rtmp = makevector(lex);
						ev = push(ev,rtmp,nil);
						state = 0;
						break;
					case OVEC:
						/* read the vector, then onto the stack */
						state = 1;
						break;
					case OCUR:
						/* read a closure, then onto the stack */
						state = 2;
						break;
					case OPAR:
						/* cons-able list */
						state = 3;
						break;
					case LATOM:
						/* some form of an atom, possibly : or some ident */
						rtmp = makeatom(lex);
						ev = push(ev,rtmp,nil);
						state = 0;
						break;
					case EOL:
						state = 4;
						break;
					default:
						/* error */
						state = 5;
						break;
				}
				break;
			case 1: 
				break;
			case 2:
				break;
			case 3:
				break;
			case 4:
				sig = 1;
				break;
			case 5:
				printf("Error!\n");
				return nil;
		}
	}
  /* eval stack here */
}
Result *
makenumber(Lexeme *l)
{
 Result *ret = nil;
 int iter = 0, ztmp = 0;
 float rtmp = 0.0f;
 if((ret = (Result *)hmalloc(sizeof(Result))) == nil)
 {
   printf("%%makenumber-e-hmalloc: cannot gcmalloc ret!\n");
   return nil;
 }
 while(l->datum[iter] != nul)
 {
   ztmp = ztmp * 10 + (l->datum[iter] - '0');
   iter++;
 }
 ret->type = INTEGER;
 ret->object.z = ztmp;
 return ret;
}
Result *
makevector(Lexeme *l)
{
  int iter = 0, len = 0;
  Result *ret = nil;
  if(l == nil)
    return nil;
  len = strlen(l->datum);
  if((ret = (Result *)hmalloc(sizeof(Result))) == nil)
  {
    printf("%%makevector-e-hmalloc: cannot gcmalloc result!\n");
    return nil;
  }
  ret->type = VECTOR;
  ret->object.vec.vlen = len;
  while(iter < len)
  {

  }
}
Result *
makeatom(Lexeme *l)
{
	Result *ret = nil;
	if((ret = (Result *)hmalloc(sizeof(Result))) == nil)
	{
		printf("%%makeatom-e-hmalloc: cannot gc_malloc ret!\n");
		return nil;
	}
	ret->type = ATOM;
	ret->object.atom = hstrdup(l->datum);
	return ret;
}
char *
hstrdup(char *src)
{
	int iter = 0, len = strlen(src);
	char *ret = nil;
	if((ret = (char *)hmalloc(sizeof(char) * len + 1)) == nil)
	{
		printf("%%hstrdup-e-hmalloc: cannot hmalloc ret!\n");
		return nil;
	}
	while(src[iter] != nul)
	{
		ret[iter] = src[iter];
		iter++;
	}
	ret[iter] = nul;
	return ret;
}
void 
princ(Result *r)
{
	
}
EvalStack *
push(EvalStack *es,Result *r,char *s)
{
	
}
EvalStack *
pop(EvalStack *es,Result **r, char **s)
{
	
}
