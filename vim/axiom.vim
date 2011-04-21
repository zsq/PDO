" Vim syntax file
" Language:	Axiom
" Maintainer:	Johannes L. Grabmeier, johannes.grabmeier@fh-deggendorf.de
" Last Change:	2006-12-18


"for Axiom

syntax clear
syntax case match
set iskeyword=@,=,>,:,-,+,\|,\$,\@


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" A bunch of useful Axiom keywords
syn keyword	axiomStatement	goto break return 
syn keyword	axiomRepeat	while for do repeat
syn keyword	axiomTodo	contained TODO FIXME XXX

" cCommentGroup allows adding matches for special things in comments
syn cluster	cCommentGroup	contains=axiomTodo

" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match	cSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
if !exists("c_no_utf")
  syn match	cSpecial	display contained "\\\(u\x\{4}\|U\x\{8}\)"
endif
if exists("c_no_cformat")
  syn region	cString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,@Spell
  " cCppString: same as cString, but ends at end of line
  syn region	cCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial,@Spell
else
  syn match	cFormat		display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hjlLtz]\|ll\)\=\([bdiuoxXDOUfeEgGcCsSpn]\|\[\^\=.[^]]*\]\)" contained
  syn match	cFormat		display "%%" contained
  syn region	cString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,cFormat,@Spell
  " cCppString: same as cString, but ends at end of line
  syn region	cCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial,cFormat,@Spell
endif

syn match	cCharacter	"L\='[^\\]'"
syn match	cCharacter	"L'[^']*'" contains=cSpecial
if exists("c_gnu")
  syn match	cSpecialError	"L\='\\[^'\"?\\abefnrtv]'"
  syn match	cSpecialCharacter "L\='\\['\"?\\abefnrtv]'"
else
  syn match	cSpecialError	"L\='\\[^'\"?\\abfnrtv]'"
  syn match	cSpecialCharacter "L\='\\['\"?\\abfnrtv]'"
endif
syn match	cSpecialCharacter display "L\='\\\o\{1,3}'"
syn match	cSpecialCharacter display "'\\x\x\{1,2}'"
syn match	cSpecialCharacter display "L'\\x\x\+'"

"when wanted, highlight trailing white space
if exists("c_space_errors")
  if !exists("c_no_trail_space_error")
    syn match	cSpaceError	display excludenl "\s\+$"
  endif
  if !exists("c_no_tab_space_error")
    syn match	cSpaceError	display " \+\t"me=e-1
  endif
endif

"catch errors caused by wrong parenthesis and brackets
" also accept <% for {, %> for }, <: for [ and :> for ] (C99)
" But avoid matching <::.
syn cluster	cParenGroup	contains=cParenError,cIncluded,cSpecial,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cUserCont,cUserLabel,cBitField,cCommentSkip,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom
if exists("c_no_bracket_error")
  syn region	cParen		transparent start='(' end=')' contains=ALLBUT,@cParenGroup,cCppParen,cCppString,@Spell
  " cCppParen: same as cParen but ends at end-of-line; used in cDefine
  syn region	cCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@cParenGroup,cParen,cString,@Spell
  syn match	cParenError	display ")"
  syn match	cErrInParen	display contained "[{}]\|<%\|%>"
else
  syn region	cParen		transparent start='(' end=')' contains=ALLBUT,@cParenGroup,cCppParen,cErrInBracket,cCppBracket,cCppString,@Spell
  " cCppParen: same as cParen but ends at end-of-line; used in cDefine
  syn region	cCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@cParenGroup,cErrInBracket,cParen,cBracket,cString,@Spell
  syn match	cParenError	display "[\])]"
  syn match	cErrInParen	display contained "[\]{}]\|<%\|%>"
  syn region	cBracket	transparent start='\[\|<::\@!' end=']\|:>' contains=ALLBUT,@cParenGroup,cErrInParen,cCppParen,cCppBracket,cCppString,@Spell
  " cCppBracket: same as cParen but ends at end-of-line; used in cDefine
  syn region	cCppBracket	transparent start='\[\|<::\@!' skip='\\$' excludenl end=']\|:>' end='$' contained contains=ALLBUT,@cParenGroup,cErrInParen,cParen,cBracket,cString,@Spell
  syn match	cErrInBracket	display contained "[);{}]\|<%\|%>"
endif

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	cNumbers	display transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctalError,cOctal
" Same, but without octal error (for comments)
syn match	cNumbersCom	display contained transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctal
syn match	cNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	cNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	cOctal		display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=cOctalZero
syn match	cOctalZero	display contained "\<0"
syn match	cFloat		display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match	cFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	cFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	cFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"
if !exists("c_no_c99")
  "hexadecimal floating point number, optional leading digits, with dot, with exponent
  syn match	cFloat		display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
  "hexadecimal floating point number, with leading digits, optional dot, with exponent
  syn match	cFloat		display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"
endif

" flag an octal number with wrong digits
syn match	cOctalError	display contained "0\o*[89]\d*"
syn case match

if exists("c_comment_strings")
  " A comment can contain cString, cCharacter and cNumber.
  " But a "*/" inside a cString in a cComment DOES end the comment!  So we
  " need to use a special type of cString: cCommentString, which also ends on
  " "*/", and sees a "*" at the start of the line as comment again.
  " Unfortunately this doesn't very well work for // type of comments :-(
  syntax match	cCommentSkip	contained "^\s*\*\($\|\s\+\)"
  syntax region cCommentString	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=cSpecial,cCommentSkip
  syntax region cComment2String	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=cSpecial
  syntax region  cCommentL	start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cComment2String,cCharacter,cNumbersCom,cSpaceError,@Spell
  syntax region cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cCommentString,cCharacter,cNumbersCom,cSpaceError,@Spell
else
  syn region	cCommentL	start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cSpaceError,@Spell
  syn region	cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cSpaceError,@Spell
endif

" keep a // comment separately, it terminates a preproc. conditional
syntax match	cCommentError	display "\*/"
syntax match	cCommentStartError display "/\*"me=e-1 contained


" Axiom comments start with -- or ++:
syntax region  axiomCommentPP	start="++" end="$" 
syntax region  axiomCommentMM	start="--" end="$" 

" Axiom system commands start with ), be aware that normal code starting
" with ) in one line also is interpreted as system command
syntax region  axiomSystemCommand	start="^ *)" end="$" 


syn keyword	cOperator	sizeof
if exists("c_gnu")
  syn keyword	cStatement	__asm__
  syn keyword	cOperator	typeof __real__ __imag__
endif
syn keyword	cType		int long short char void NNI I PI List L
syn keyword	cType		signed unsigned float double
if !exists("c_no_ansi") || exists("c_ansi_typedefs")
  syn keyword   cType		size_t ssize_t wchar_t ptrdiff_t sig_atomic_t fpos_t
  syn keyword   cType		clock_t time_t va_list jmp_buf FILE DIR div_t ldiv_t
  syn keyword   cType		mbstate_t wctrans_t wint_t wctype_t
endif
if !exists("c_no_c99") " ISO C99
  syn keyword	cType		bool complex
  syn keyword	cType		int8_t int16_t int32_t int64_t
  syn keyword	cType		uint8_t uint16_t uint32_t uint64_t
  syn keyword	cType		int_least8_t int_least16_t int_least32_t int_least64_t
  syn keyword	cType		uint_least8_t uint_least16_t uint_least32_t uint_least64_t
  syn keyword	cType		int_fast8_t int_fast16_t int_fast32_t int_fast64_t
  syn keyword	cType		uint_fast8_t uint_fast16_t uint_fast32_t uint_fast64_t
  syn keyword	cType		intptr_t uintptr_t
  syn keyword	cType		intmax_t uintmax_t
endif
if exists("c_gnu")
  syn keyword	cType		__label__ __complex__ __volatile__
endif

syn keyword	cStructure	struct union enum typedef
syn keyword	cStorageClass	static register auto volatile extern const
if exists("c_gnu")
  syn keyword	cStorageClass	inline __attribute__
endif
if !exists("c_no_c99")
  syn keyword	cStorageClass	inline restrict
endif

if !exists("c_no_ansi") || exists("c_ansi_constants") || exists("c_gnu")
  if exists("c_gnu")
    syn keyword cConstant __GNUC__ __FUNCTION__ __PRETTY_FUNCTION__
  endif
  syn keyword cConstant __LINE__ __FILE__ __DATE__ __TIME__ __STDC__
  syn keyword cConstant __STDC_VERSION__
  syn keyword cConstant CHAR_BIT MB_LEN_MAX MB_CUR_MAX
  syn keyword cConstant UCHAR_MAX UINT_MAX ULONG_MAX USHRT_MAX
  syn keyword cConstant CHAR_MIN INT_MIN LONG_MIN SHRT_MIN
  syn keyword cConstant CHAR_MAX INT_MAX LONG_MAX SHRT_MAX
  syn keyword cConstant SCHAR_MIN SINT_MIN SLONG_MIN SSHRT_MIN
  syn keyword cConstant SCHAR_MAX SINT_MAX SLONG_MAX SSHRT_MAX
  if !exists("c_no_c99")
    syn keyword cConstant LLONG_MAX ULLONG_MAX
    syn keyword cConstant INT8_MIN INT16_MIN INT32_MIN INT64_MIN
    syn keyword cConstant INT8_MAX INT16_MAX INT32_MAX INT64_MAX
    syn keyword cConstant UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX
    syn keyword cConstant INT_LEAST8_MIN INT_LEAST16_MIN INT_LEAST32_MIN INT_LEAST64_MIN
    syn keyword cConstant INT_LEAST8_MAX INT_LEAST16_MAX INT_LEAST32_MAX INT_LEAST64_MAX
    syn keyword cConstant UINT_LEAST8_MAX UINT_LEAST16_MAX UINT_LEAST32_MAX UINT_LEAST64_MAX
    syn keyword cConstant INT_FAST8_MIN INT_FAST16_MIN INT_FAST32_MIN INT_FAST64_MIN
    syn keyword cConstant INT_FAST8_MAX INT_FAST16_MAX INT_FAST32_MAX INT_FAST64_MAX
    syn keyword cConstant UINT_FAST8_MAX UINT_FAST16_MAX UINT_FAST32_MAX UINT_FAST64_MAX
    syn keyword cConstant INTPTR_MIN INTPTR_MAX UINTPTR_MAX
    syn keyword cConstant INTMAX_MIN INTMAX_MAX UINTMAX_MAX
    syn keyword cConstant PTRDIFF_MIN PTRDIFF_MAX SIG_ATOMIC_MIN SIG_ATOMIC_MAX
    syn keyword cConstant SIZE_MAX WCHAR_MIN WCHAR_MAX WINT_MIN WINT_MAX
  endif
  syn keyword cConstant FLT_RADIX FLT_ROUNDS
  syn keyword cConstant FLT_DIG FLT_MANT_DIG FLT_EPSILON
  syn keyword cConstant DBL_DIG DBL_MANT_DIG DBL_EPSILON
  syn keyword cConstant LDBL_DIG LDBL_MANT_DIG LDBL_EPSILON
  syn keyword cConstant FLT_MIN FLT_MAX FLT_MIN_EXP FLT_MAX_EXP
  syn keyword cConstant FLT_MIN_10_EXP FLT_MAX_10_EXP
  syn keyword cConstant DBL_MIN DBL_MAX DBL_MIN_EXP DBL_MAX_EXP
  syn keyword cConstant DBL_MIN_10_EXP DBL_MAX_10_EXP
  syn keyword cConstant LDBL_MIN LDBL_MAX LDBL_MIN_EXP LDBL_MAX_EXP
  syn keyword cConstant LDBL_MIN_10_EXP LDBL_MAX_10_EXP
  syn keyword cConstant HUGE_VAL CLOCKS_PER_SEC NULL
  syn keyword cConstant LC_ALL LC_COLLATE LC_CTYPE LC_MONETARY
  syn keyword cConstant LC_NUMERIC LC_TIME
  syn keyword cConstant SIG_DFL SIG_ERR SIG_IGN
  syn keyword cConstant SIGABRT SIGFPE SIGILL SIGHUP SIGINT SIGSEGV SIGTERM
  " Add POSIX signals as well...
  syn keyword cConstant SIGABRT SIGALRM SIGCHLD SIGCONT SIGFPE SIGHUP
  syn keyword cConstant SIGILL SIGINT SIGKILL SIGPIPE SIGQUIT SIGSEGV
  syn keyword cConstant SIGSTOP SIGTERM SIGTRAP SIGTSTP SIGTTIN SIGTTOU
  syn keyword cConstant SIGUSR1 SIGUSR2
  syn keyword cConstant _IOFBF _IOLBF _IONBF BUFSIZ EOF WEOF
  syn keyword cConstant FOPEN_MAX FILENAME_MAX L_tmpnam
  syn keyword cConstant SEEK_CUR SEEK_END SEEK_SET
  syn keyword cConstant TMP_MAX stderr stdin stdout
  syn keyword cConstant EXIT_FAILURE EXIT_SUCCESS RAND_MAX
  " Add POSIX errors as well
  syn keyword cConstant E2BIG EACCES EAGAIN EBADF EBADMSG EBUSY
  syn keyword cConstant ECANCELED ECHILD EDEADLK EDOM EEXIST EFAULT
  syn keyword cConstant EFBIG EILSEQ EINPROGRESS EINTR EINVAL EIO EISDIR
  syn keyword cConstant EMFILE EMLINK EMSGSIZE ENAMETOOLONG ENFILE ENODEV
  syn keyword cConstant ENOENT ENOEXEC ENOLCK ENOMEM ENOSPC ENOSYS
  syn keyword cConstant ENOTDIR ENOTEMPTY ENOTSUP ENOTTY ENXIO EPERM
  syn keyword cConstant EPIPE ERANGE EROFS ESPIPE ESRCH ETIMEDOUT EXDEV
  " math.h
  syn keyword cConstant M_E M_LOG2E M_LOG10E M_LN2 M_LN10 M_PI M_PI_2 M_PI_4
  syn keyword cConstant M_1_PI M_2_PI M_2_SQRTPI M_SQRT2 M_SQRT1_2
endif
if !exists("c_no_c99") " ISO C99
  syn keyword cConstant true false
endif

" Accept %: for # (C99)
syn region	cPreCondit	start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cComment,cCppString,cCharacter,cCppParen,cParenError,cNumbers,cCommentError,cSpaceError
syn match	cPreCondit	display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
if !exists("c_no_if0")
  syn region	cCppOut		start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2
  syn region	cCppOut2	contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=cSpaceError,cCppSkip
  syn region	cCppSkip	contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cSpaceError,cCppSkip
endif
syn region	cIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	cIncluded	display contained "<[^>]*>"
syn match	cInclude	display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=cIncluded
"syn match cLineSkip	"\\$"
syn cluster	cPreProcGroup	contains=cPreCondit,cIncluded,cInclude,cDefine,cErrInParen,cErrInBracket,cUserLabel,cSpecial,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom,cString,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cParen,cBracket,cMulti
syn region	cDefine		start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@cPreProcGroup,@Spell
syn region	cPreProc	start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@cPreProcGroup,@Spell

" Highlight User Labels
syn cluster	cMultiGroup	contains=cIncluded,cSpecial,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cUserCont,cUserLabel,cBitField,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom,cCppParen,cCppBracket,cCppString
syn region	cMulti		transparent start='?' skip='::' end=':' contains=ALLBUT,@cMultiGroup,@Spell
" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster	cLabelGroup	contains=cUserLabel
syn match	cUserCont	display "^\s*\I\i*\s*:$" contains=@cLabelGroup
syn match	cUserCont	display ";\s*\I\i*\s*:$" contains=@cLabelGroup
syn match	cUserCont	display "^\s*\I\i*\s*:[^:]"me=e-1 contains=@cLabelGroup
syn match	cUserCont	display ";\s*\I\i*\s*:[^:]"me=e-1 contains=@cLabelGroup

syn match	cUserLabel	display "\I\i*" contained

" Avoid recognizing most bitfields as labels
syn match	cBitField	display "^\s*\I\i*\s*:\s*[1-9]"me=e-1
syn match	cBitField	display ";\s*\I\i*\s*:\s*[1-9]"me=e-1

if exists("c_minlines")
  let b:c_minlines = c_minlines
else
  if !exists("c_no_if0")
    let b:c_minlines = 50	" #if 0 constructs can be long
  else
    let b:c_minlines = 15	" mostly for () constructs
  endif
endif
exec "syn sync ccomment cComment minlines=" . b:c_minlines

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_c_syn_inits")
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

" new commands for AXIOM


syntax keyword axiomCategory  AGG Aggregate
syntax keyword axiomCategory  HOAGG HomogeneousAggregate
syntax keyword axiomCategory  CLAGG Collection
syntax keyword axiomCategory  BGAGG BagAggregate
syntax keyword axiomCategory  SKAGG StackAggregate
syntax keyword axiomCategory  QUAGG QueueAggregate
syntax keyword axiomCategory  DQAGG DequeueAggregate
syntax keyword axiomCategory  PRQAGG PriorityQueueAggregate
syntax keyword axiomCategory  DIOPS DictionaryOperations
syntax keyword axiomCategory  DIAGG Dictionary
syntax keyword axiomCategory  MDAGG MultiDictionary
syntax keyword axiomCategory  SETAGG SetAggregate
syntax keyword axiomCategory  FSAGG FiniteSetAggregate
syntax keyword axiomCategory  MSETAGG MultisetAggregate
syntax keyword axiomCategory  OMSAGG OrderedMultisetAggregate
syntax keyword axiomCategory  KDAGG KeyedDictionary
syntax keyword axiomCategory  ELTAB Eltable
syntax keyword axiomCategory  ELTAGG EltableAggregate
syntax keyword axiomCategory  IXAGG IndexedAggregate
syntax keyword axiomCategory  TBAGG TableAggregate
syntax keyword axiomCategory  RCAGG RecursiveAggregate
syntax keyword axiomCategory  BRAGG BinaryRecursiveAggregate
syntax keyword axiomCategory  DLAGG DoublyLinkedAggregate
syntax keyword axiomCategory  URAGG UnaryRecursiveAggregate
syntax keyword axiomCategory  STAGG StreamAggregate
syntax keyword axiomCategory  LNAGG LinearAggregate
syntax keyword axiomCategory  FLAGG FiniteLinearAggregate
syntax keyword axiomCategory  A1AGG OneDimensionalArrayAggregate
syntax keyword axiomCategory  ELAGG ExtensibleLinearAggregate
syntax keyword axiomCategory  LSAGG ListAggregate
syntax keyword axiomCategory  ALAGG AssociationListAggregate
syntax keyword axiomCategory  SRAGG StringAggregate
syntax keyword axiomCategory  BTAGG BitAggregate
syntax keyword axiomCategory  FINRALG FiniteRankAlgebra
syntax keyword axiomCategory  FRAMALG FramedAlgebra
syntax keyword axiomCategory  MONOGEN MonogenicAlgebra
syntax keyword axiomCategory  ACF AlgebraicallyClosedField
syntax keyword axiomCategory  ACFS AlgebraicallyClosedFunctionSpace
syntax keyword axiomCategory  NUMINT NumericalIntegrationCategory
syntax keyword axiomCategory  ODECAT OrdinaryDifferentialEquationsSolverCategory
syntax keyword axiomCategory  PDECAT PartialDifferentialEquationsSolverCategory
syntax keyword axiomCategory  OPTCAT NumericalOptimizationCategory
syntax keyword axiomCategory  ARR2CAT TwoDimensionalArrayCategory
syntax keyword axiomCategory  ATTREG AttributeRegistry
syntax keyword axiomCategory  LOGIC Logic
syntax keyword axiomCategory  GRMOD GradedModule
syntax keyword axiomCategory  GRALG GradedAlgebra
syntax keyword axiomCategory  BASTYPE BasicType
syntax keyword axiomCategory  SETCAT SetCategory
syntax keyword axiomCategory  STEP StepThrough
syntax keyword axiomCategory  SGROUP SemiGroup
syntax keyword axiomCategory  MONOID Monoid
syntax keyword axiomCategory  GROUP Group
syntax keyword axiomCategory  ABELSG AbelianSemiGroup
syntax keyword axiomCategory  ABELMON AbelianMonoid
syntax keyword axiomCategory  CABMON CancellationAbelianMonoid
syntax keyword axiomCategory  ABELGRP AbelianGroup
syntax keyword axiomCategory  RNG Rng
syntax keyword axiomCategory  LMODULE LeftModule
syntax keyword axiomCategory  RMODULE RightModule
syntax keyword axiomCategory  RING Ring
syntax keyword axiomCategory  BMODULE BiModule
syntax keyword axiomCategory  ENTIRER EntireRing
syntax keyword axiomCategory  CHARZ CharacteristicZero
syntax keyword axiomCategory  CHARNZ CharacteristicNonZero
syntax keyword axiomCategory  COMRING CommutativeRing
syntax keyword axiomCategory  MODULE Module
syntax keyword axiomCategory  ALGEBRA Algebra
syntax keyword axiomCategory  LINEXP LinearlyExplicitRingOver
syntax keyword axiomCategory  FLINEXP FullyLinearlyExplicitRingOver
syntax keyword axiomCategory  INTDOM IntegralDomain
syntax keyword axiomCategory  GCDDOM GcdDomain
syntax keyword axiomCategory  UFD UniqueFactorizationDomain
syntax keyword axiomCategory  PFECAT PolynomialFactorizationExplicit
syntax keyword axiomCategory  PID PrincipalIdealDomain
syntax keyword axiomCategory  EUCDOM EuclideanDomain
syntax keyword axiomCategory  DIVRING DivisionRing
syntax keyword axiomCategory  FIELD Field
syntax keyword axiomCategory  FINITE Finite
syntax keyword axiomCategory  VSPACE VectorSpace
syntax keyword axiomCategory  ORDSET OrderedSet
syntax keyword axiomCategory  ORDFIN OrderedFinite
syntax keyword axiomCategory  ORDMON OrderedMonoid
syntax keyword axiomCategory  OASGP OrderedAbelianSemiGroup
syntax keyword axiomCategory  OAMON OrderedAbelianMonoid
syntax keyword axiomCategory  OCAMON OrderedCancellationAbelianMonoid
syntax keyword axiomCategory  OAGROUP OrderedAbelianGroup
syntax keyword axiomCategory  ORDRING OrderedRing
syntax keyword axiomCategory  OINTDOM OrderedIntegralDomain
syntax keyword axiomCategory  OAMONS OrderedAbelianMonoidSup
syntax keyword axiomCategory  DIFRING DifferentialRing
syntax keyword axiomCategory  PDRING PartialDifferentialRing
syntax keyword axiomCategory  DIFEXT DifferentialExtension
syntax keyword axiomCategory  TYPE Type
syntax keyword axiomCategory  KOERCE CoercibleTo
syntax keyword axiomCategory  KONVERT ConvertibleTo
syntax keyword axiomCategory  RETRACT RetractableTo
syntax keyword axiomCategory  COMBOPC CombinatorialOpsCategory
syntax keyword axiomCategory  FFCAT FunctionFieldCategory
syntax keyword axiomCategory  LALG LeftAlgebra
syntax keyword axiomCategory  FDIVCAT FiniteDivisorCategory
syntax keyword axiomCategory  DVARCAT DifferentialVariableCategory
syntax keyword axiomCategory  DPOLCAT DifferentialPolynomialCategory
syntax keyword axiomCategory  IEVALAB InnerEvalable
syntax keyword axiomCategory  EVALAB Evalable
syntax keyword axiomCategory  FEVALAB FullyEvalableOver
syntax keyword axiomCategory  FPC FieldOfPrimeCharacteristic
syntax keyword axiomCategory  XF ExtensionField
syntax keyword axiomCategory  FAXF FiniteAlgebraicExtensionField
syntax keyword axiomCategory  FFIELDC FiniteFieldCategory
syntax keyword axiomCategory  FILECAT FileCategory
syntax keyword axiomCategory  FNCAT FileNameCategory
syntax keyword axiomCategory  FORTFN FortranFunctionCategory
syntax keyword axiomCategory  FMC FortranMatrixCategory
syntax keyword axiomCategory  FORTCAT FortranProgramCategory
syntax keyword axiomCategory  FVC FortranVectorCategory
syntax keyword axiomCategory  FMTC FortranMachineTypeCategory
syntax keyword axiomCategory  FMFUN FortranMatrixFunctionCategory
syntax keyword axiomCategory  FVFUN FortranVectorFunctionCategory
syntax keyword axiomCategory  QFCAT QuotientFieldCategory
syntax keyword axiomCategory  FAMONC FreeAbelianMonoidCategory
syntax keyword axiomCategory  ES ExpressionSpace
syntax keyword axiomCategory  FS FunctionSpace
syntax keyword axiomCategory  COMPCAT ComplexCategory
syntax keyword axiomCategory  IDPC IndexedDirectProductCategory
syntax keyword axiomCategory  INTCAT IntervalCategory
syntax keyword axiomCategory  CACHSET CachableSet
syntax keyword axiomCategory  ULSCCAT UnivariateLaurentSeriesConstructorCategory
syntax keyword axiomCategory  MLO MonogenicLinearOperator
syntax keyword axiomCategory  LODOCAT LinearOrdinaryDifferentialOperatorCategory
syntax keyword axiomCategory  MATCAT MatrixCategory
syntax keyword axiomCategory  RMATCAT RectangularMatrixCategory
syntax keyword axiomCategory  SMATCAT SquareMatrixCategory
syntax keyword axiomCategory  MONAD Monad
syntax keyword axiomCategory  MONADWU MonadWithUnit
syntax keyword axiomCategory  NARNG NonAssociativeRng
syntax keyword axiomCategory  NASRING NonAssociativeRing
syntax keyword axiomCategory  NAALG NonAssociativeAlgebra
syntax keyword axiomCategory  FINAALG FiniteRankNonAssociativeAlgebra
syntax keyword axiomCategory  FRNAALG FramedNonAssociativeAlgebra
syntax keyword axiomCategory  PTCAT PointCategory
syntax keyword axiomCategory  RPOLCAT RecursivePolynomialCategory
syntax keyword axiomCategory  NTSCAT NormalizedTriangularSetCategory
syntax keyword axiomCategory  SNTSCAT SquareFreeNormalizedTriangularSetCategory
syntax keyword axiomCategory  OC OctonionCategory
syntax keyword axiomCategory  OM OpenMath
syntax keyword axiomCategory  OREPCAT UnivariateSkewPolynomialCategory
syntax keyword axiomCategory  PADICCT PAdicIntegerCategory
syntax keyword axiomCategory  PATMAB PatternMatchable
syntax keyword axiomCategory  FPATMAB FullyPatternMatchable
syntax keyword axiomCategory  PATAB Patternable
syntax keyword axiomCategory  PPCURVE PlottablePlaneCurveCategory
syntax keyword axiomCategory  PSCURVE PlottableSpaceCurveCategory
syntax keyword axiomCategory  PERMCAT PermutationCategory
syntax keyword axiomCategory  PSETCAT PolynomialSetCategory
syntax keyword axiomCategory  AMR AbelianMonoidRing
syntax keyword axiomCategory  FAMR FiniteAbelianMonoidRing
syntax keyword axiomCategory  POLYCAT PolynomialCategory
syntax keyword axiomCategory  UPOLYC UnivariatePolynomialCategory
syntax keyword axiomCategory  PSCAT PowerSeriesCategory
syntax keyword axiomCategory  UPSCAT UnivariatePowerSeriesCategory
syntax keyword axiomCategory  UTSCAT UnivariateTaylorSeriesCategory
syntax keyword axiomCategory  ULSCAT UnivariateLaurentSeriesCategory
syntax keyword axiomCategory  UPXSCAT UnivariatePuiseuxSeriesCategory
syntax keyword axiomCategory  MTSCAT MultivariateTaylorSeriesCategory
syntax keyword axiomCategory  PTRANFN PartialTranscendentalFunctions
syntax keyword axiomCategory  UPXSCCA UnivariatePuiseuxSeriesConstructorCategory
syntax keyword axiomCategory  QUATCAT QuaternionCategory
syntax keyword axiomCategory  RRCC RealRootCharacterizationCategory
syntax keyword axiomCategory  RCFIELD RealClosedField
syntax keyword axiomCategory  RSETCAT RegularTriangularSetCategory
syntax keyword axiomCategory  FRETRCT FullyRetractableTo
syntax keyword axiomCategory  SEGCAT SegmentCategory
syntax keyword axiomCategory  SEGXCAT SegmentExpansionCategory
syntax keyword axiomCategory  SEXCAT SExpressionCategory
syntax keyword axiomCategory  REAL RealConstant
syntax keyword axiomCategory  RADCAT RadicalCategory
syntax keyword axiomCategory  RNS RealNumberSystem
syntax keyword axiomCategory  FPS FloatingPointSystem
syntax keyword axiomCategory  INS IntegerNumberSystem
syntax keyword axiomCategory  SPACEC ThreeSpaceCategory
syntax keyword axiomCategory  SFRTCAT SquareFreeRegularTriangularSetCategory
syntax keyword axiomCategory  LZSTAGG LazyStreamAggregate
syntax keyword axiomCategory  STRICAT StringCategory
syntax keyword axiomCategory  BTCAT BinaryTreeCategory
syntax keyword axiomCategory  ELEMFUN ElementaryFunctionCategory
syntax keyword axiomCategory  TRIGCAT TrigonometricFunctionCategory
syntax keyword axiomCategory  ATRIG ArcTrigonometricFunctionCategory
syntax keyword axiomCategory  HYPCAT HyperbolicFunctionCategory
syntax keyword axiomCategory  AHYP ArcHyperbolicFunctionCategory
syntax keyword axiomCategory  TRANFUN TranscendentalFunctionCategory
syntax keyword axiomCategory  PRIMCAT PrimitiveFunctionCategory
syntax keyword axiomCategory  LFCAT LiouvillianFunctionCategory
syntax keyword axiomCategory  CFCAT CombinatorialFunctionCategory
syntax keyword axiomCategory  SPFCAT SpecialFunctionCategory
syntax keyword axiomCategory  TSETCAT TriangularSetCategory
syntax keyword axiomCategory  VECTCAT VectorCategory
syntax keyword axiomCategory  DIRPCAT DirectProductCategory
syntax keyword axiomCategory  LIECAT LieAlgebra
syntax keyword axiomCategory  FLALG FreeLieAlgebra
syntax keyword axiomCategory  FMCAT FreeModuleCat
syntax keyword axiomCategory  XALG XAlgebra
syntax keyword axiomCategory  XFALG XFreeAlgebra
syntax keyword axiomCategory  XPOLYC XPolynomialsCat

syntax keyword axiomDomain  ACPLOT PlaneAlgebraicCurvePlot
syntax keyword axiomDomain  SAE SimpleAlgebraicExtension
syntax keyword axiomDomain  DLIST DataList
syntax keyword axiomDomain  ICARD IndexCard
syntax keyword axiomDomain  DBASE Database
syntax keyword axiomDomain  QEQUAT QueryEquation
syntax keyword axiomDomain  NIPROB NumericalIntegrationProblem
syntax keyword axiomDomain  ODEPROB NumericalODEProblem
syntax keyword axiomDomain  PDEPROB NumericalPDEProblem
syntax keyword axiomDomain  OPTPROB NumericalOptimizationProblem
syntax keyword axiomDomain  NONE None
syntax keyword axiomDomain  ANY Any
syntax keyword axiomDomain  PRIMARR PrimitiveArray
syntax keyword axiomDomain  TUPLE Tuple
syntax keyword axiomDomain  IFARRAY IndexedFlexibleArray
syntax keyword axiomDomain  FARRAY FlexibleArray
syntax keyword axiomDomain  IARRAY1 IndexedOneDimensionalArray
syntax keyword axiomDomain  ARRAY1 OneDimensionalArray
syntax keyword axiomDomain  IIARRAY2 InnerIndexedTwoDimensionalArray
syntax keyword axiomDomain  IARRAY2 IndexedTwoDimensionalArray
syntax keyword axiomDomain  ARRAY2 TwoDimensionalArray
syntax keyword axiomDomain  ASP1 Asp1
syntax keyword axiomDomain  ASP10 Asp10
syntax keyword axiomDomain  ASP12 Asp12
syntax keyword axiomDomain  ASP19 Asp19
syntax keyword axiomDomain  ASP20 Asp20
syntax keyword axiomDomain  ASP24 Asp24
syntax keyword axiomDomain  ASP27 Asp27
syntax keyword axiomDomain  ASP28 Asp28
syntax keyword axiomDomain  ASP29 Asp29
syntax keyword axiomDomain  ASP30 Asp30
syntax keyword axiomDomain  ASP31 Asp31
syntax keyword axiomDomain  ASP33 Asp33
syntax keyword axiomDomain  ASP34 Asp34
syntax keyword axiomDomain  ASP35 Asp35
syntax keyword axiomDomain  ASP4 Asp4
syntax keyword axiomDomain  ASP41 Asp41
syntax keyword axiomDomain  ASP42 Asp42
syntax keyword axiomDomain  ASP49 Asp49
syntax keyword axiomDomain  ASP50 Asp50
syntax keyword axiomDomain  ASP55 Asp55
syntax keyword axiomDomain  ASP6 Asp6
syntax keyword axiomDomain  ASP7 Asp7
syntax keyword axiomDomain  ASP73 Asp73
syntax keyword axiomDomain  ASP74 Asp74
syntax keyword axiomDomain  ASP77 Asp77
syntax keyword axiomDomain  ASP78 Asp78
syntax keyword axiomDomain  ASP8 Asp8
syntax keyword axiomDomain  ASP80 Asp80
syntax keyword axiomDomain  ASP9 Asp9
syntax keyword axiomDomain  STACK Stack
syntax keyword axiomDomain  ASTACK ArrayStack
syntax keyword axiomDomain  QUEUE Queue
syntax keyword axiomDomain  DEQUEUE Dequeue
syntax keyword axiomDomain  HEAP Heap
syntax keyword axiomDomain  REF Reference
syntax keyword axiomDomain  BOOLEAN Boolean
syntax keyword axiomDomain  IBITS IndexedBits
syntax keyword axiomDomain  BITS Bits
syntax keyword axiomDomain  CARD CardinalNumber
syntax keyword axiomDomain  CARTEN CartesianTensor
syntax keyword axiomDomain  QFORM QuadraticForm
syntax keyword axiomDomain  CLIF CliffordAlgebra
syntax keyword axiomDomain  COLOR Color
syntax keyword axiomDomain  PALETTE Palette
syntax keyword axiomDomain  ORDCOMP OrderedCompletion
syntax keyword axiomDomain  ONECOMP OnePointCompletion
syntax keyword axiomDomain  IAN InnerAlgebraicNumber
syntax keyword axiomDomain  AN AlgebraicNumber
syntax keyword axiomDomain  CONTFRAC ContinuedFraction
syntax keyword axiomDomain  RADFF RadicalFunctionField
syntax keyword axiomDomain  ALGFF AlgebraicFunctionField
syntax keyword axiomDomain  INTFTBL IntegrationFunctionsTable
syntax keyword axiomDomain  D01AJFA d01ajfAnnaType
syntax keyword axiomDomain  D01AKFA d01akfAnnaType
syntax keyword axiomDomain  D01AMFA d01amfAnnaType
syntax keyword axiomDomain  D01AQFA d01aqfAnnaType
syntax keyword axiomDomain  D01APFA d01apfAnnaType
syntax keyword axiomDomain  D01ALFA d01alfAnnaType
syntax keyword axiomDomain  D01ANFA d01anfAnnaType
syntax keyword axiomDomain  D01ASFA d01asfAnnaType
syntax keyword axiomDomain  D01GBFA d01gbfAnnaType
syntax keyword axiomDomain  D01FCFA d01fcfAnnaType
syntax keyword axiomDomain  D01TRNS d01TransformFunctionType
syntax keyword axiomDomain  ODEIFTBL ODEIntensityFunctionsTable
syntax keyword axiomDomain  D02BBFA d02bbfAnnaType
syntax keyword axiomDomain  D02BHFA d02bhfAnnaType
syntax keyword axiomDomain  D02CJFA d02cjfAnnaType
syntax keyword axiomDomain  D02EJFA d02ejfAnnaType
syntax keyword axiomDomain  D03EEFA d03eefAnnaType
syntax keyword axiomDomain  D03FAFA d03fafAnnaType
syntax keyword axiomDomain  EAB ExtAlgBasis
syntax keyword axiomDomain  ANTISYM AntiSymm
syntax keyword axiomDomain  DERHAM DeRhamComplex
syntax keyword axiomDomain  DHMATRIX DenavitHartenbergMatrix
syntax keyword axiomDomain  FRIDEAL FractionalIdeal
syntax keyword axiomDomain  FRMOD FramedModule
syntax keyword axiomDomain  HELLFDIV HyperellipticFiniteDivisor
syntax keyword axiomDomain  FDIV FiniteDivisor
syntax keyword axiomDomain  ODVAR OrderlyDifferentialVariable
syntax keyword axiomDomain  SDVAR SequentialDifferentialVariable
syntax keyword axiomDomain  DSMP DifferentialSparseMultivariatePolynomial
syntax keyword axiomDomain  ODPOL OrderlyDifferentialPolynomial
syntax keyword axiomDomain  SDPOL SequentialDifferentialPolynomial
syntax keyword axiomDomain  DROPT DrawOption
syntax keyword axiomDomain  E04DGFA e04dgfAnnaType
syntax keyword axiomDomain  E04FDFA e04fdfAnnaType
syntax keyword axiomDomain  E04GCFA e04gcfAnnaType
syntax keyword axiomDomain  E04JAFA e04jafAnnaType
syntax keyword axiomDomain  E04MBFA e04mbfAnnaType
syntax keyword axiomDomain  E04NAFA e04nafAnnaType
syntax keyword axiomDomain  E04UCFA e04ucfAnnaType
syntax keyword axiomDomain  EQ Equation
syntax keyword axiomDomain  EXPUPXS ExponentialOfUnivariatePuiseuxSeries
syntax keyword axiomDomain  UPXSSING UnivariatePuiseuxSeriesWithExponentialSingularity
syntax keyword axiomDomain  EXPEXPAN ExponentialExpansion
syntax keyword axiomDomain  EXPR Expression
syntax keyword axiomDomain  HACKPI Pi
syntax keyword axiomDomain  FFCGP FiniteFieldCyclicGroupExtensionByPolynomial
syntax keyword axiomDomain  FFCGX FiniteFieldCyclicGroupExtension
syntax keyword axiomDomain  FFCG FiniteFieldCyclicGroup
syntax keyword axiomDomain  FFNBP FiniteFieldNormalBasisExtensionByPolynomial
syntax keyword axiomDomain  FFNBX FiniteFieldNormalBasisExtension
syntax keyword axiomDomain  FFNB FiniteFieldNormalBasis
syntax keyword axiomDomain  FFP FiniteFieldExtensionByPolynomial
syntax keyword axiomDomain  FFX FiniteFieldExtension
syntax keyword axiomDomain  IFF InnerFiniteField
syntax keyword axiomDomain  FF FiniteField
syntax keyword axiomDomain  FILE File
syntax keyword axiomDomain  TEXTFILE TextFile
syntax keyword axiomDomain  BINFILE BinaryFile
syntax keyword axiomDomain  KAFILE KeyedAccessFile
syntax keyword axiomDomain  LIB Library
syntax keyword axiomDomain  FLOAT Float
syntax keyword axiomDomain  ZMOD IntegerMod
syntax keyword axiomDomain  FNAME FileName
syntax keyword axiomDomain  OSI OrdSetInts
syntax keyword axiomDomain  COMM Commutator
syntax keyword axiomDomain  FNLA FreeNilpotentLie
syntax keyword axiomDomain  FORMULA ScriptFormulaFormat
syntax keyword axiomDomain  MINT MachineInteger
syntax keyword axiomDomain  MFLOAT MachineFloat
syntax keyword axiomDomain  MCMPLX MachineComplex
syntax keyword axiomDomain  RESULT Result
syntax keyword axiomDomain  FC FortranCode 
syntax keyword axiomDomain  FORTRAN FortranProgram
syntax keyword axiomDomain  M3D ThreeDimensionalMatrix
syntax keyword axiomDomain  SFORT SimpleFortranProgram
syntax keyword axiomDomain  SWITCH Switch
syntax keyword axiomDomain  FTEM FortranTemplate
syntax keyword axiomDomain  FEXPR FortranExpression
syntax keyword axiomDomain  FST FortranScalarType
syntax keyword axiomDomain  FT FortranType 
syntax keyword axiomDomain  SYMTAB SymbolTable
syntax keyword axiomDomain  SYMS TheSymbolTable
syntax keyword axiomDomain  FCOMP FourierComponent
syntax keyword axiomDomain  FSERIES FourierSeries
syntax keyword axiomDomain  FPARFRAC FullPartialFractionExpansion
syntax keyword axiomDomain  LO Localize
syntax keyword axiomDomain  LA LocalAlgebra
syntax keyword axiomDomain  FRAC Fraction
syntax keyword axiomDomain  LMOPS ListMonoidOps
syntax keyword axiomDomain  FMONOID FreeMonoid
syntax keyword axiomDomain  FGROUP FreeGroup
syntax keyword axiomDomain  IFAMON InnerFreeAbelianMonoid
syntax keyword axiomDomain  FAMONOID FreeAbelianMonoid
syntax keyword axiomDomain  FAGROUP FreeAbelianGroup
syntax keyword axiomDomain  FR Factored
syntax keyword axiomDomain  BFUNCT BasicFunctions
syntax keyword axiomDomain  COMPLEX Complex
syntax keyword axiomDomain  ODP OrderedDirectProduct
syntax keyword axiomDomain  HDP HomogeneousDirectProduct
syntax keyword axiomDomain  SHDP SplitHomogeneousDirectProduct
syntax keyword axiomDomain  GDMP GeneralDistributedMultivariatePolynomial
syntax keyword axiomDomain  DMP DistributedMultivariatePolynomial
syntax keyword axiomDomain  HDMP HomogeneousDistributedMultivariatePolynomial
syntax keyword axiomDomain  GCNAALG GenericNonAssociativeAlgebra
syntax keyword axiomDomain  LAUPOL LaurentPolynomial
syntax keyword axiomDomain  GSERIES GeneralUnivariatePowerSeries
syntax keyword axiomDomain  IDEAL PolynomialIdeals
syntax keyword axiomDomain  IDPO IndexedDirectProductObject
syntax keyword axiomDomain  IDPAM IndexedDirectProductAbelianMonoid
syntax keyword axiomDomain  IDPOAM IndexedDirectProductOrderedAbelianMonoid
syntax keyword axiomDomain  IDPOAMS IndexedDirectProductOrderedAbelianMonoidSup
syntax keyword axiomDomain  IDPAG IndexedDirectProductAbelianGroup
syntax keyword axiomDomain  IR IntegrationResult
syntax keyword axiomDomain  INT Integer
syntax keyword axiomDomain  NNI NonNegativeInteger
syntax keyword axiomDomain  PI PositiveInteger
syntax keyword axiomDomain  ROMAN RomanNumeral
syntax keyword axiomDomain  INTRVL Interval
syntax keyword axiomDomain  ITUPLE InfiniteTuple
syntax keyword axiomDomain  MKCHSET MakeCachableSet
syntax keyword axiomDomain  KERNEL Kernel
syntax keyword axiomDomain  ULSCONS UnivariateLaurentSeriesConstructor
syntax keyword axiomDomain  ULS UnivariateLaurentSeries
syntax keyword axiomDomain  LIE AssociatedLieAlgebra
syntax keyword axiomDomain  JORDAN AssociatedJordanAlgebra
syntax keyword axiomDomain  LSQM LieSquareMatrix
syntax keyword axiomDomain  ILIST IndexedList
syntax keyword axiomDomain  LIST List
syntax keyword axiomDomain  ALIST AssociationList
syntax keyword axiomDomain  LMDICT ListMultiDictionary
syntax keyword axiomDomain  SETMN SetOfMIntegersInOneToN
syntax keyword axiomDomain  OMLO OppositeMonogenicLinearOperator
syntax keyword axiomDomain  ODR OrdinaryDifferentialRing
syntax keyword axiomDomain  DPMO DirectProductModule
syntax keyword axiomDomain  DPMM DirectProductMatrixModule
syntax keyword axiomDomain  LODO LinearOrdinaryDifferentialOperator
syntax keyword axiomDomain  LODO1 LinearOrdinaryDifferentialOperator1
syntax keyword axiomDomain  LODO2 LinearOrdinaryDifferentialOperator2
syntax keyword axiomDomain  IMATRIX IndexedMatrix
syntax keyword axiomDomain  MATRIX Matrix
syntax keyword axiomDomain  RMATRIX RectangularMatrix
syntax keyword axiomDomain  SQMATRIX SquareMatrix
syntax keyword axiomDomain  SAOS SingletonAsOrderedSet
syntax keyword axiomDomain  INFORM InputForm
syntax keyword axiomDomain  MODMONOM ModuleMonomial
syntax keyword axiomDomain  GMODPOL GeneralModulePolynomial
syntax keyword axiomDomain  MODMON ModMonic
syntax keyword axiomDomain  MODRING ModularRing
syntax keyword axiomDomain  EMR EuclideanModularRing
syntax keyword axiomDomain  MODFIELD ModularField
syntax keyword axiomDomain  MOEBIUS MoebiusTransform
syntax keyword axiomDomain  MRING MonoidRing
syntax keyword axiomDomain  MSET Multiset
syntax keyword axiomDomain  SMTS SparseMultivariateTaylorSeries
syntax keyword axiomDomain  TS TaylorSeries
syntax keyword axiomDomain  INDE IndexedExponents
syntax keyword axiomDomain  SMP SparseMultivariatePolynomial
syntax keyword axiomDomain  POLY Polynomial
syntax keyword axiomDomain  MPOLY MultivariatePolynomial
syntax keyword axiomDomain  ALGSC AlgebraGivenByStructuralConstants
syntax keyword axiomDomain  SPLNODE SplittingNode
syntax keyword axiomDomain  SPLTREE SplittingTree
syntax keyword axiomDomain  POINT Point
syntax keyword axiomDomain  COMPPROP SubSpaceComponentProperty
syntax keyword axiomDomain  SUBSPACE SubSpace
syntax keyword axiomDomain  NSUP NewSparseUnivariatePolynomial
syntax keyword axiomDomain  NSMP NewSparseMultivariatePolynomial
syntax keyword axiomDomain  OCT Octonion
syntax keyword axiomDomain  OMENC OpenMathEncoding
syntax keyword axiomDomain  OMDEV OpenMathDevice
syntax keyword axiomDomain  OMCONN OpenMathConnection
syntax keyword axiomDomain  OMERRK OpenMathErrorKind
syntax keyword axiomDomain  OMERR OpenMathError
syntax keyword axiomDomain  MODOP ModuleOperator
syntax keyword axiomDomain  OP Operator
syntax keyword axiomDomain  BOP BasicOperator
syntax keyword axiomDomain  AUTOMOR Automorphism
syntax keyword axiomDomain  ORESUP SparseUnivariateSkewPolynomial
syntax keyword axiomDomain  OREUP UnivariateSkewPolynomial
syntax keyword axiomDomain  OUTFORM OutputForm
syntax keyword axiomDomain  IPADIC InnerPAdicInteger
syntax keyword axiomDomain  PADIC PAdicInteger
syntax keyword axiomDomain  BPADIC BalancedPAdicInteger
syntax keyword axiomDomain  PADICRC PAdicRationalConstructor
syntax keyword axiomDomain  PADICRAT PAdicRational
syntax keyword axiomDomain  BPADICRT BalancedPAdicRational
syntax keyword axiomDomain  PARPCURV ParametricPlaneCurve
syntax keyword axiomDomain  PARSCURV ParametricSpaceCurve
syntax keyword axiomDomain  PARSURF ParametricSurface
syntax keyword axiomDomain  PATRES PatternMatchResult
syntax keyword axiomDomain  PATLRES PatternMatchListResult
syntax keyword axiomDomain  PATTERN Pattern
syntax keyword axiomDomain  PERMGRP PermutationGroup
syntax keyword axiomDomain  PERM Permutation
syntax keyword axiomDomain  PFR PartialFraction
syntax keyword axiomDomain  IPF InnerPrimeField
syntax keyword axiomDomain  PF PrimeField
syntax keyword axiomDomain  PLOT3D Plot3D
syntax keyword axiomDomain  PLOT Plot
syntax keyword axiomDomain  GPOLSET GeneralPolynomialSet
syntax keyword axiomDomain  FM FreeModule
syntax keyword axiomDomain  PR PolynomialRing
syntax keyword axiomDomain  SUP SparseUnivariatePolynomial
syntax keyword axiomDomain  UP UnivariatePolynomial
syntax keyword axiomDomain  PRODUCT Product
syntax keyword axiomDomain  PRTITION Partition
syntax keyword axiomDomain  SYMPOLY SymmetricPolynomial
syntax keyword axiomDomain  UPXSCONS UnivariatePuiseuxSeriesConstructor
syntax keyword axiomDomain  UPXS UnivariatePuiseuxSeries
syntax keyword axiomDomain  QALGSET QuasiAlgebraicSet
syntax keyword axiomDomain  QUAT Quaternion
syntax keyword axiomDomain  RADIX RadixExpansion
syntax keyword axiomDomain  BINARY BinaryExpansion
syntax keyword axiomDomain  DECIMAL DecimalExpansion
syntax keyword axiomDomain  HEXADEC HexadecimalExpansion
syntax keyword axiomDomain  ROIRC RightOpenIntervalRootCharacterization 
syntax keyword axiomDomain  RECLOS RealClosure
syntax keyword axiomDomain  REGSET RegularTriangularSet
syntax keyword axiomDomain  RESRING ResidueRing
syntax keyword axiomDomain  ROUTINE RoutinesTable
syntax keyword axiomDomain  ATTRBUT AttributeButtons
syntax keyword axiomDomain  RULE RewriteRule
syntax keyword axiomDomain  RULESET Ruleset
syntax keyword axiomDomain  SEG Segment
syntax keyword axiomDomain  SEGBIND SegmentBinding
syntax keyword axiomDomain  UNISEG UniversalSegment
syntax keyword axiomDomain  SET Set
syntax keyword axiomDomain  SEXOF SExpressionOf
syntax keyword axiomDomain  SEX SExpression
syntax keyword axiomDomain  DFLOAT DoubleFloat
syntax keyword axiomDomain  SINT SingleInteger
syntax keyword axiomDomain  SPACE3 ThreeSpace
syntax keyword axiomDomain  SREGSET SquareFreeRegularTriangularSet
syntax keyword axiomDomain  STREAM Stream
syntax keyword axiomDomain  CHAR Character
syntax keyword axiomDomain  CCLASS CharacterClass
syntax keyword axiomDomain  ISTRING IndexedString
syntax keyword axiomDomain  STRING String
syntax keyword axiomDomain  SUCH SuchThat
syntax keyword axiomDomain  SULS SparseUnivariateLaurentSeries
syntax keyword axiomDomain  ISUPS InnerSparseUnivariatePowerSeries
syntax keyword axiomDomain  SUPXS SparseUnivariatePuiseuxSeries
syntax keyword axiomDomain  SUTS SparseUnivariateTaylorSeries
syntax keyword axiomDomain  SYMBOL Symbol
syntax keyword axiomDomain  TABLEAU Tableau
syntax keyword axiomDomain  HASHTBL HashTable
syntax keyword axiomDomain  INTABL InnerTable
syntax keyword axiomDomain  TABLE Table
syntax keyword axiomDomain  EQTBL EqTable
syntax keyword axiomDomain  STRTBL StringTable
syntax keyword axiomDomain  GSTBL GeneralSparseTable
syntax keyword axiomDomain  STBL SparseTable
syntax keyword axiomDomain  ITAYLOR InnerTaylorSeries
syntax keyword axiomDomain  UTS UnivariateTaylorSeries
syntax keyword axiomDomain  TEX TexFormat
syntax keyword axiomDomain  TREE Tree
syntax keyword axiomDomain  BTREE BinaryTree
syntax keyword axiomDomain  BBTREE BalancedBinaryTree
syntax keyword axiomDomain  BSTREE BinarySearchTree
syntax keyword axiomDomain  BTOURN BinaryTournament
syntax keyword axiomDomain  PENDTREE PendantTree
syntax keyword axiomDomain  GTSET GeneralTriangularSet
syntax keyword axiomDomain  WUTSET WuWenTsunTriangularSet
syntax keyword axiomDomain  TUBE TubePlot
syntax keyword axiomDomain  VARIABLE Variable
syntax keyword axiomDomain  RULECOLD RuleCalled
syntax keyword axiomDomain  FUNCTION FunctionCalled
syntax keyword axiomDomain  OVAR OrderedVariableList
syntax keyword axiomDomain  ANON AnonymousFunction
syntax keyword axiomDomain  IVECTOR IndexedVector
syntax keyword axiomDomain  VECTOR Vector
syntax keyword axiomDomain  DIRPROD DirectProduct
syntax keyword axiomDomain  GRIMAGE GraphImage
syntax keyword axiomDomain  VIEW2D TwoDimensionalViewport
syntax keyword axiomDomain  VIEW3D ThreeDimensionalViewport
syntax keyword axiomDomain  VOID Void
syntax keyword axiomDomain  EXIT Exit
syntax keyword axiomDomain  WP WeightedPolynomials
syntax keyword axiomDomain  OWP OrdinaryWeightedPolynomials
syntax keyword axiomDomain  MAGMA Magma
syntax keyword axiomDomain  LWORD LyndonWord
syntax keyword axiomDomain  LPOLY LiePolynomial
syntax keyword axiomDomain  PBWLB PoincareBirkhoffWittLyndonBasis
syntax keyword axiomDomain  XPBWPOLY XPBWPolynomial
syntax keyword axiomDomain  LEXP LieExponentials
syntax keyword axiomDomain  OFMONOID OrderedFreeMonoid
syntax keyword axiomDomain  FM1 FreeModule1
syntax keyword axiomDomain  XPR XPolynomialRing
syntax keyword axiomDomain  XDPOLY XDistributedPolynomial
syntax keyword axiomDomain  XRPOLY XRecursivePolynomial
syntax keyword axiomDomain  XPOLY XPolynomial
syntax keyword axiomDomain  RGCHAIN RegularChain
syntax keyword axiomDomain  Record


syntax keyword axiomPackage  REALSOLV RealSolvePackage
syntax keyword axiomPackage  FLAGG2 FiniteLinearAggregateFunctions2
syntax keyword axiomPackage  FSAGG2 FiniteSetAggregateFunctions2
syntax keyword axiomPackage  CPIMA CharacteristicPolynomialInMonogenicalAlgebra
syntax keyword axiomPackage  NORMMA NormInMonogenicAlgebra
syntax keyword axiomPackage  IALGFACT InnerAlgFactor
syntax keyword axiomPackage  SAEFACT SimpleAlgebraicExtensionAlgFactor
syntax keyword axiomPackage  RFFACT RationalFunctionFactor
syntax keyword axiomPackage  SAERFFC SAERationalFunctionAlgFactor
syntax keyword axiomPackage  ALGFACT AlgFactor
syntax keyword axiomPackage  AF AlgebraicFunction
syntax keyword axiomPackage  MRATFAC MRationalFactorize
syntax keyword axiomPackage  MPRFF MPolyCatRationalFunctionFactorizer
syntax keyword axiomPackage  MPCPF MPolyCatPolyFactorizer
syntax keyword axiomPackage  GENMFACT GeneralizedMultivariateFactorize
syntax keyword axiomPackage  RFFACTOR RationalFunctionFactorizer
syntax keyword axiomPackage  SUPFRACF SupFractionFactorizer
syntax keyword axiomPackage  MTHING MergeThing
syntax keyword axiomPackage  OPQUERY OperationsQuery
syntax keyword axiomPackage  NONE1 NoneFunctions1
syntax keyword axiomPackage  ANY1 AnyFunctions1
syntax keyword axiomPackage  PRIMARR2 PrimitiveArrayFunctions2
syntax keyword axiomPackage  ARRAY12 OneDimensionalArrayFunctions2
syntax keyword axiomPackage  BEZOUT BezoutMatrix
syntax keyword axiomPackage  BRILL BrillhartTests
syntax keyword axiomPackage  NAGC02 NagPolynomialRootsPackage
syntax keyword axiomPackage  NAGC05 NagRootFindingPackage
syntax keyword axiomPackage  NAGC06 NagSeriesSummationPackage
syntax keyword axiomPackage  CARTEN2 CartesianTensorFunctions2
syntax keyword axiomPackage  ICDEN InnerCommonDenominator
syntax keyword axiomPackage  CDEN CommonDenominator
syntax keyword axiomPackage  UPCDEN UnivariatePolynomialCommonDenominator
syntax keyword axiomPackage  MCDEN MatrixCommonDenominator
syntax keyword axiomPackage  CLIP TwoDimensionalPlotClipping
syntax keyword axiomPackage  CMPLXRT ComplexRootPackage
syntax keyword axiomPackage  COMBF CombinatorialFunction
syntax keyword axiomPackage  FSPECF FunctionalSpecialFunction
syntax keyword axiomPackage  SUMFS FunctionSpaceSum
syntax keyword axiomPackage  COMBINAT IntegerCombinatoricFunctions
syntax keyword axiomPackage  ORDCOMP2 OrderedCompletionFunctions2
syntax keyword axiomPackage  ONECOMP2 OnePointCompletionFunctions2
syntax keyword axiomPackage  INFINITY Infinity
syntax keyword axiomPackage  NCNTFRAC NumericContinuedFraction
syntax keyword axiomPackage  ESCONT ExpertSystemContinuityPackage
syntax keyword axiomPackage  ESCONT1 ExpertSystemContinuityPackage1
syntax keyword axiomPackage  COORDSYS CoordinateSystems
syntax keyword axiomPackage  CRAPACK CRAsyntax keyword axiomPackage 
syntax keyword axiomPackage  CRFP ComplexRootFindingPackage
syntax keyword axiomPackage  MMAP MultipleMap
syntax keyword axiomPackage  FFCAT2 FunctionFieldCategoryFunctions2
syntax keyword axiomPackage  CHVAR ChangeOfVariable
syntax keyword axiomPackage  CYCLES CycleIndicators
syntax keyword axiomPackage  EVALCYC EvaluateCycleIndicators
syntax keyword axiomPackage  CYCLOTOM CyclotomicPolynomialPackage
syntax keyword axiomPackage  D01AGNT d01AgentsPackage
syntax keyword axiomPackage  INTPACK AnnaNumericalIntegrationPackage
syntax keyword axiomPackage  NAGD01 NagIntegrationPackage
syntax keyword axiomPackage  D01WGTS d01WeightsPackage
syntax keyword axiomPackage  D02AGNT d02AgentsPackage
syntax keyword axiomPackage  ODEPACK AnnaOrdinaryDifferentialEquationPackage
syntax keyword axiomPackage  NAGD02 NagOrdinaryDifferentialEquationsPackage
syntax keyword axiomPackage  D03AGNT d03AgentsPackage
syntax keyword axiomPackage  PDEPACK AnnaPartialDifferentialEquationPackage
syntax keyword axiomPackage  NAGD03 NagPartialDifferentialEquationsPackage
syntax keyword axiomPackage  DDFACT DistinctDegreeFactorize
syntax keyword axiomPackage  REPSQ RepeatedSquaring
syntax keyword axiomPackage  REPDB RepeatedDoubling
syntax keyword axiomPackage  FLASORT FiniteLinearAggregateSort
syntax keyword axiomPackage  DEFINTEF ElementaryFunctionDefiniteIntegration
syntax keyword axiomPackage  DFINTTLS DefiniteIntegrationTools
syntax keyword axiomPackage  DEFINTRF RationalFunctionDefiniteIntegration
syntax keyword axiomPackage  DEGRED DegreeReductionPackage
syntax keyword axiomPackage  FRIDEAL2 FractionalIdealFunctions2
syntax keyword axiomPackage  MHROWRED ModularHermitianRowReduction
syntax keyword axiomPackage  FDIV2 FiniteDivisorFunctions2
syntax keyword axiomPackage  DROPT1 DrawOptionFunctions1
syntax keyword axiomPackage  DROPT0 DrawOptionFunctions0
syntax keyword axiomPackage  DRAWCX DrawComplex
syntax keyword axiomPackage  DRAWCFUN TopLevelDrawFunctionsForCompiledFunctions
syntax keyword axiomPackage  DRAW TopLevelDrawFunctions
syntax keyword axiomPackage  DRAWCURV TopLevelDrawFunctionsForAlgebraicCurves
syntax keyword axiomPackage  DRAWPT TopLevelDrawFunctionsForPoints
syntax keyword axiomPackage  NAGE01 NagInterpolationPackage
syntax keyword axiomPackage  NAGE02 NagFittingPackage
syntax keyword axiomPackage  E04AGNT e04AgentsPackage
syntax keyword axiomPackage  OPTPACK AnnaNumericalOptimizationPackage
syntax keyword axiomPackage  NAGE04 NagOptimisationPackage
syntax keyword axiomPackage  SYMFUNC SymmetricFunctions
syntax keyword axiomPackage  TANEXP TangentExpansions
syntax keyword axiomPackage  EFSTRUC ElementaryFunctionStructurePackage
syntax keyword axiomPackage  ITRIGMNP InnerTrigonometricManipulations
syntax keyword axiomPackage  TRIGMNIP TrigonometricManipulations
syntax keyword axiomPackage  CTRIGMNP ComplexTrigonometricManipulations
syntax keyword axiomPackage  EFULS ElementaryFunctionsUnivariateLaurentSeries
syntax keyword axiomPackage  EFUPXS ElementaryFunctionsUnivariatePuiseuxSeries
syntax keyword axiomPackage  EP EigenPackage
syntax keyword axiomPackage  CHARPOL CharacteristicPolynomialPackage
syntax keyword axiomPackage  EF ElementaryFunction
syntax keyword axiomPackage  ELFUTS EllipticFunctionsUnivariateTaylorSeries
syntax keyword axiomPackage  EQ2 EquationFunctions2
syntax keyword axiomPackage  ERROR ErrorFunctions
syntax keyword axiomPackage  EXPR2UPS ExpressionToUnivariatePowerSeries
syntax keyword axiomPackage  EXPRODE ExpressionSpaceODESolver
syntax keyword axiomPackage  PAN2EXPR PolynomialAN2Expression
syntax keyword axiomPackage  EXPR2 ExpressionFunctions2
syntax keyword axiomPackage  PMPREDFS FunctionSpaceAttachPredicates
syntax keyword axiomPackage  PMASSFS FunctionSpaceAssertions
syntax keyword axiomPackage  PMPRED AttachPredicates
syntax keyword axiomPackage  PMASS PatternMatchAssertions
syntax keyword axiomPackage  PICOERCE PiCoercions
syntax keyword axiomPackage  NAGF01 NagMatrixOperationsPackage
syntax keyword axiomPackage  NAGF02 NagEigenPackage
syntax keyword axiomPackage  NAGF04 NagLinearEquationSolvingPackage
syntax keyword axiomPackage  NAGF07 NagLapack
syntax keyword axiomPackage  FACUTIL FactoringUtilities
syntax keyword axiomPackage  PUSHVAR PushVariables
syntax keyword axiomPackage  DLP DiscreteLogarithmPackage
syntax keyword axiomPackage  FFSLPE FiniteFieldSolveLinearPolynomialEquation
syntax keyword axiomPackage  FFF FiniteFieldFunctions
syntax keyword axiomPackage  FFHOM FiniteFieldHomomorphisms
syntax keyword axiomPackage  INBFF InnerNormalBasisFieldFunctions
syntax keyword axiomPackage  FFPOLY2 FiniteFieldPolynomialPackage2
syntax keyword axiomPackage  FFPOLY FiniteFieldPolynomialPackage
syntax keyword axiomPackage  IRREDFFX IrredPolyOverFiniteField
syntax keyword axiomPackage  HB HallBasis
syntax keyword axiomPackage  FORMULA1 ScriptFormulaFormat1
syntax keyword axiomPackage  FCPAK1 FortranCodePackage1
syntax keyword axiomPackage  NAGSP NAGLinkSupportPackage
syntax keyword axiomPackage  FORT FortranPackage
syntax keyword axiomPackage  FOP FortranOutputStackPackage
syntax keyword axiomPackage  TEMUTL TemplateUtilities
syntax keyword axiomPackage  MCALCFN MultiVariableCalculusFunctions
syntax keyword axiomPackage  QFCAT2 QuotientFieldCategoryFunctions2
syntax keyword axiomPackage  LPEFRAC LinearPolynomialEquationByFractions
syntax keyword axiomPackage  FRAC2 FractionFunctions2
syntax keyword axiomPackage  FRUTIL FactoredFunctionUtilities
syntax keyword axiomPackage  FR2 FactoredFunctions2
syntax keyword axiomPackage  FS2EXPXP FunctionSpaceToExponentialExpansion
syntax keyword axiomPackage  FS2UPS FunctionSpaceToUnivariatePowerSeries
syntax keyword axiomPackage  ES1 ExpressionSpaceFunctions1
syntax keyword axiomPackage  ES2 ExpressionSpaceFunctions2
syntax keyword axiomPackage  FS2 FunctionSpaceFunctions2
syntax keyword axiomPackage  FSUPFACT FunctionSpaceUnivariatePolynomialFactor
syntax keyword axiomPackage  GALFACT GaloisGroupFactorizer
syntax keyword axiomPackage  GALFACTU GaloisGroupFactorizationUtilities
syntax keyword axiomPackage  GALPOLYU GaloisGroupPolynomialUtilities
syntax keyword axiomPackage  GALUTIL GaloisGroupUtilities
syntax keyword axiomPackage  GAUSSFAC GaussianFactorizationPackage
syntax keyword axiomPackage  COMPLPAT ComplexPattern
syntax keyword axiomPackage  CPMATCH ComplexPatternMatch
syntax keyword axiomPackage  COMPLEX2 ComplexFunctions2
syntax keyword axiomPackage  COMPFACT ComplexFactorization
syntax keyword axiomPackage  CINTSLPE ComplexIntegerSolveLinearPolynomialEquation
syntax keyword axiomPackage  GBEUCLID EuclideanGroebnerBasisPackage
syntax keyword axiomPackage  GBINTERN GroebnerInternalPackage
syntax keyword axiomPackage  GB GroebnerPackage
syntax keyword axiomPackage  ORDFUNS OrderingFunctions
syntax keyword axiomPackage  GENEEZ GenExEuclid
syntax keyword axiomPackage  CVMP CoerceVectorMatrixPackage
syntax keyword axiomPackage  GENUFACT GenUFactorize
syntax keyword axiomPackage  GENUPS GenerateUnivariatePowerSeries
syntax keyword axiomPackage  GHENSEL GeneralHenselPackage
syntax keyword axiomPackage  GENPGCD GeneralPolynomialGcdPackage
syntax keyword axiomPackage  GRDEF GraphicsDefaults
syntax keyword axiomPackage  GBF GroebnerFactorizationPackage
syntax keyword axiomPackage  GROEBSOL GroebnerSolve
syntax keyword axiomPackage  IDECOMP IdealDecompositionPackage
syntax keyword axiomPackage  STINPROD StreamInfiniteProduct
syntax keyword axiomPackage  INFPROD0 InfiniteProductCharacteristicZero
syntax keyword axiomPackage  INPRODPF InfiniteProductPrimeField
syntax keyword axiomPackage  INPRODFF InfiniteProductFiniteField
syntax keyword axiomPackage  INTG0 GenusZeroIntegration
syntax keyword axiomPackage  INTPAF PureAlgebraicIntegration
syntax keyword axiomPackage  INTAF AlgebraicIntegration
syntax keyword axiomPackage  DBLRESP DoubleResultantPackage
syntax keyword axiomPackage  INTHERAL AlgebraicHermiteIntegration
syntax keyword axiomPackage  INTALG AlgebraicIntegrate
syntax keyword axiomPackage  IR2 IntegrationResultFunctions2
syntax keyword axiomPackage  TRIMAT TriangularMatrixOperations
syntax keyword axiomPackage  IBATOOL IntegralBasisTools
syntax keyword axiomPackage  FFINTBAS FunctionFieldIntegralBasis
syntax keyword axiomPackage  WFFINTBS WildFunctionFieldIntegralBasis
syntax keyword axiomPackage  NFINTBAS NumberFieldIntegralBasis
syntax keyword axiomPackage  INTEF ElementaryIntegration
syntax keyword axiomPackage  INTSLPE IntegerSolveLinearPolynomialEquation
syntax keyword axiomPackage  FSCINT FunctionSpaceComplexIntegration
syntax keyword axiomPackage  FSINT FunctionSpaceIntegration
syntax keyword axiomPackage  PRIMES IntegerPrimesPackage
syntax keyword axiomPackage  IROOT IntegerRoots
syntax keyword axiomPackage  INTFACT IntegerFactorizationPackage
syntax keyword axiomPackage  INTPM PatternMatchIntegration
syntax keyword axiomPackage  SUBRESP SubResultantPackage
syntax keyword axiomPackage  MONOTOOL MonomialExtensionTools
syntax keyword axiomPackage  INTHERTR TranscendentalHermiteIntegration
syntax keyword axiomPackage  INTTR TranscendentalIntegration
syntax keyword axiomPackage  INTRAT RationalIntegration
syntax keyword axiomPackage  INTRF RationalFunctionIntegration
syntax keyword axiomPackage  IR2F IntegrationResultToFunction
syntax keyword axiomPackage  IRRF2F IntegrationResultRFToFunction
syntax keyword axiomPackage  IRSN IrrRepSymNatPackage
syntax keyword axiomPackage  ITFUN2 InfiniteTupleFunctions2
syntax keyword axiomPackage  ITFUN3 InfiniteTupleFunctions3
syntax keyword axiomPackage  SCACHE SortedCache
syntax keyword axiomPackage  KERNEL2 KernelFunctions2
syntax keyword axiomPackage  KOVACIC Kovacic
syntax keyword axiomPackage  LAPLACE LaplaceTransform
syntax keyword axiomPackage  INVLAPLA InverseLaplaceTransform
syntax keyword axiomPackage  ULS2 UnivariateLaurentSeriesFunctions2
syntax keyword axiomPackage  LEADCDET LeadingCoefDetermination
syntax keyword axiomPackage  LIMITPS PowerSeriesLimitPackage
syntax keyword axiomPackage  SIGNEF ElementaryFunctionSign
syntax keyword axiomPackage  LINDEP LinearDependence
syntax keyword axiomPackage  ZLINDEP IntegerLinearDependence
syntax keyword axiomPackage  LGROBP LinGroebnerPackage
syntax keyword axiomPackage  LF LiouvillianFunction
syntax keyword axiomPackage  HEUGCD HeuGcd
syntax keyword axiomPackage  LIST2 ListFunctions2
syntax keyword axiomPackage  LIST3 ListFunctions3
syntax keyword axiomPackage  LIST2MAP ListToMap
syntax keyword axiomPackage  PREASSOC PrecomputedAssociatedEquations
syntax keyword axiomPackage  ASSOCEQ AssociatedEquations
syntax keyword axiomPackage  LODOF LinearOrdinaryDifferentialOperatorFactorizer
syntax keyword axiomPackage  NCODIV NonCommutativeOperatorDivision
syntax keyword axiomPackage  LODOOPS LinearOrdinaryDifferentialOperatorsOps
syntax keyword axiomPackage  FACTFUNC FactoredFunctions
syntax keyword axiomPackage  POLYROOT PolynomialRoots
syntax keyword axiomPackage  ALGMANIP AlgebraicManipulations
syntax keyword axiomPackage  SIMPAN SimplifyAlgebraicNumberConvertPackage
syntax keyword axiomPackage  TRMANIP TranscendentalManipulations
syntax keyword axiomPackage  MAPHACK1 MappingPackageInternalHacks1
syntax keyword axiomPackage  MAPHACK2 MappingPackageInternalHacks2
syntax keyword axiomPackage  MAPHACK3 MappingPackageInternalHacks3
syntax keyword axiomPackage  MAPPKG1 MappingPackage1
syntax keyword axiomPackage  MAPPKG2 MappingPackage2
syntax keyword axiomPackage  MAPPKG3 MappingPackage3
syntax keyword axiomPackage  IMATLIN InnerMatrixLinearAlgebraFunctions
syntax keyword axiomPackage  MATCAT2 MatrixCategoryFunctions2
syntax keyword axiomPackage  RMCAT2 RectangularMatrixCategoryFunctions2
syntax keyword axiomPackage  IMATQF InnerMatrixQuotientFieldFunctions
syntax keyword axiomPackage  MATLIN MatrixLinearAlgebraFunctions
syntax keyword axiomPackage  MATSTOR StorageEfficientMatrixOperations
syntax keyword axiomPackage  MESH MeshCreationRoutinesForThreeDimensions
syntax keyword axiomPackage  MFINFACT MultFiniteFactorize
syntax keyword axiomPackage  INFORM1 InputFormFunctions1
syntax keyword axiomPackage  MKFUNC MakeFunction
syntax keyword axiomPackage  MKUCFUNC MakeUnaryCompiledFunction
syntax keyword axiomPackage  MKBCFUNC MakeBinaryCompiledFunction
syntax keyword axiomPackage  MKFLCFN MakeFloatCompiledFunction
syntax keyword axiomPackage  MKRECORD MakeRecord
syntax keyword axiomPackage  MLIFT MultivariateLifting
syntax keyword axiomPackage  MDDFACT ModularDistinctDegreeFactorizer
syntax keyword axiomPackage  INMODGCD InnerModularGcd
syntax keyword axiomPackage  MRF2 MonoidRingFunctions2
syntax keyword axiomPackage  INNMFACT InnerMultFact
syntax keyword axiomPackage  MULTFACT MultivariateFactorize
syntax keyword axiomPackage  ALGMFACT AlgebraicMultFact
syntax keyword axiomPackage  POLY2 PolynomialFunctions2
syntax keyword axiomPackage  MULTSQFR MultivariateSquareFree
syntax keyword axiomPackage  ALGPKG AlgebraPackage
syntax keyword axiomPackage  SCPKG StructuralConstantsPackage
syntax keyword axiomPackage  FRNAAF2 FramedNonAssociativeAlgebraFunctions2
syntax keyword axiomPackage  IPRNTPK InternalPrintPackage
syntax keyword axiomPackage  TBCMPPK TabulatedComputationPackage
syntax keyword axiomPackage  PTPACK PointPackage
syntax keyword axiomPackage  PTFUNC2 PointFunctions2
syntax keyword axiomPackage  NSUP2 NewSparseUnivariatePolynomialFunctions2
syntax keyword axiomPackage  RETSOL RetractSolvePackage
syntax keyword axiomPackage  NLINSOL NonLinearSolvePackage
syntax keyword axiomPackage  NODE1 NonLinearFirstOrderODESolver
syntax keyword axiomPackage  NPCOEF NPCoef
syntax keyword axiomPackage  NORMPK NormalizationPackage
syntax keyword axiomPackage  LAZM3PK LazardSetSolvingPackage
syntax keyword axiomPackage  INEP InnerNumericEigenPackage
syntax keyword axiomPackage  NREP NumericRealEigenPackage
syntax keyword axiomPackage  NCEP NumericComplexEigenPackage
syntax keyword axiomPackage  NUMERIC Numeric
syntax keyword axiomPackage  DRAWHACK DrawNumericHack
syntax keyword axiomPackage  NUMODE NumericalOrdinaryDifferentialEquations
syntax keyword axiomPackage  NUMQUAD NumericalQuadrature
syntax keyword axiomPackage  INFSP InnerNumericFloatSolvePackage
syntax keyword axiomPackage  FLOATRP FloatingRealPackage
syntax keyword axiomPackage  FLOATCP FloatingComplexPackage
syntax keyword axiomPackage  INTHEORY IntegerNumberTheoryFunctions
syntax keyword axiomPackage  PNTHEORY PolynomialNumberTheoryFunctions
syntax keyword axiomPackage  OCTCT2 OctonionCategoryFunctions2
syntax keyword axiomPackage  ODESYS SystemODESolver
syntax keyword axiomPackage  ODERED ReduceLODE
syntax keyword axiomPackage  ODEPAL PureAlgebraicLODE
syntax keyword axiomPackage  REDORDER ReductionOfOrder
syntax keyword axiomPackage  LODEEF ElementaryFunctionLODESolver
syntax keyword axiomPackage  ODEEF ElementaryFunctionODESolver
syntax keyword axiomPackage  BALFACT BalancedFactorisation
syntax keyword axiomPackage  BOUNDZRO BoundIntegerRoots
syntax keyword axiomPackage  ODEPRIM PrimitiveRatDE
syntax keyword axiomPackage  UTSODETL UTSodetools
syntax keyword axiomPackage  ODERAT RationalLODE
syntax keyword axiomPackage  ODETOOLS ODETools
syntax keyword axiomPackage  ODEINT ODEIntegration
syntax keyword axiomPackage  ODECONST ConstantLODE
syntax keyword axiomPackage  OMPKG OpenMathPackage
syntax keyword axiomPackage  OMSERVER OpenMathServerPackage
syntax keyword axiomPackage  OMEXPR ExpressionToOpenMath
syntax keyword axiomPackage  BOP1 BasicOperatorFunctions1
syntax keyword axiomPackage  COMMONOP CommonOperators
syntax keyword axiomPackage  APPLYORE ApplyUnivariateSkewPolynomial
syntax keyword axiomPackage  OREPCTO UnivariateSkewPolynomialCategoryOps
syntax keyword axiomPackage  NUMFMT NumberFormats
syntax keyword axiomPackage  OUT OutputPackage
syntax keyword axiomPackage  SPECOUT SpecialOutputPackage
syntax keyword axiomPackage  DISPLAY DisplayPackage
syntax keyword axiomPackage  PADEPAC PadeApproximantPackage
syntax keyword axiomPackage  PADE PadeApproximants
syntax keyword axiomPackage  IBPTOOLS IntegralBasisPolynomialTools
syntax keyword axiomPackage  IBACHIN ChineseRemainderToolsForIntegralBases
syntax keyword axiomPackage  PWFFINTB PAdicWildFunctionFieldIntegralBasis
syntax keyword axiomPackage  PARPC2 ParametricPlaneCurveFunctions2
syntax keyword axiomPackage  PARSC2 ParametricSpaceCurveFunctions2
syntax keyword axiomPackage  PARSU2 ParametricSurfaceFunctions2
syntax keyword axiomPackage  PARTPERM PartitionsAndPermutations
syntax keyword axiomPackage  PATRES2 PatternMatchResultFunctions2
syntax keyword axiomPackage  PMSYM PatternMatchSymbol
syntax keyword axiomPackage  PMKERNEL PatternMatchKernel
syntax keyword axiomPackage  PMDOWN PatternMatchPushDown
syntax keyword axiomPackage  PMTOOLS PatternMatchTools
syntax keyword axiomPackage  PMLSAGG PatternMatchListAggregate
syntax keyword axiomPackage  PMINS PatternMatchIntegerNumberSystem
syntax keyword axiomPackage  PMQFCAT PatternMatchQuotientFieldCategory
syntax keyword axiomPackage  PMPLCAT PatternMatchPolynomialCategory
syntax keyword axiomPackage  PMFS PatternMatchFunctionSpace
syntax keyword axiomPackage  PATMATCH PatternMatch
syntax keyword axiomPackage  PATTERN1 PatternFunctions1
syntax keyword axiomPackage  PATTERN2 PatternFunctions2
syntax keyword axiomPackage  PCOMP PolynomialComposition
syntax keyword axiomPackage  PDECOMP PolynomialDecomposition
syntax keyword axiomPackage  GRAY GrayCode
syntax keyword axiomPackage  PERMAN Permanent
syntax keyword axiomPackage  PGE PermutationGroupExamples
syntax keyword axiomPackage  PFBRU PolynomialFactorizationByRecursionUnivariate
syntax keyword axiomPackage  PFBR PolynomialFactorizationByRecursion
syntax keyword axiomPackage  FORDER FindOrderFinite
syntax keyword axiomPackage  RDIV ReducedDivisor
syntax keyword axiomPackage  PFOTOOLS PointsOfFiniteOrderTools
syntax keyword axiomPackage  PFOQ PointsOfFiniteOrderRational
syntax keyword axiomPackage  FSRED FunctionSpaceReduce
syntax keyword axiomPackage  PFO PointsOfFiniteOrder
syntax keyword axiomPackage  PFRPAC PartialFractionPackage
syntax keyword axiomPackage  PGCD PolynomialGcdPackage
syntax keyword axiomPackage  PGROEB PolyGroebner
syntax keyword axiomPackage  PINTERPA PolynomialInterpolationAlgorithms
syntax keyword axiomPackage  PINTERP PolynomialInterpolation
syntax keyword axiomPackage  PLEQN ParametricLinearEquations
syntax keyword axiomPackage  PLOT1 PlotFunctions1
syntax keyword axiomPackage  PLOTTOOL PlotTools
syntax keyword axiomPackage  MPC2 MPolyCatFunctions2
syntax keyword axiomPackage  MPC3 MPolyCatFunctions3
syntax keyword axiomPackage  POLTOPOL PolToPol
syntax keyword axiomPackage  POLYLIFT PolynomialCategoryLifting
syntax keyword axiomPackage  UPOLYC2 UnivariatePolynomialCategoryFunctions2
syntax keyword axiomPackage  COMMUPC CommuteUnivariatePolynomialCategory
syntax keyword axiomPackage  UPSQFREE UnivariatePolynomialSquareFree
syntax keyword axiomPackage  PSQFR PolynomialSquareFree
syntax keyword axiomPackage  UPMP UnivariatePolynomialMultiplicationPackage
syntax keyword axiomPackage  SUP2 SparseUnivariatePolynomialFunctions2
syntax keyword axiomPackage  UP2 UnivariatePolynomialFunctions2
syntax keyword axiomPackage  POLY2UP PolynomialToUnivariatePolynomial
syntax keyword axiomPackage  PRIMELT PrimitiveElement
syntax keyword axiomPackage  FSPRMELT FunctionSpacePrimitiveElement
syntax keyword axiomPackage  PRINT PrintPackage
syntax keyword axiomPackage  PRS PseudoRemainderSequence
syntax keyword axiomPackage  PSEUDLIN PseudoLinearNormalForm
syntax keyword axiomPackage  UPXS2 UnivariatePuiseuxSeriesFunctions2
syntax keyword axiomPackage  QALGSET2 QuasiAlgebraicSet2
syntax keyword axiomPackage  QUATCT2 QuaternionCategoryFunctions2
syntax keyword axiomPackage  REP RadicalEigenPackage
syntax keyword axiomPackage  RADUTIL RadixUtilities
syntax keyword axiomPackage  RANDSRC RandomNumberSource
syntax keyword axiomPackage  RDIST RandomDistributions
syntax keyword axiomPackage  INTBIT IntegerBits
syntax keyword axiomPackage  RIDIST RandomIntegerDistributions
syntax keyword axiomPackage  RFDIST RandomFloatDistributions
syntax keyword axiomPackage  RATFACT RationalFactorize
syntax keyword axiomPackage  INTTOOLS IntegrationTools
syntax keyword axiomPackage  RDEEF ElementaryRischDE
syntax keyword axiomPackage  RDETR TranscendentalRischDE
syntax keyword axiomPackage  RDETRS TranscendentalRischDESystem
syntax keyword axiomPackage  RDEEFS ElementaryRischDESystem
syntax keyword axiomPackage  REAL0Q RealZeroPackageQ
syntax keyword axiomPackage  REAL0 RealZeroPackage
syntax keyword axiomPackage  POLUTIL RealPolynomialUtilitiesPackage
syntax keyword axiomPackage  QCMPACK QuasiComponentPackage
syntax keyword axiomPackage  RSETGCD RegularTriangularSetGcdPackage
syntax keyword axiomPackage  RSDCMPK RegularSetDecompositionPackage
syntax keyword axiomPackage  REP1 RepresentationPackage1
syntax keyword axiomPackage  REP2 RepresentationPackage2
syntax keyword axiomPackage  INTRET IntegerRetractions
syntax keyword axiomPackage  RATRET RationalRetractions
syntax keyword axiomPackage  POLYCATQ PolynomialCategoryQuotientFunctions
syntax keyword axiomPackage  RF RationalFunction
syntax keyword axiomPackage  ODEPRRIC PrimitiveRatRicDE
syntax keyword axiomPackage  ODERTRIC RationalRicDE
syntax keyword axiomPackage  RINTERP RationalInterpolation
syntax keyword axiomPackage  APPRULE ApplyRules
syntax keyword axiomPackage  SEG2 SegmentFunctions2
syntax keyword axiomPackage  SEGBIND2 SegmentBindingFunctions2
syntax keyword axiomPackage  UNISEG2 UniversalSegmentFunctions2
syntax keyword axiomPackage  INCRMAPS IncrementingMaps
syntax keyword axiomPackage  UDPO UserDefinedPartialOrdering
syntax keyword axiomPackage  UDVO UserDefinedVariableOrdering
syntax keyword axiomPackage  SGCF SymmetricGroupCombinatoricFunctions
syntax keyword axiomPackage  TOOLSIGN ToolsForSign
syntax keyword axiomPackage  INPSIGN InnerPolySign
syntax keyword axiomPackage  SIGNRF RationalFunctionSign
syntax keyword axiomPackage  LIMITRF RationalFunctionLimitPackage
syntax keyword axiomPackage  SMITH SmithNormalForm
syntax keyword axiomPackage  DIOSP DiophantineSolutionPackage
syntax keyword axiomPackage  SOLVEFOR PolynomialSolveByFormulas
syntax keyword axiomPackage  LSMP LinearSystemMatrixPackage
syntax keyword axiomPackage  LSMP1 LinearSystemMatrixPackage1
syntax keyword axiomPackage  LSPP LinearSystemPolynomialPackage
syntax keyword axiomPackage  SOLVERAD RadicalSolvePackage
syntax keyword axiomPackage  SORTPAK SortPackage
syntax keyword axiomPackage  TOPSP TopLevelThreeSpace
syntax keyword axiomPackage  DFSFUN DoubleFloatSpecialFunctions
syntax keyword axiomPackage  ORTHPOL OrthogonalPolynomialFunctions
syntax keyword axiomPackage  NTPOLFN NumberTheoreticPolynomialFunctions
syntax keyword axiomPackage  SFQCMPK SquareFreeQuasiComponentPackage
syntax keyword axiomPackage  SFRGCD SquareFreeRegularTriangularSetGcdPackage
syntax keyword axiomPackage  SRDCMPK SquareFreeRegularSetDecompositionPackage
syntax keyword axiomPackage  NAGS NagSpecialFunctionsPackage
syntax keyword axiomPackage  CSTTOOLS CyclicStreamTools
syntax keyword axiomPackage  STREAM1 StreamFunctions1
syntax keyword axiomPackage  STREAM2 StreamFunctions2
syntax keyword axiomPackage  STREAM3 StreamFunctions3
syntax keyword axiomPackage  STTAYLOR StreamTaylorSeriesOperations
syntax keyword axiomPackage  STTF StreamTranscendentalFunctions
syntax keyword axiomPackage  STTFNC StreamTranscendentalFunctionsNonCommutative
syntax keyword axiomPackage  SHP SturmHabichtPackage
syntax keyword axiomPackage  ISUMP InnerPolySum
syntax keyword axiomPackage  GOSPER GosperSummationMethod
syntax keyword axiomPackage  SUMRF RationalFunctionSum
syntax keyword axiomPackage  SYSSOLP SystemSolvePackage
syntax keyword axiomPackage  MSYSCMD MoreSystemCommands
syntax keyword axiomPackage  TABLBUMP TableauxBumpers
syntax keyword axiomPackage  UTS2 UnivariateTaylorSeriesFunctions2
syntax keyword axiomPackage  TEX1 TexFormat1
syntax keyword axiomPackage  ESTOOLS ExpertSystemToolsPackage
syntax keyword axiomPackage  ESTOOLS1 ExpertSystemToolsPackage1
syntax keyword axiomPackage  ESTOOLS2 ExpertSystemToolsPackage2
syntax keyword axiomPackage  SOLVETRA TransSolvePackage
syntax keyword axiomPackage  SOLVESER TransSolvePackageService
syntax keyword axiomPackage  PSETPK PolynomialSetUtilitiesPackage
syntax keyword axiomPackage  TUBETOOL TubePlotTools
syntax keyword axiomPackage  EXPRTUBE ExpressionTubePlot
syntax keyword axiomPackage  NUMTUBE NumericTubePlot
syntax keyword axiomPackage  NORMRETR NormRetractPackage
syntax keyword axiomPackage  TWOFACT TwoFactorize
syntax keyword axiomPackage  UNIFACT UnivariateFactorize
syntax keyword axiomPackage  UPDECOMP UnivariatePolynomialDecompositionPackage
syntax keyword axiomPackage  UPDIVP UnivariatePolynomialDivisionPackage
syntax keyword axiomPackage  UTSODE UnivariateTaylorSeriesODESolver
syntax keyword axiomPackage  VECTOR2 VectorFunctions2
syntax keyword axiomPackage  DIRPROD2 DirectProductFunctions2
syntax keyword axiomPackage  VIEWDEF ViewDefaultsPackage
syntax keyword axiomPackage  VIEW ViewportPackage
syntax keyword axiomPackage  RESLATC ResolveLatticeCompletion
syntax keyword axiomPackage  WEIER WeierstrassPreparation
syntax keyword axiomPackage  XEXPPKG XExponentialPackage
syntax keyword axiomPackage  YSTREAM ParadoxicalCombinatorsForStreams
syntax keyword axiomPackage  FGLMICPK FGLMIfCanPackage
syntax keyword axiomPackage  LEXTRIPK LexTriangularPackage
syntax keyword axiomPackage  IRURPK InternalRationalUnivariateRepresentationPackage
syntax keyword axiomPackage  RURPK RationalUnivariateRepresentationPackage
syntax keyword axiomPackage  ZDSOLVE ZeroDimensionalSolvePackage
"syntax cluster axiomType contains=axiomDomain,axiomCategory,axiomPackage

syntax keyword	axiomConditional	if else then => \|
syntax keyword	axiomTypeDefinition	where with add \$ \@
syntax keyword	axiomDefinition		: :: := == ==> -> +-> 

  HiLink cFormat		cSpecial
  HiLink cCppString		cString
  HiLink cCommentL		cComment
  HiLink cCommentStart		cComment
  HiLink cLabel			Label
  HiLink cUserLabel		Label
  HiLink cConditional		Conditional
  HiLink cRepeat		Repeat
  HiLink cCharacter		Character
  HiLink cSpecialCharacter	cSpecial
  HiLink cNumber		Number
  HiLink cOctal			Number
  HiLink cOctalZero		PreProc	 " link this to Error if you want
  HiLink cFloat			Float
  HiLink cOctalError		cError
  HiLink cParenError		cError
  HiLink cErrInParen		cError
  HiLink cErrInBracket		cError
  HiLink cCommentError		cError
  HiLink cCommentStartError	cError
  HiLink cSpaceError		cError
  HiLink cSpecialError		cError
  HiLink cOperator		Operator
  HiLink cStructure		Structure
  HiLink cStorageClass		StorageClass
  HiLink cInclude		Include
  HiLink cPreProc		PreProc
  HiLink cDefine		Macro
  HiLink cIncluded		cString
  HiLink cError			Error
  HiLink cStatement		Statement
  HiLink cPreCondit		PreCondit
  HiLink cType			Type
  HiLink cConstant		Constant
  HiLink cCommentString		cString
  HiLink cComment2String	cString
  HiLink cCommentSkip		cComment
  HiLink cString		String
  HiLink cComment		Comment
  HiLink cSpecial		SpecialChar
  HiLink cCppSkip		cCppOut
  HiLink cCppOut2		cCppOut
  HiLink cCppOut		Comment

  HiLink axiomCommentPP		axiomComment
  HiLink axiomCommentMM		axiomComment
  HiLink axiomComment		Comment

  HiLink axiomRepeat		Repeat
  HiLink axiomConditional	Conditional
  HiLink axiomDefinition	Conditional
  HiLink axiomSystemCommand	PreProc
  HiLink axiomStatement		Statement
  HiLink axiomTypeDefinition	PreProc

  HiLink axiomTodo		Todo
  HiLink axiomCategory		Type
  HiLink axiomDomain		Type
  HiLink axiomPackage		Type

  delcommand HiLink
endif

let b:current_syntax = "c"

" vim: ts=8
