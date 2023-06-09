/*
 * PEGTL grammar for C w/ Algebraic Types
 */

#pragma once

#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/utf16.hpp>
#include <tao/pegtl/contrib/utf32.hpp>
#include <tao/pegtl/rules.hpp>

namespace cat
{
   namespace pegtl = tao::pegtl;
   // clang-format off

	//+--------------
	// spaces
	//+--------------

	// WhiteSpace  <- [ \n\r\t]
	struct whitespace : pegtl::space{};

	// LineComment <- '//' (!'\n' .)
	struct line_comment : pegtl::seq<pegtl::two<'/'>, pegtl::until< pegtl::eolf >>{};

	// LongComment <- '/*' (!'*/'.)* '*/'
	struct multi_line_comment_start : pegtl::if_must<pegtl::one<'/'>, pegtl::one<'*'>>{};
	struct multi_line_comment_end : pegtl::if_must<pegtl::one<'*'>, pegtl::one<'/'>>{};
	struct multi_line_comment_middle : pegtl::until<multi_line_comment_end, pegtl::any>{};

	struct multi_line_comment : pegtl::if_must<
		multi_line_comment_start ,
		multi_line_comment_middle> {};

	// Pragma      <- '#'  (!'\n' .)*
	struct pragma : pegtl::seq<pegtl::one<'#'>, pegtl::until<pegtl::eolf>>{};

	struct Spacing : pegtl::disable<pegtl::sor<whitespace, line_comment, multi_line_comment, pragma>>{};

	//+--------------
	// Keywords
	//+--------------

	struct AUTO      : TAO_PEGTL_KEYWORD("auto"){};
	struct BREAK     : TAO_PEGTL_KEYWORD("break"){};
	struct CASE      : TAO_PEGTL_KEYWORD("case"){};
	struct CHAR      : TAO_PEGTL_KEYWORD("char"){};
	struct CONST     : TAO_PEGTL_KEYWORD("const"){};
	struct CONTINUE  : TAO_PEGTL_KEYWORD("continue"){};
	struct DEFAULT   : TAO_PEGTL_KEYWORD("default"){};
	struct DOUBLE    : TAO_PEGTL_KEYWORD("double"){};
	struct DO        : TAO_PEGTL_KEYWORD("do"){};
	struct ELSE      : TAO_PEGTL_KEYWORD("else"){};
	struct ENUM      : TAO_PEGTL_KEYWORD("enum"){};
	struct EXTERN    : TAO_PEGTL_KEYWORD("extern"){};
	struct FLOAT     : TAO_PEGTL_KEYWORD("float"){};
	struct FOR       : TAO_PEGTL_KEYWORD("for"){};
	struct GOTO      : TAO_PEGTL_KEYWORD("goto"){};
	struct IF        : TAO_PEGTL_KEYWORD("if"){};
	struct INT       : TAO_PEGTL_KEYWORD("int"){};
	struct INLINE    : TAO_PEGTL_KEYWORD("inline"){};
	struct LONG      : TAO_PEGTL_KEYWORD("long"){};
	struct REGISTER  : TAO_PEGTL_KEYWORD("register"){};
	struct RESTRICT  : TAO_PEGTL_KEYWORD("restrict"){};
	struct RETURN    : TAO_PEGTL_KEYWORD("return"){};
	struct SHORT     : TAO_PEGTL_KEYWORD("short"){};
	struct SIGNED    : TAO_PEGTL_KEYWORD("signed"){};
	struct SIZEOF    : TAO_PEGTL_KEYWORD("sizeof"){};
	struct STATIC    : TAO_PEGTL_KEYWORD("static"){};
	struct STRUCT    : TAO_PEGTL_KEYWORD("struct"){};
	struct SWITCH    : TAO_PEGTL_KEYWORD("switch"){};
	struct TYPEDEF   : TAO_PEGTL_KEYWORD("typedef"){};
	struct UNION     : TAO_PEGTL_KEYWORD("union"){};
	struct UNSIGNED  : TAO_PEGTL_KEYWORD("unsigned"){};
	struct VOID      : TAO_PEGTL_KEYWORD("void"){};
	struct VOLATILE  : TAO_PEGTL_KEYWORD("volatile"){};
	struct WHILE     : TAO_PEGTL_KEYWORD("while"){};
	struct BOOL      : TAO_PEGTL_KEYWORD("bool"){};
	struct COMPLEX   : TAO_PEGTL_KEYWORD("_Complex"){};
	struct STDCALL   : TAO_PEGTL_KEYWORD("_stdcall"){};
	struct DECLSPEC  : TAO_PEGTL_KEYWORD("__declspec"){};
	struct ATTRIBUTE : TAO_PEGTL_KEYWORD("__attribute__"){};

	struct Keyword : pegtl::disable < pegtl::sor<
										  AUTO,
										  BREAK,
										  CASE,
										  CHAR,
										  CONST,
										  CONTINUE,
										  DEFAULT,
										  DOUBLE,
										  DO,
										  ELSE,
										  ENUM,
										  EXTERN,
										  FLOAT,
										  FOR,
										  GOTO,
										  IF,
										  INT,
										  INLINE,
										  LONG,
										  REGISTER,
										  RESTRICT,
										  RETURN,
										  SHORT,
										  SIGNED,
										  SIZEOF,
										  STATIC,
										  STRUCT,
										  SWITCH,
										  TYPEDEF,
										  UNION,
										  UNSIGNED,
										  VOID,
										  VOLATILE,
										  WHILE,
										  BOOL,
										  COMPLEX,
										  STDCALL,
										  DECLSPEC,
										  ATTRIBUTE > >{};

    //+---------------------------------
	// Universal Character names
    //+---------------------------------

	struct HexQuad : pegtl::seq<pegtl::xdigit, pegtl::xdigit, pegtl::xdigit, pegtl::xdigit>{};
	struct UniversalCharacter : pegtl::seq<TAO_PEGTL_ISTRING("\\u"), HexQuad>{};

	//+---------------------------------
	// Escape Sequence
    //+---------------------------------

	struct SimpleEscape : pegtl::seq<pegtl::one<'\\'>, TAO_PEGTL_STRING("\'\"\?\\abfnrtv")>{};
	struct OctalEscape : pegtl::seq<pegtl::one<'\\'>,
									pegtl::odigit,
									pegtl::opt<pegtl::odigit>,
									pegtl::opt<pegtl::odigit>>{};
	struct HexEscape : pegtl::seq<TAO_PEGTL_STRING("\\x"), pegtl::plus<pegtl::xdigit>>{};
	struct Escape : pegtl::sor<SimpleEscape, OctalEscape, HexEscape, UniversalCharacter>{};

	//+---------------------------------
	// Character
    //+---------------------------------

	struct CharAscii : pegtl::sor<Escape, pegtl::alpha>{};
	struct Char16 : pegtl::sor<
		tao::pegtl::utf16::any,
		tao::pegtl::utf8::any,
		CharAscii >{};
	struct Char32 : pegtl::sor<
		tao::pegtl::utf32::any,
		tao::pegtl::utf16::any,
		tao::pegtl::utf8::any,
		CharAscii >{};

	struct NonAsciiChar : pegtl::disable<
		pegtl::if_must<
			pegtl::sor<Char16, Char32>,
			pegtl::not_at<pegtl::blank>>>{};

	struct CharConstant : pegtl::seq<
		pegtl::one<'\''>,
		CharAscii,
		pegtl::one<'\''>>{};

	struct WideCharConstant : pegtl::seq<
		pegtl::one<'L'>,
		pegtl::one<'\''>,
		Char16,
		pegtl::one<'\''>>{};

	struct Char16Constant : pegtl::seq<
		pegtl::one<'u'>,
		pegtl::one<'\''>,
		Char16,
		pegtl::one<'\''>>{};

	struct Char32Constant : pegtl::seq<
		pegtl::one<'U'>,
		pegtl::one<'\''>,
		Char32,
		pegtl::one<'\''>>{};

	struct CharacterConstant : pegtl::disable<
		pegtl::sor<
			CharConstant,
			WideCharConstant,
			Char16Constant,
			Char32Constant>>{};

	//+---------------------------------
	// Identifiers (user defined names)
    //+---------------------------------

	struct Identifier : pegtl::if_must<
		pegtl::seq<pegtl::identifier_first,
				   pegtl::star<NonAsciiChar>>,
		pegtl::not_at<Keyword>>{};

	//+--------------------------------
	// Constants
	//+--------------------------------

	struct HexPrefix : TAO_PEGTL_ISTRING("0x"){};

	struct EnumerationConstant : pegtl::identifier{};

	struct FloatSuffix : pegtl::sor<TAO_PEGTL_ISTRING("fl")>{};

	// https://en.cppreference.com/w/cpp/language/floating_literal
	struct BinaryExponent : pegtl::seq<pegtl::istring<'p'>,
									   pegtl::sor<pegtl::one<'+'>, pegtl::one<'-'>>,
									   pegtl::plus<pegtl::digit>>{};

	struct Exponent : pegtl::seq<pegtl::istring<'e'>,
								 pegtl::sor<pegtl::one<'+'>, pegtl::one<'-'>>,
								 pegtl::plus<pegtl::digit>>{};

	struct HexFraction : pegtl::sor<
		pegtl::seq<pegtl::star<pegtl::xdigit>, pegtl::one<'.'>, pegtl::plus<pegtl::xdigit>>,
		pegtl::seq<pegtl::plus<pegtl::xdigit>, pegtl::one<'.'>>>{};

	struct Fraction : pegtl::sor<
		pegtl::seq<pegtl::star<pegtl::digit>, pegtl::one<'.'>, pegtl::plus<pegtl::digit>>,
		pegtl::seq<pegtl::plus<pegtl::digit>, pegtl::one<'.'>>>{};

	struct HexFloatConstant : pegtl::sor<
		pegtl::seq<HexPrefix, HexFraction, pegtl::opt<BinaryExponent>>,
		pegtl::seq<HexPrefix, pegtl::plus<pegtl::xdigit>, BinaryExponent>>{};

	struct DecimalFloatConstant : pegtl::sor<
		pegtl::seq<Fraction, pegtl::opt<Exponent>>,
		pegtl::seq<pegtl::plus<pegtl::digit>, Exponent>>{};

	struct FloatConstant : pegtl::seq<
		pegtl::sor<DecimalFloatConstant, HexFloatConstant>,
		pegtl::opt<FloatSuffix>>{};

	struct Lsuffix : pegtl::sor<
		TAO_PEGTL_STRING("ll"),
		TAO_PEGTL_STRING("LL"),
		pegtl::one<'l'>,
		pegtl::one<'L'>>{};

	struct IntegerSuffix : pegtl::sor<
		pegtl::seq<pegtl::istring<'u'>, pegtl::opt<Lsuffix>>,
		pegtl::seq<Lsuffix, pegtl::opt<pegtl::istring<'u'>>>>{};

	struct DecimalConstant : pegtl::seq<pegtl::range<'1', '9'>, pegtl::star<pegtl::digit>>{};

	struct OctalConstant : pegtl::seq<pegtl::one<'0'>, pegtl::star<pegtl::odigit>>{};

	struct HexConstant : pegtl::seq<HexPrefix, pegtl::plus<pegtl::xdigit>>{};

	struct IntegerConstant : pegtl::disable<
		pegtl::sor<
			DecimalConstant,
			HexConstant,
			OctalConstant>>{};

	struct Constant : pegtl::disable<
		pegtl::sor<
			FloatConstant,
			IntegerConstant,
			CharacterConstant,
			EnumerationConstant>>{};

	//+--------------------------------
	// String Literals
	//+--------------------------------

	struct StringChar : CharAscii{};
	struct StringLiteral : pegtl::seq<
		pegtl::opt<pegtl::one<'L'>>,
		pegtl::plus<
			pegtl::one<'\"'>,
			pegtl::star<StringChar>,
			pegtl::one<'\"'>,
			pegtl::star<pegtl::space>
			>>{};

	//+--------------------------------
	// Punctuators
	//+--------------------------------

	struct LBRK : pegtl::one<'['>{};
	struct RBRK : pegtl::one<']'>{};
	struct LPAR : pegtl::one<'('>{};
	struct RPAR : pegtl::one<')'>{};
	struct LWING : pegtl::one<'{'>{};
	struct RWING : pegtl::one<'}'>{};
	struct DOT : pegtl::one<'.'>{};
	struct PTR : pegtl::seq<pegtl::one<'-'>, pegtl::one<'>'>>{};
	struct INC : pegtl::two<'+'>{};
	struct DEC : pegtl::two<'-'>{};

	struct AND : pegtl::seq<
		pegtl::one<'&'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'&'>>>>{};

	struct STAR : pegtl::seq<
		pegtl::one<'*'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct PLUS : pegtl::seq<
		pegtl::one<'+'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<
				pegtl::sor<pegtl::one<'+'>>, pegtl::one<'='>>>>{};

	struct MINUS : pegtl::seq<
		pegtl::one<'-'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<
				pegtl::sor<pegtl::one<'-'>>, pegtl::one<'='>, pegtl::one<'>'>>>>{};

	struct TILDA : pegtl::one<'~'>{};
	struct BANG : pegtl::seq<
		pegtl::one<'!'>,
		pegtl::if_must<pegtl::any,
					   pegtl::not_at<pegtl::one<'='>>>>{};

	struct DIV : pegtl::seq<
		pegtl::one<'/'>,
		pegtl::if_must<pegtl::any,
					   pegtl::not_at<pegtl::one<'='>>>>{};

	struct MOD : pegtl::seq<
		pegtl::one<'%'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<
				pegtl::sor<pegtl::one<'+'>>, pegtl::one<'='>>>>{};

	struct LEFT : pegtl::seq<
		pegtl::two<'<'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<
				pegtl::sor<pegtl::one<'='>, pegtl::one<'>'>>>>>{};

	struct RIGHT : pegtl::seq<
		pegtl::two<'<'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct LT : pegtl::seq<
		pegtl::one<'<'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct GT : pegtl::seq<
		pegtl::one<'<'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct LE : pegtl::seq<
		pegtl::one<'<'>,
		pegtl::one<'='>>{};
	struct GE : pegtl::seq<
		pegtl::one<'>'>,
		pegtl::one<'='>>{};
	struct EQUEQU : pegtl::two<'='>{};
	struct BANGEQU : pegtl::seq<
		pegtl::one<'!'>,
		pegtl::one<'='>>{};

	struct HAT : pegtl::seq<
		pegtl::one<'^'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct OR : pegtl::seq<
		pegtl::one<'|'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct ANDAND : pegtl::two<'&'>{};
	struct OROR : pegtl::two<'|'>{};
	struct QUERY : pegtl::one<'?'>{};

	struct COLON : pegtl::seq<
		pegtl::one<':'>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'>'>>>>{};

	struct SEMI : pegtl::one<';'>{};
	struct ELLIPSIS : pegtl::three<'.'>{};

	struct EQU : pegtl::seq<
		pegtl::one<'='>,
		pegtl::if_must<
			pegtl::any,
			pegtl::not_at<pegtl::one<'='>>>>{};

	struct STAREQU : pegtl::seq<
		pegtl::one<'*'>,
		pegtl::one<'='>>{};
	struct DIVEQU : pegtl::seq<
		pegtl::one<'/'>,
		pegtl::one<'='>>{};
	struct MODEQU : pegtl::seq<
		pegtl::one<'%'>,
		pegtl::one<'='>>{};
	struct PLUSEQU : pegtl::seq<
		pegtl::one<'+'>,
		pegtl::one<'='>>{};
	struct MINUSEQU : pegtl::seq<
		pegtl::one<'-'>,
		pegtl::one<'='>>{};
	struct LEFTEQU : pegtl::seq<
		pegtl::two<'<'>,
		pegtl::one<'='>>{};
	struct RIGHTEQU : pegtl::seq<
		pegtl::two<'>'>,
		pegtl::one<'='>>{};
	struct ANDEQU : pegtl::seq<
		pegtl::one<'&'>,
		pegtl::one<'='>>{};
	struct HATEQU : pegtl::seq<
		pegtl::one<'^'>,
		pegtl::one<'='>>{};
	struct OREQU : pegtl::seq<
		pegtl::one<'|'>,
		pegtl::one<'='>>{};
	struct COMMA : pegtl::one<','>{};

	//+--------------------------------
	// Expressions
	//+--------------------------------

	struct TypeName;
	struct InitializerList;

	struct PrimaryExpression;
	struct PostfixExpression;
	struct ArgumentExpressionList;
	struct UnaryExpression;
	struct UnaryOperator;
	struct CastExpression;
	struct MultiplicativeExpression;
	struct AdditiveExpression;
	struct ShiftExpression;
	struct RelationalExpression;
	struct EqualityExpression;
	struct ANDExpression;
	struct ExclusiveORExpression;
	struct InclusiveORExpression;
	struct LogicalANDExpression;
	struct LogicalORExpression;
	struct ConditionalExpression;
	struct AssignmentExpression;
	struct AssignmentOperator;
	struct Expression;
	struct ConstantExpression;

	struct PrimaryExpression : pegtl::sor<
		StringLiteral,
		Constant,
		Identifier,
		pegtl::seq<LPAR, Expression, RPAR>>{};

	struct PostfixExpression :
		pegtl::seq<
		pegtl::sor<
			PrimaryExpression,
			pegtl::seq<
				LPAR,
				TypeName,
				RPAR,
				LWING,
				InitializerList,
				pegtl::opt<COMMA>,
				RWING>>,
		pegtl::star<
			pegtl::sor<
				pegtl::seq<LBRK, Expression, RBRK>,
				pegtl::seq<LPAR, pegtl::opt<ArgumentExpressionList>, RPAR>,
				pegtl::seq<DOT, Identifier>,
				pegtl::seq<PTR, Identifier>,
				INC,
				DEC>>>{};

	struct AssignmentOperator : pegtl::sor<
		EQU, STAREQU, DIVEQU, MODEQU, PLUSEQU, MINUSEQU, LEFTEQU, RIGHTEQU, ANDEQU, HATEQU, OREQU
		>{};

	struct AssignmentExpression : pegtl::sor<
		pegtl::seq<UnaryExpression, AssignmentOperator, AssignmentExpression>,
		ConditionalExpression>{};

	struct ArgumentExpressionList : pegtl::seq<
		AssignmentExpression,
		pegtl::star<COMMA, AssignmentExpression>>{};

	struct UnaryOperator : pegtl::sor<
		AND, STAR, PLUS, MINUS, TILDA, BANG
		>{};

	struct UnaryExpression : pegtl::sor<
		PostfixExpression,
		pegtl::seq<INC, UnaryExpression>,
		pegtl::seq<DEC, UnaryExpression>,
		pegtl::seq<UnaryOperator, CastExpression>,
		pegtl::seq<SIZEOF, pegtl::sor<UnaryExpression, pegtl::seq<LPAR, TypeName, RPAR>>>>{};

	struct CastExpression : pegtl::sor<
		pegtl::seq<LPAR, TypeName, RPAR, CastExpression>,
		UnaryExpression>{};

	struct MultiplicativeExpression : pegtl::seq<
		CastExpression,
		pegtl::star<
			pegtl::sor<STAR, DIV, MOD>,
			CastExpression>>{};

	struct AdditiveExpression : pegtl::seq<
		MultiplicativeExpression,
		pegtl::star<
			pegtl::sor<PLUS, MINUS>,
			MultiplicativeExpression>>{};

	struct ShiftExpression : pegtl::seq<
		AdditiveExpression,
		pegtl::star<
			pegtl::sor<LEFT, RIGHT>,
			AdditiveExpression>>{};

	struct RelationalExpression : pegtl::seq<
		ShiftExpression,
		pegtl::star<
			pegtl::sor<LE, GE, LT, GT>,
			ShiftExpression>>{};

	struct EqualityExpression : pegtl::seq<
		RelationalExpression,
		pegtl::star<
			pegtl::sor<EQUEQU, BANGEQU>,
			RelationalExpression>>{};

	struct ANDExpression : pegtl::seq<
		EqualityExpression,
		pegtl::star<AND, EqualityExpression>>{};

	struct ExclusiveORExpression : pegtl::seq<
		ANDExpression,
		pegtl::star<HAT, ANDExpression>>{};

	struct InclusiveORExpression : pegtl::seq<
		ExclusiveORExpression,
		pegtl::star<OR, ExclusiveORExpression>>{};

	struct LogicalANDExpression : pegtl::seq<
		InclusiveORExpression,
		pegtl::star<ANDAND, InclusiveORExpression>>{};

	struct LogicalORExpression : pegtl::seq<
		LogicalANDExpression,
		pegtl::star<OROR, LogicalANDExpression>>{};

	struct ConditionalExpression : pegtl::seq<
		LogicalORExpression,
		pegtl::star<QUERY, Expression, COLON, LogicalORExpression>>{};

	struct Expression : pegtl::seq<
		AssignmentExpression,
		pegtl::star<COMMA, AssignmentExpression>>{};

	struct ConstantExpression : ConditionalExpression{};

	//+--------------------------------
	// Statements
	//+--------------------------------

	struct Declaration;

	struct Statement;
	struct LabeledStatement;
	struct CompoundStatement;
	struct ExpressionStatement;
	struct SelectionStatement;
	struct IterationStatement;
	struct JumpStatement;

	struct Statement : pegtl::sor<
		LabeledStatement,
		CompoundStatement,
		ExpressionStatement,
		SelectionStatement,
		IterationStatement,
		JumpStatement>{};

	struct LabeledStatement : pegtl::sor<
		pegtl::seq<Identifier, COLON, Statement>,
		pegtl::seq<CASE, ConstantExpression, COLON, Statement>,
		pegtl::seq<DEFAULT, COLON, Statement>>{};

	struct CompoundStatement : pegtl::seq<
		LWING, pegtl::star<pegtl::sor<Declaration, Statement>>, RWING>{};

	struct ExpressionStatement : pegtl::seq<
		pegtl::opt<Expression>, SEMI>{};

	struct SelectionStatement : pegtl::sor<
		pegtl::seq<
			IF,
			LPAR,
			Expression,
			RPAR,
			Statement,
			pegtl::opt<
				pegtl::seq<ELSE, Statement>>>,
		pegtl::seq<
			SWITCH,
			LPAR,
			Expression,
			RPAR,
			Statement>>{};

	struct IterationStatement : pegtl::sor<
		pegtl::seq<WHILE, LPAR, Expression, RPAR, Statement>,
		pegtl::seq<DO, Statement, WHILE, LPAR, Expression, RPAR, SEMI>,
		pegtl::seq<FOR, LPAR,
				   pegtl::opt<Expression>,
				   SEMI,
				   pegtl::opt<Expression>,
				   SEMI,
				   pegtl::opt<Expression>,
				   RPAR, Statement>,
		pegtl::seq<FOR, LPAR, Declaration,
				   pegtl::opt<Expression>,
				   SEMI,
				   pegtl::opt<Expression>,
				   RPAR, Statement>>{};

	struct JumpStatement : pegtl::sor<
		pegtl::seq<GOTO, Identifier, SEMI>,
		pegtl::seq<CONTINUE, SEMI>,
		pegtl::seq<BREAK, SEMI>,
		pegtl::seq<RETURN, pegtl::opt<Expression>, SEMI>>{};

	//+--------------------------------
	// Declarations
	//+--------------------------------

	struct Declaration;
	struct DeclarationSpecifiers;
	struct InitDeclaratorList;
	struct InitDeclarator;
	struct StorageClassSpecifier;
	struct TypeSpecifier;
	struct StructOrUnionSpecifier;
	struct StructOrUnion;
	struct StructDeclaration;
	struct SpecifierQualifierList;
	struct StructDeclaratorList;
	struct StructDeclarator;
	struct EnumSpecifier;
	struct EnumeratorList;
	struct Enumerator;
	struct TypeQualifier;
	struct FunctionSpecifier;
	struct Declarator;
	struct DirectDeclarator;
	struct Pointer;
	struct ParameterTypeList;
	struct ParameterList;
	struct ParameterDeclaration;
	struct IdentifierList;
	struct TypeName;
	struct AbstractDeclarator;
	struct DirectAbstractDeclarator;
	struct TypedefName;
	struct Initializer;
	struct InitializerList;
	struct Designation;
	struct Designator;

	struct Declaration : pegtl::seq<
		DeclarationSpecifiers,
		pegtl::opt<InitDeclaratorList>,
		SEMI>{};

	struct DeclarationSpecifiers : pegtl::sor<
		pegtl::seq<
			pegtl::star<
				pegtl::sor<StorageClassSpecifier, TypeQualifier, FunctionSpecifier>>,
			TypedefName,
			pegtl::star<
				pegtl::sor<StorageClassSpecifier, TypeQualifier, FunctionSpecifier>>>,
		pegtl::plus<
			pegtl::sor<StorageClassSpecifier, TypeSpecifier, TypeQualifier, FunctionSpecifier>>>{};

	struct InitDeclaratorList : pegtl::seq<
		InitDeclarator,
		pegtl::star<COMMA, InitDeclarator>>{};

	//+--------------------------------
	// External definitions
	//+--------------------------------

	struct TranslationUnit;
	struct ExternalDeclaration;
	struct FunctionDefinition;
	struct DeclarationList;

   // clang-format on
}  // namespace cat
