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
		pegtl::sor<Char16,
				   Char32>>{};

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

	struct Identifier : pegtl::if_must<pegtl::not_at<Keyword>,
									   pegtl::sor<pegtl::identifier_first,
												  NonAsciiChar>>{};

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
		pegtl::if_must<pegtl::not_at<pegtl::one<'&'>>, pegtl::any>>{};
	struct STAR : pegtl::seq<
		pegtl::one<'*'>,
		pegtl::if_must<pegtl::not_at<pegtl::one<'='>>, pegtl::any>>{};
	struct PLUS : pegtl::seq<
		pegtl::one<'+'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::sor<pegtl::one<'+'>>, pegtl::one<'='>>,
					   pegtl::any>>{};
	struct MINUS : pegtl::seq<
		pegtl::one<'-'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::sor<pegtl::one<'-'>>, pegtl::one<'='>, pegtl::one<'>'>>,
					   pegtl::any>>{};
	struct TILDA : pegtl::one<'~'>{};
	struct BANG : pegtl::seq<
		pegtl::one<'!'>,
		pegtl::if_must<pegtl::not_at<pegtl::one<'='>>, pegtl::any>>{};
	struct DIV : pegtl::seq<
		pegtl::one<'/'>,
		pegtl::if_must<pegtl::not_at<pegtl::one<'='>>, pegtl::any>>{};
	struct MOD : pegtl::seq<
		pegtl::one<'%'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::sor<pegtl::one<'+'>>, pegtl::one<'='>>,
					   pegtl::any>>{};
	struct LEFT : pegtl::seq<
		pegtl::two<'<'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::sor<pegtl::one<'='>, pegtl::one<'>'>>,
						   pegtl::any>>>{};
	struct RIGHT : pegtl::seq<
		pegtl::two<'<'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'='>,
						   pegtl::any>>>{};
	struct LT : pegtl::seq<
		pegtl::one<'<'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'='>,
						   pegtl::any>>>{};
	struct GT : pegtl::seq<
		pegtl::one<'<'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'='>,
						   pegtl::any>>>{};
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
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'='>,
						   pegtl::any>>>{};
	struct OR : pegtl::seq<
		pegtl::one<'|'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'='>,
						   pegtl::any>>>{};
	struct ANDAND : pegtl::two<'&'>{};
	struct OROR : pegtl::two<'|'>{};
	struct QUERY : pegtl::one<'?'>{};
	struct COLON : pegtl::seq<
		pegtl::one<':'>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'>'>,
						   pegtl::any>>>{};
	struct SEMI : pegtl::one<';'>{};
	struct ELLIPSIS : pegtl::three<'.'>{};
	struct EQU : pegtl::seq<
		pegtl::one<'='>,
		pegtl::if_must<pegtl::not_at<
						   pegtl::one<'='>,
						   pegtl::any>>>{};
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
	struct RIGHTEUQ : pegtl::seq<
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
   // clang-format on
}  // namespace cat
