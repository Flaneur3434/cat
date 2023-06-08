/*
 * PEGTL grammar for C w/ Algebraic Types
 */

#pragma once

#include "tao/pegtl/ascii.hpp"
#include "tao/pegtl/rules.hpp"
#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>

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

	struct spacing : pegtl::disable<pegtl::sor<whitespace, line_comment, multi_line_comment, pragma>>{};

	//+--------------
	// Keywords
	//+--------------

	struct AUTO      : TAO_PEGTL_KEYWORD(auto);
	struct BREAK     : TAO_PEGTL_KEYWORD(break);
	struct CASE      : TAO_PEGTL_KEYWORD(case);
	struct CHAR      : TAO_PEGTL_KEYWORD(char);
	struct CONST     : TAO_PEGTL_KEYWORD(const);
	struct CONTINUE  : TAO_PEGTL_KEYWORD(continue);
	struct DEFAULT   : TAO_PEGTL_KEYWORD(default);
	struct DOUBLE    : TAO_PEGTL_KEYWORD(double);
	struct DO        : TAO_PEGTL_KEYWORD(do);
	struct ELSE      : TAO_PEGTL_KEYWORD(else);
	struct ENUM      : TAO_PEGTL_KEYWORD(enum);
	struct EXTERN    : TAO_PEGTL_KEYWORD(extern);
	struct FLOAT     : TAO_PEGTL_KEYWORD(float);
	struct FOR       : TAO_PEGTL_KEYWORD(for);
	struct GOTO      : TAO_PEGTL_KEYWORD(goto);
	struct IF        : TAO_PEGTL_KEYWORD(if);
	struct INT       : TAO_PEGTL_KEYWORD(int);
	struct INLINE    : TAO_PEGTL_KEYWORD(inline);
	struct LONG      : TAO_PEGTL_KEYWORD(long);
	struct REGISTER  : TAO_PEGTL_KEYWORD(register);
	struct RESTRICT  : TAO_PEGTL_KEYWORD(restrict);
	struct RETURN    : TAO_PEGTL_KEYWORD(return);
	struct SHORT     : TAO_PEGTL_KEYWORD(short);
	struct SIGNED    : TAO_PEGTL_KEYWORD(signed);
	struct SIZEOF    : TAO_PEGTL_KEYWORD(sizeof);
	struct STATIC    : TAO_PEGTL_KEYWORD(static);
	struct STRUCT    : TAO_PEGTL_KEYWORD(struct);
	struct SWITCH    : TAO_PEGTL_KEYWORD(switch);
	struct TYPEDEF   : TAO_PEGTL_KEYWORD(typedef);
	struct UNION     : TAO_PEGTL_KEYWORD(union);
	struct UNSIGNED  : TAO_PEGTL_KEYWORD(unsigned);
	struct VOID      : TAO_PEGTL_KEYWORD(void);
	struct VOLATILE  : TAO_PEGTL_KEYWORD(volatile);
	struct WHILE     : TAO_PEGTL_KEYWORD(while);
	struct BOOL      : TAO_PEGTL_KEYWORD(bool);
	struct COMPLEX   : TAO_PEGTL_KEYWORD(_Complex);
	struct STDCALL   : TAO_PEGTL_KEYWORD(_stdcall);
	struct DECLSPEC  : TAO_PEGTL_KEYWORD(__declspec);
	struct ATTRIBUTE : TAO_PEGTL_KEYWORD(__attribute__);

	struct keyword : pegtl::disable < pegtl::sor<
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
		UNISGNED,
		VOID,
		VOLATILE,
		WHILE,
		BOOL,
		COMPLEX,
		STDCALL,
		DECLSPEC,
		ATTRIBUTE > >{};

        // clang-format on
}  // namespace cat
