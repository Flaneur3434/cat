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

	// WhiteSpace  <- [ \n\r\t]
	struct whitespace : pegtl::space{};

	// LineComment <- '//' (!'\n' .)
	struct line_comment : pegtl::seq<pegtl::two<'/'>, pegtl::until< pegtl::eolf >>{};

	// LongComment <- '/*' (!'*/'.)* '*/'
	struct multi_line_comment : pegtl::if_must<
		pegtl::seq<pegtl::one<'/'>, pegtl::one<'*'>>,
		pegtl::not_at<pegtl::seq<pegtl::one<'*'>, pegtl::one<'/'>>>,
		pegtl::seq<pegtl::one<'*'>, pegtl::one<'/'>>> {};

	// Pragma      <- '#'  (!'\n' .)*
	struct pragma : pegtl::seq<pegtl::one<'#'>, pegtl::until<pegtl::eolf>>{};

	struct spacing : pegtl::disable<pegtl::sor<whitespace, line_comment, multi_line_comment, pragma>>{};

   // clang-format on
}  // namespace cat
