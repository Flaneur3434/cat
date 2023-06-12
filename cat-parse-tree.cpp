#include "cat.hpp"
#include <iostream>
#include <memory>
#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/parse_tree.hpp>

namespace pegtl = tao::pegtl;

int main( int argc, char** argv )
{
   pegtl::argv_input<> in( argv, 1 );
   auto root = pegtl::parse_tree::parse< cat::NonAsciiChar >( in );

   std::cout << root->string() << std::endl;
}
