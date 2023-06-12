#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/analyze.hpp>

#include <iostream>

#include "cat.hpp"

namespace pegtl = tao::pegtl;

int main()
{
   if( const auto problems = pegtl::analyze< pegtl::plus< cat::NonAsciiChar > >() != 0 ) {
      std::cerr << "problems: " << problems << std::endl;
      return 1;
   }
   return 0;
}
