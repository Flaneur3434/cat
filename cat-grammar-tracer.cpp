#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/trace.hpp>
#include <tao/pegtl/contrib/utf32.hpp>

#include "cat.hpp"

namespace pegtl = tao::pegtl;

int main( int argc, char** argv )
{
   if( argc != 2 )
      return 1;

   pegtl::argv_input<> in( argv, 1 );
   pegtl::standard_trace< cat::Constant >( in );

   return 0;
}
