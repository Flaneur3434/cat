#include <tao/pegtl.hpp>

using namespace tao::pegtl;

namespace modulus
{
   template< unsigned M, unsigned R = 0 >
   struct my_rule
   {
      static_assert( M > 1, "Modulus must be greater than 1" );
      static_assert( R < M, "Remainder must be less than modulus" );

      template< typename ParseInput >
      static bool match( ParseInput& in )
      {
         if( !in.empty() ) {
            if( ( ( *in.current() ) % M ) == R ) {
               in.bump( 1 );
               return true;
            }
         }
         return false;
      }
   };

   struct grammar
      : until< eolf, must< my_rule< 3 > > >
   {};

}  // namespace modulus

int main( int argc, char** argv )
{
   if( argc > 1 ) {
      argv_input in( argv, 1 );
      parse< modulus::grammar >( in );
   }
   return 0;
}
