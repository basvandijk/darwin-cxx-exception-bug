#include "foo.hpp"
#include "exception"

extern "C"
{
	int foo_wrapper(void)
	{
		try
		{
			foo();
			return 0;
		}
		catch (const std::exception & e)
		{
			return 1;
		}
	}
}