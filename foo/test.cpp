#include <iostream>
#include "foo.hpp"

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

int main()
{
	int r = foo_wrapper();
	if (r != 0)
    {
		std::cout << "Whoops!\n";
		return 1;
    }
}