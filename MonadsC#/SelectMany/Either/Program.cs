using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Linq.Expressions;
using System.Threading.Tasks;

namespace Either
{
    internal class Program
    {
        public Either<int, string> Foo(int arg)
        {
            return Either.Ok<int, string>(arg);
        }

        public Either<int, string> Bar(int arg)
        {
            return Either.Ok<int, string>(arg);
        }

        public Either<int, string> Fail(int arg)
        {
            return Either.Error<int, string>("Failed");
        }

        public static void Main(string[] args)
        {
            var p = new Program();
            var result1 = p.Foo(1);
            var result2 = result1.Bind(value => p.Bar(value));
            Console.WriteLine($"Result: {result2.What}");

            var result3 = result2.Bind(value => p.Fail(value));
            Console.WriteLine($"Result: {result3.What}");
        }
    }
}