using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Linq.Expressions;
using System.Security.Cryptography.X509Certificates;
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
            var result2 = result1.Bind(p.Bar);
            Console.WriteLine($"Result: {result2.What}");

            // error
            var result3 = result2.Bind(p.Fail);
            Console.WriteLine($"Result: {result3.What}");

            // still error
            var result4 = result3.Bind(p.Bar);
            Console.WriteLine($"Result: {result4.What}");



            // F o Y, F composed Y
            Func<int, Either<int, string>> FCompY = a =>
            {
                var t1 = p.Foo(a);
                return t1.Bind(p.Bar);
            };
            Console.WriteLine($"Result: {FCompY(1).What}");




            // proper composition function
            Func<int, Either<int, string>> f = x => p.Foo(x);
            Func<int, Either<int, string>> g = x => p.Bar(x);
            Func<int, Either<int, string>> h = Either.Compose(f, g);
            Console.WriteLine($"Result: {h(1).What}");
        }
    }
}