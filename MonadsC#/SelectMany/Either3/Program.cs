using System;
using System.Globalization;

namespace Either3
{
    internal class Program
    {
        public static Either<double, string> F(int arg)
        {
            if (arg == 0)
            {
                return Either.Error<double, string>("Argument null");
            }
            return Either.Ok<double, string>(1.0 / arg);
        }

        public static Either<string, string> G(double arg)
        {
            if (arg < 0)
            {
                return Either.Error<string, string>("Argument negative");
            }
            return Either.Ok<string, string>(arg.ToString(CultureInfo.InvariantCulture));
        }

        public static void Main(string[] args)
        {
            var x = F(2);
            var y = x.Bind(G);


            if (y.What == Either<string, string>.Type.Error)
            {
                Console.WriteLine($"Error: {y.Error}");
                return;
            }
            Console.WriteLine($"Result: {y.Value}");
        }
    }
}