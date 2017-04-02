using System;
using System.Globalization;

namespace Either2
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
            // this is no longer possible
//            var z = G(F(1));
//            Console.WriteLine($"Result: {z}");

            var x = F(1);
            if (x.What == Either<double, string>.Type.Error)
            {
                Console.WriteLine($"Error: {x.Error}");
                return;
            }
            var y = G(x.Value);
            if (y.What == Either<string, string>.Type.Error)
            {
                Console.WriteLine($"Error: {y.Error}");
                return;
            }
            Console.WriteLine($"Result: {y.Value}");
        }
    }
}