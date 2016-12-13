using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiValuedFunction2
{
    internal class Program
    {
        public static IEnumerable<double> F(double a, double b, double c)
        {
            double discriminant = b * b - 4.0 * a * c;
            if (discriminant < 0)
            {
                return Enumerable.Empty<double>();
            }
            double x1 = (-b - Math.Sqrt(discriminant)) / 2.0 / a;
            double x2 = (-b + Math.Sqrt(discriminant)) / 2.0 / a;
            return new[] {x1, x2};
        }

        public static IEnumerable<double> G(double arg)
        {
            return new[] {-Math.Abs(arg), Math.Abs(arg)};
        }

        public static void Main(string[] args)
        {
            var y = F(2.0, 5.0, -3.0);
            var z = y.Bind(G);
            z.ForEach(Console.WriteLine);
        }
    }
}