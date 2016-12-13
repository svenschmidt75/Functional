using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiValuedFunction1
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
            var result = new List<double>(4);
            var y = F(2.0, 5.0, -3.0);
            y.ForEach(r =>
            {
                var z = G(r);
                result.AddRange(z);
            });
            result.ForEach(Console.WriteLine);
        }
    }
}