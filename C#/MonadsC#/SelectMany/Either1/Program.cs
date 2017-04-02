using System;
using System.Globalization;

namespace Either1
{
    internal class Program
    {
        public static double F(int arg)
        {
            if (arg == 0)
            {
                throw new Exception("Argument null");
            }
            return 1.0 / arg;
        }

        public static string G(double arg)
        {
            if (arg < 0)
            {
                throw new Exception("Argument negative");
            }
            return arg.ToString(CultureInfo.InvariantCulture);
        }

        public static void Main(string[] args)
        {
            var z = G(F(1));
            Console.WriteLine($"Result: {z}");
        }
    }
}
