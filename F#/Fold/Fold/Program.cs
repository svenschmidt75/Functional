using System;
using System.Collections.Generic;
using System.Linq;

namespace Fold
{
    public static class Fold
    {
        public static T Imperative<T>(this IEnumerable<T> items, T accumulator, Func<T, T, T> func)
        {
            foreach (var item in items)
            {
                accumulator = func(accumulator, item);
            }
            return accumulator;
        }

        public static T Functional2<T>(this IEnumerable<T> items, T accumulator, Func<T, T, T> func)
        {
            if (items.Any() == false)
            {
                return accumulator;
            }
            return Functional2<T>(items.Skip(1), func(accumulator, items.First()), func);
        }

        public static T Functional<T>(this IEnumerable<T> items, T accumulator, Func<T, T, T> func)
        {
            return items.Any() == false ? accumulator : Functional<T>(items.Skip(1), func(accumulator, items.First()), func);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var lst = Enumerable.Range(1, 6);
            Console.WriteLine(lst.Aggregate(0, (i1, i2) => i1 + i2));

            Console.WriteLine(lst.Imperative(0, (i1, i2) => i1 + i2));

            Console.WriteLine(lst.Functional(0, (i1, i2) => i1 + i2));

            Console.WriteLine(lst.Functional2(0, (i1, i2) => i1 + i2));
        }
    }
}
