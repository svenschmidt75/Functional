using System;
using System.Collections.Generic;

namespace MultiValuedFunction1
{
    public static class EnumerableHelper
    {
        public static void ForEach<T>(this IEnumerable<T> source, Action<T> action)
        {
            foreach (var item in source)
            {
                action(item);
            }
        }
    }
}