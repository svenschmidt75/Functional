using System;
using System.Collections.Generic;
using System.Linq;

namespace MultiValuedFunction2
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

        public static IEnumerable<TResult> Bind<TSource, TResult>(this IEnumerable<TSource> source,
            Func<TSource, IEnumerable<TResult>> func)
        {
            IEnumerable<TResult> result = Enumerable.Empty<TResult>();
            source.ForEach(value =>
            {
                var tmp = func(value);
                result = result.Concat(tmp);
            });
            return result.ToList();
        }
    }
}