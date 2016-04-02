using System;
using System.Runtime.Remoting.Messaging;

namespace WriterMonad
{
    public static class TupleHelper
    {
        public static void Bind<T1, T2>(this Tuple<T1, T2> tuple, Action<T1, T2> action)
        {
            action(tuple.Item1, tuple.Item2);
        }
    }

    public static class WriterMonadHelper
    {
        public static WriterMonad Return(int value, string log)
        {
            return new WriterMonad(value, log);
        }

        // Monad: m :: m a -> (a -> m b) -> m b
        public static WriterMonad Bind(this WriterMonad writerMonad, Func<int, WriterMonad> func)
        {
            int value1 = 0;
            string log1 = String.Empty;
            writerMonad.RunWriter().Bind(
                (item1, item2) =>
                {
                    value1 = item1;
                    log1 = item2;
                });

            int value2 = 0;
            string log2 = String.Empty;
            func(value1).RunWriter().Bind(
                (item1, item2) =>
                {
                    value2 = item1;
                    log2 = item2;

                });
            return Return(value2, log1 + log2);
        }

        public static Tuple<int, string> RunWriter(this WriterMonad writerMonad)
        {
            return Tuple.Create(writerMonad.Value, writerMonad.Log);
        }

    }

    public class WriterMonad
    {
        public string Log { get; }

        public int Value { get; }

        public WriterMonad(int value, string log)
        {
            Value = value;
            Log = log;
        }

    }


    class Program
    {
        public static WriterMonad Half(int value)
        {
            return WriterMonadHelper.Return(value / 2, "halfed " + value);
        }

        static void Main(string[] args)
        {
            WriterMonad result1 = Half(10);
            var result2 = result1.Bind(Half);
            var result3 = result2.Bind(Half);

            int value;
            string log;
            result3.RunWriter().Bind(
                (item1, item2) =>
                {
                    value = item1;
                    log = item2;
                });



            var result4 = Half(10).Bind(
                i =>
                {
                    return Half(i).Bind(
                        i1 =>
                        {
                            return Half(i1);

                        });
                });

            result4.RunWriter().Bind(
                (item1, item2) =>
                {
                    value = item1;
                    log = item2;
                });

        }
    }
}
