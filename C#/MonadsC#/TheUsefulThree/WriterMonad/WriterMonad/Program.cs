using System;

namespace WriterMonad
{
    public static class TupleHelper
    {
        public static void Bind<T1, T2>(this Tuple<T1, T2> tuple, Action<T1, T2> action)
        {
            action(tuple.Item1, tuple.Item2);
        }
    }

    public class LogWriterMonad<T>
    {
        public string Log { get; }

        public T Value { get; }

        public LogWriterMonad(T value, string log)
        {
            Value = value;
            Log = log;
        }

    }

    public static class LogWriterMonadHelper
    {
        public static LogWriterMonad<T> Return<T>(T value, string log)
        {
            return new LogWriterMonad<T>(value, log);
        }

        // Monad: m :: m a -> (a -> m b) -> m b
        public static LogWriterMonad<T> Bind<T>(this LogWriterMonad<T> logWriterMonad, Func<T, LogWriterMonad<T>> func)
        {
            T value1 = default(T);
            string log1 = String.Empty;
            logWriterMonad.RunWriter().Bind(
                (item1, item2) =>
                {
                    value1 = item1;
                    log1 = item2;
                });

            T value2 = default(T);
            string log2 = String.Empty;
            func(value1).RunWriter().Bind(
                (item1, item2) =>
                {
                    value2 = item1;
                    log2 = item2;

                });
            return Return(value2, log1 + log2);
        }

        public static Tuple<T, string> RunWriter<T>(this LogWriterMonad<T> logWriterMonad)
        {
            return Tuple.Create(logWriterMonad.Value, logWriterMonad.Log);
        }

    }

    class Program
    {
        // This method has type signature Half :: a -> m b
        public static LogWriterMonad<int> Half(int value)
        {
            return LogWriterMonadHelper.Return(value / 2, "halfed " + value);
        }

        static void Main(string[] args)
        {
            Test1();
            Test2();
        }

        private static void Test1()
        {
            LogWriterMonad<int> result1 = Half(10);
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
        }

        private static void Test2()
        {
            int value;
            string log;
            var result = Half(10).Bind(
                i =>
                {
                    return Half(i).Bind(
                        i1 => { return Half(i1); });
                });

            result.RunWriter().Bind(
                (item1, item2) =>
                {
                    value = item1;
                    log = item2;
                });
        }
    }
}
