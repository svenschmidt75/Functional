using System;
using System.Diagnostics;

namespace ReaderMonad
{
    public class ReaderMonad<T>
    {
        // func is a function that takes the state and returns an argument
        public Func<T, T> Func { get; }

        public ReaderMonad(Func<T, T> func)
        {
            Func = func;
        }
    }

    public static class ReaderMonadHelper
    {
        public static Func<T, T> RunReader<T>(this ReaderMonad<T> reader)
        {
            return reader.Func;
        }

        public static ReaderMonad<T> Bind<T>(this ReaderMonad<T> reader, Func<T, ReaderMonad<T>> func)
        {
            Func<T, T> f = state =>
            {
                // f :: a -> c -> b is wrapped as c -> b in reader
                // apply state to get b
                var value = reader.Func(state);

                // pass that b from reader into func, wrappes
                // g b :: c -> d
                var rm = func(value);

                // return that function g b :: c -> d
                return rm.Func(state);
            };

            // wrap g b :: c -> d into a new Reader monad
            return new ReaderMonad<T>(f);
        }
    }


    class Program
    {
        public static ReaderMonad<int> Function1(int value)
        {
            var f = new Func<int, int>(state => value + state);
            return new ReaderMonad<int>(f);
        }

        public static ReaderMonad<int> Function2(int value)
        {
            var f = new Func<int, int>(state => value + 2 * state);
            return new ReaderMonad<int>(f);
        }

        static void Main(string[] args)
        {
            Test1();
            Test2();
        }

        private static void Test1()
        {
            var rm1 = Function1(3);
            var rm2 = rm1.Bind(Function2);

            int state = 1;
            var value = rm2.Func(state);
            Console.WriteLine("Final value: {0}", value);
            Debug.Assert(value == 6);
        }

        private static void Test2()
        {
            var rm1 = Function1(3);
            var rm2 = rm1.Bind(
                state =>
                {
                    return Function2(state);
                });

            int state_value = 1;
            var value = rm2.Func(state_value);
            Console.WriteLine("Final value: {0}", value);
            Debug.Assert(value == 6);
        }

    }
}
