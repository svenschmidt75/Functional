using System;
using System.Diagnostics;

namespace StateMonad
{
    public class StateMonad<T>
    {
        // func is a function that takes state and returns an argument
        // and new state
        public Func<T, Tuple<T, T>> Func { get; }

        public StateMonad(Func<T, Tuple<T, T>> func)
        {
            Func = func;
        }
    }

    public static class StateMonadHelper
    {
        public static Func<T, Tuple<T, T>> RunReader<T>(this StateMonad<T> reader)
        {
            return reader.Func;
        }

        public static StateMonad<T> Bind<T>(this StateMonad<T> stateMonad, Func<T, StateMonad<T>> func)
        {
            Func<T, Tuple<T, T>> f = state =>
            {
                // f :: a -> c -> (b, c) is wrapped as c -> (b, c) in stateMonad
                // apply state to get b and the new state
                Tuple<T, T> ret = stateMonad.Func(state);
                var value = ret.Item1;
                var newState = ret.Item2;

                // pass that b (value) from stateMonad into func, wrappes
                // g b :: c -> (d, c)
                var rm = func(value);

                // return that function g b :: c -> (d, c)
                // which, when evaluated, passes the new state
                // into g to get (d, c)
                return rm.Func(newState);
            };

            // wrap g b :: c -> (d, c) into a new State monad
            return new StateMonad<T>(f);
        }
    }


    class Program
    {
        public static StateMonad<int> Function1(int value)
        {
            var f = new Func<int, Tuple<int, int>>(state => Tuple.Create(value + state, 2 * state));
            return new StateMonad<int>(f);
        }

        public static StateMonad<int> Function2(int value)
        {
            var f = new Func<int, Tuple<int, int>>(state => Tuple.Create(2 * value + 3 * state, 9 * state));
            return new StateMonad<int>(f);
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

            int state = 5;
            var value = rm2.Func(state);
            Console.WriteLine("Final value: {0}", value);
            Debug.Assert(value.Item1 == 46);
            Debug.Assert(value.Item2 == 90);
        }

        private static void Test2()
        {
            var rm1 = Function1(3);
            var rm2 = rm1.Bind(
                state =>
                {
                    return Function2(state);
                });

            int state_value = 5;
            var value = rm2.Func(state_value);
            Console.WriteLine("Final value: {0}", value);
            Debug.Assert(value.Item1 == 46);
            Debug.Assert(value.Item2 == 90);
        }
    }
}
