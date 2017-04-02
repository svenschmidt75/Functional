using System;

namespace Either
{
    public static class Either
    {
        public static Either<T1, T2> Ok<T1, T2>(T1 value)
        {
            return new Either<T1, T2>(value);
        }

        public static Either<T1, T2> Error<T1, T2>(T2 error)
        {
            return new Either<T1, T2>(error);
        }

        // this is SelectMany's signature!!
        public static Either<TResult2, TError> Bind<TResult1, TError, TResult2>(this Either<TResult1, TError> ma, Func<TResult1, Either<TResult2, TError>> func)
        {
            if (ma.What == Either<TResult1, TError>.Type.Error)
            {
                return Either.Error<TResult2, TError>(ma.Error);
            }
            return func(ma.Value);
        }

        public static Func<TResult1, Either<TResult3, TError>> Compose<TResult1, TError, TResult2, TResult3>(Func<TResult1, Either<TResult2, TError>> f, Func<TResult2, Either<TResult3, TError>> g)
        {
            return x =>
            {
                var r1 = f(x);
                if (r1.What == Either<TResult2, TError>.Type.Error)
                {
                    return Either.Error<TResult3, TError>(r1.Error);
                }
                var r2 = g(r1.Value);
                return r2;
            };
        }

    }

    public class Either<T1, T2>
    {
        public Either(T1 value)
        {
            What = Type.Ok;
            Value = value;
        }

        public Either(T2 error)
        {
            What = Type.Error;
            Error = error;
        }

        public enum Type
        {
            Error,
            Ok
        };

        public T1 Value { get; private set; }

        public T2 Error { get; private set; }

        public Type What { get; private set; }
    }
}