﻿using System;

namespace Either2
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