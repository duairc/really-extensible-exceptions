# Really extensible exceptions

This package, `really-extensible-exceptions` suggests a design pattern for
(and provides the supporting type class machinery for) creating hierarchies of
exceptions in Haskell. Haskell already supports extensible hierarchies of
exceptions using a design pattern suggested in the documentation of the
`Control.Exception` module: however, a significant amount of boilerplate is
required with this pattern, especially for complex hierarchies, and the
pattern is limited in the sense that it does not support multiple inheritance.
`really-extensible-exceptions` reduces (but does not completely eliminate) the
boilerplate required, and also supports multiple inheritance: for example, you
could have an exception type `ConnectionTimedOut` which is both a
`TimeoutException` and a `NetworkException` (and is catchable as such),
without `TimeoutException` being a subclass of `NetworkException` (or
vice-versa).

`really-extensible-exceptions` requires GHC 7.8 or later, as it relies on the
the new polymorphic `Typeable` class which comes with this version of GHC.

## Example from `Control.Exception`

The documentation from the `Control.Exception` module presents a simplified
exception hierarchy where an exception type `MismatchedParentheses` is an
instance of `FrontendException`, which is itself a subclass of
`CompilerException`. The following code is how this hierarchy is implemented
with the standard design pattern:

    --------------------------------------------------------------------------
    -- CompilerException
    class Exception e => CompilerException e where
        toCompilerException :: e -> SomeCompilerException
        toCompilerException = SomeCompilerException

        fromCompilerException :: SomeCompilerException -> Maybe e
        fromCompilerException (SomeCompilerException e) = cast e

    data SomeCompilerException =
        forall e. CompilerException e => SomeCompilerException e
      deriving Typeable

    instance Show SomeCompilerException where
        show (SomeCompilerException e) = show e

    instance Exception SomeCompilerException

    instance CompilerException SomeCompilerException where
        toCompilerException = id
        fromCompilerException = Just

    compilerExceptionToException :: CompilerException e => e -> SomeException
    compilerExceptionToException = toException . toCompilerException

    compilerExceptionFromException :: CompilerException e => SomeException -> Maybe e
    compilerExceptionFromException = fromException <=< fromCompilerException

    --------------------------------------------------------------------------
    -- FrontendException

    class CompilerException e => FrontendException e where
        toFrontendException :: e -> SomeFrontendException
        toFrontendException = SomeFrontendException

        fromFrontendException :: SomeFrontendException -> Maybe e
        fromFrontendException (SomeFrontendException e) = cast e

    data SomeFrontendException =
        forall e. FrontendException e => SomeFrontendException e
      deriving Typeable

    instance Show SomeFrontendException where
        show (SomeFrontendException e) = show e

    instance Exception SomeFrontendException where
        toException = compilerExceptionToException
        fromException = compilerExceptionFromException

    instance CompilerException SomeFrontendException

    instance FrontendException SomeFrontendException where
        toFrontendException = id
        fromFrontendException = Just

    frontendExceptionToException :: FrontendException e => e -> SomeException
    frontendExceptionToException = toException . toFrontendException

    frontendExceptionFromException :: FrontendException e => SomeException -> Maybe e
    frontendExceptionFromException = fromFrontendException <=< fromException

    --------------------------------------------------------------------------
    -- MismatchedParentheses

    data MismatchedParentheses = MismatchedParentheses
        deriving (Show, Typeable)

    instance Exception MismatchedParentheses where
        toException = frontendExceptionToException
        fromException = frontendExceptionFromException

    instance CompilerException MismatchedParentheses where
        toCompilerException = toCompilerException . toFrontendException
        fromCompilerException = fromFrontendException <=< fromCompilerException

    instance FrontendException MismatchedParentheses

We can now catch a `MismatchedParentheses` exception as
`MismatchedParentheses`, `SomeFrontendException` or `SomeCompilerException`,
but not other types, e.g. `IOException`:

    >>> throwIO MismatchedParentheses `catch` \e -> putStrLn ("Caught " ++ show (e :: MismatchedParentheses))
    Caught MismatchedParentheses

    >>> throwIO MismatchedParentheses `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeFrontendException))
    Caught MismatchedParentheses

    >>> throwIO MismatchedParentheses `catch` \e -> putStrLn ("Caught " ++ show (e :: SomeCompilerException))
    Caught MismatchedParentheses

    >>> throwIO MismatchedParentheses `catch` \e -> putStrLn ("Caught " ++ show (e :: IOException))
    *** Exception: MismatchedParentheses

## With `Control.Exception.ReallyExtensible`

The following code is an implementation of the above exception hierarchy using
the machinery in `Control.Exception.ReallyExtensible`.

    --------------------------------------------------------------------------
    -- CompilerException
    class Exception e => CompilerException e

    deriving instance Typeable CompilerException

    instance CompilerException (Some CompilerException)

    type SomeCompilerException = Some CompilerException

    --------------------------------------------------------------------------
    -- FrontendException
    class CompilerException e => FrontendException e

    deriving instance Typeable FrontendException

    instance CompilerException (Some FrontendException)

    instance FrontendException (Some FrontendException)

    type SomeFrontendException = Some FrontendException

    --------------------------------------------------------------------------
    -- MismatchedParentheses
    data MismatchedParentheses = MismatchedParentheses
      deriving (Show, Typeable)

    instance CompilerException MismatchedParentheses

    instance FrontendException MismatchedParentheses

    instance Exception MismatchedParentheses where
        toException = toExtensibleException
            [ catchableAs (Proxy :: Proxy CompilerException)
            , catchableAs (Proxy :: Proxy FrontendException)
            ]
        fromException = fromExtensibleException

As you can see, while there is still some boilerplate code, there's a good bit
less of it. Two things to note: firstly, in addition to exception *types*
(like `MismatchedParentheses`), we alse make exception *predicates* (like
`CompilerException`) instances of `Typeable`. (This is what `deriving instance
Typeable CompilerException` means, and is why we require GHC 7.8's new
polymorphic `Typeable` class.) The other is that we get rid of all the
existential types for each exception predicate (i.e., `SomeCompilerException`
for `CompilerException`), and we instead have a "universal" existential type
`Some` that is polymorphic over all exception predicates. (We can optionally
provide type synonyms like `type SomeCompilerException = Some
CompilerException` for compatibility if we like.)

The other big difference is that we've eliminated the need for operations such
as `toFrontendException`, `fromFrontendException`,
`frontendExceptionToException` and `frontendExceptionFromException`. Instead
all we need to do is to use `toExtensibleException` and
`fromExtensibleException` in the `Exception` instance for our exception types.
`toExtensibleException` gets passed a list of `Cast`s, constructed with
`catchableAs`, to which we pass the predicates (via `Proxy`s) catchable as
which we want our exception type to be. In the code above, we want
`MismatchedParentheses` to be catchable as `CompilerException` and
`FrontendException`. Because there are no restrictions on the predicates we
supply to `toExtensibleException` for a given exception type (other than that
the predicates are satisfied by the type), this pattern is flexible enough to
easily support arbitrarily complicated exception hierarchies, including those
using multiple inheritance.