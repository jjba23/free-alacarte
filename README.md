
# Table of Contents

1.  [Free à la Carte](#org7848ba2)
2.  [How do you use this ?](#org7daa9b5)
3.  [More on the topic](#orgf4b2ed8)


<a id="org7848ba2"></a>

# Free à la Carte

Free monads based on  from intuitions from the Data types à la Carte paper. Combine functors and make embedded DSLs in Haskell.


<a id="org7daa9b5"></a>

# How do you use this ?

This section gives a brief demonstration of using free monads to model effects.

Four effectful functions are defined, categorized into two separate data types.

    data Teletype a
      = GetChar (Char -> a)
      | PutChar Char a
      deriving (Functor)
    
    data FileSystem a
      = ReadFile FilePath (String -> a)
      | WriteFile FilePath String a
      deriving (Functor)

If you are into it, you can also write the Functor instances by hand, for your free monads, e.g.:

    instance Functor Teletype where
    fmap :: (a -> b) -> Teletype a -> Teletype b
    fmap f = \case
      GetChar g   -> GetChar (f . g)
      PutChar c g -> PutChar c (f g)

An `exec` function can execute values of these data types using the `Free` free monad. This uses intuitions of category theory to describe imperative sequence of computations as a fold over a functor. **NOTE**: the `exec` function is provided by this library and you don't need to implement it yourself.

    exec :: Exec f => Free f a -> IO a
    exec = foldFree return execAlgebra

You should then write the `Exec` instances, in other words, the concrete implementations.
**NOTE**: the typeclass `Exec`, and `Exec (f :+: g)` instance are also provided by this library, and you don't need to implement it yourself.

    class Functor f => Exec f where
      execAlgebra :: f (IO a) -> IO a
    
    instance (Exec f, Exec g) => Exec (f :+: g) where
      execAlgebra = \case
        Left' e -> execAlgebra e
        Right' e -> execAlgebra e    
    
    -- write your own implementations 
    instance Exec Teletype where
      execAlgebra = \case
        GetChar f    -> Prelude.getChar >>= f
        PutChar c io -> Prelude.putChar c >> io
    
    instance Exec FileSystem where
      execAlgebra (ReadFile path f) = Prelude.readFile path >>= f
      execAlgebra (WriteFile path s f) = Prelude.writeFile path s >> f

Then we can define some smart constructors to create our embedded DSL and save us some boilerplate, while adding syntactic sugar.

    getChar :: (Teletype :<: f) => Free f Char
    getChar = injectFree (GetChar Pure)
    
    putChar :: (Teletype :<: f) => Char -> Free f ()
    putChar c = injectFree (PutChar c (Pure ()))
    
    readFile :: (FileSystem :<: f) => FilePath -> Free f String
    readFile path = injectFree (ReadFile path Pure)
    
    writeFile :: (FileSystem :<: f) => FilePath -> String -> Free f ()
    writeFile path s = injectFree (WriteFile path s (Pure ()))

The `cat` function serves as an example of composition. In the following, I use a more general type than that used in the paper. Here we use `mapM_` instead of `mapM` to discard the resulting list of unit.

    cat :: (FileSystem :<: f, Teletype :<: f) => FilePath -> Free f ()
    cat path = mapM_ putChar =<< readFile path

The following example uses the `cat` function to print the content of the README.md file in this directory.

    main :: IO ()
    main = exec @(FileSystem :+: Teletype) $ cat "README.md"


<a id="orgf4b2ed8"></a>

# More on the topic

I can only extremely recommend the following resources to gain more understanding about the ideas and intuitions behind this library, and behind Data types à la Carte.

-   Original paper, by Wouter Swierstra: <https://webspace.science.uu.nl/~swier004/publications/2008-jfp.pdf>
-   Powerpoint explanation, by Wouter Swierstra: <https://webspace.science.uu.nl/~swier004/talks/2018-fp-ams.pdf>
-   Good alternative explanation and implementation, by Travis Cardwell: <https://www.extrema.is/blog/2022/04/04/data-types-a-la-carte>

