Intermission: Check your understanding

1. What functions are being imported from `Contro.Monad`?
    * A: `forever` and `when`
2. What imports are both unqualified and imported in their entirety?
    * A: `Data.Bits` and `Database.Blacktip.Types`
3. From the name, what do you suppose importing the `Types` module brings in?
    * Data types used in the curent module
4. Now, let's compare a small part of `blacktip`'s code to the above import list.
    1. The type signature refers to three aliased imports. What modules are named in those aliases?
        * A: `MV.MVar` == `Control.Concurrent.MVar.MVar`
        * A: `FPC` == `Filsystem.Path.CurrentOS`
        * A: `CC` == `Control.Concurrent`
    2. Which import does `FS.writeFile` refer to?
        * A: `Filesystem`
    3. Which import does `forever` come from?
        * A: `Control.Monad`
