misfortune
===========

This is a fortune-mod clone.  In addition to the features generally expected of a `fortune` program, this can be used as a Haskell library (`import Data.Fortune`) and also supports UTF-8 fortune files, configurable search paths, automatic merging of fortune databases with the same name (so you can have a local fortunes folder that just adds to existing fortune databases), filtering fortunes by line lengths, and a "print fortune matching regex" mode (instead of just "print all fortunes matching regex" mode).

Usage
------

Most of the command-line flags from `fortune` work with `misfortune` as well.  To just print a fortune, run:

    misfortune

To index a new fortune file (or update the index on an existing one), run:

    misfortune-strfile path/to/file

Note that `misfortune` uses a different format for its index files than `fortune` does.  If there is an existing fortune-mod index file, it will not be overwritten.

To use the fortune API in your Haskell programs:

    import Data.Fortune
    import qualified Data.Text as T
    
    main = do
        f <- openFortuneFile "pangrams" '%' True
        
        appendFortune f (T.pack "The quick brown fox jumps over the lazy dog.")
        appendFortune f (T.pack "Quick blowing zephyrs vex daft Jim.")
        
        closeFortuneFile f
        
        putStrLn =<< randomFortune ["pangrams"]

This example will create or append to a file "pangrams" in the working directory, and create or update the corresponding index file "pangrams.dat".  It then closes that file and requests a random fortune from all databases named "pangrams" in the search path - so it will either print one of the two just written or one found in another "pangrams" file.  Every eligible fortune is equally likely.

Installation
-------------

Get the current release from Hackage:

    cabal install misfortune

Or build the latest version from git:

    git clone https://github.com/mokus0/misfortune.git
    cd misfortune
    cabal install

