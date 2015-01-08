# align

Simple unix filter to align text on specified substrings.

`align` can be run from inside Vim or other editors with custom key bindings to
align lines of code neatly on programming language operators like =>, ::, @=?,
etc.

## Usage

Assume you want to align this text:

input.sample:
```
    "parseKeyExpr" ~: [ObjectKey "item"] @=? parseKeyExpr "item"
  , "ngEvalToString" ~: "apple" @=? ngEvalToString testContext1 "item" 
  , "ngEvalToString2" ~: "apple" @=? ngEvalToString testContext2 "item.name" 
```

You can do so with this command:

    align '~: @=?' < input.sample

which outputs:

```
    "parseKeyExpr"    ~: [ObjectKey "item"] @=? parseKeyExpr "item"
  , "ngEvalToString"  ~: "apple"            @=? ngEvalToString testContext1 "item"
  , "ngEvalToString2" ~: "apple"            @=? ngEvalToString testContext2 "item.name"
```

`align` takes an argument list of strings to align the text and performs
the alignment operation on the text it gets from STDIN.

`align` will only match each alignment string once, so if that string
occurs multiple times in a line, you need to specify it than many times 
in the argument.

## How to use align in Vim 

To use align from Vim, you can select some text, and then use a Vim
filter command:

    !align '~: @=?'

For common alignment operations, you can make Vim commands and put them
in your `.vimrc`, e.g.:

```vimscript
command! -range AlignHaskellTypeAnnotation :<line1>,<line2>!align '::'
vnoremap <leader>h :AlignHaskellTypeAnnotation<cr>

command! -range AlignHaskellTest :<line1>,<line2>!align '~: @=?'
vnoremap <leader>H :AlignHaskellTest<cr>

```

## Installation

You must first have the Haskll platform installed on your system:

* [Haskell platform](https://www.haskell.org/platform)

`git clone` this project to a local directory. Then from that directory, 
run this command:

```
cabal install
```

This will likely install the `align` executable in ~/.cabal/bin, which should be on your PATH.


## Author

Daniel Choi <https://github.com/danchoi>

