# align

Simple unix filter to align text on specified substrings.

## Usage

Assume you want to align this text:

```
# input.sample:

    "parseKeyExpr" ~: [ObjectKey "item"] @=? parseKeyExpr "item"
  , "ngEvalToString" ~: "apple" @=?   ngEvalToString testContext1 "item" 
  , "ngEvalToString2" ~: "apple" @=?   ngEvalToString testContext2 "item.name" 
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

## Using in Vim 

To use align from Vim, you can select some text, and then use a Vim
filter command:

    !align '~: @=?'

For common alignment operations, you can make Vim commands and put them
in your `.vimrc`, e.g.:

```vimscript
command! -range AlignHaskellTest :<line1>,<line2>!align '~: @=?'
" optionally bind the command to a key:
vnoremap <leader>h :AlignHaskellTest<cr>

```

    