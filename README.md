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

    align '~: @=?' < input.sampe

which outputs:

    "parseKeyExpr"    ~: [ObjectKey "item"] @=? parseKeyExpr "item"
  , "ngEvalToString"  ~: "apple"            @=? ngEvalToString testContext1 "item"
  , "ngEvalToString2" ~: "apple"            @=? ngEvalToString testContext2 "item.name"

`align` takes an argument list of strings to align the text and performs
the alignment operation on the text it gets from STDIN.

To use align from Vim, you can select some text, and then use a Vim
filter command:

    !align '~: @=?'
