# Hackline

## Why

I just figured out the `awk` is hard to learn and disappears
quickly from my mind. at the other side, `grep` and `sed` are not flexible
enough to quickly transform and process text files at the terminal.

Hackline is just an one-liner text processor to:

- Split by any delimiter, combine columns and etc ...
- Specify a pattern to omit or keep the line.


## Anatomy

```
hackline [opts] -- "Pattern" do a b c >>= do x` -- "Pattern 2" do ... 
```

## Operators

```
a >> b: (line -> a -> print out) then (line -> b -> print out)
```

```
a >>= b: line -> a -> b -> print out
```

## Functions

- `split :: str -> [str] -> [str]`
- `join :: str -> [str] -> [str]`
- `break :: break the file`
- `omit ::  omit current line`
- `print :: [str] -> [str]`

