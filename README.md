# linebyline


## Install

Install `cabal`, then:

```bash
make install
```

Add `$HOME/.cabal/bin` into the `PATH` environment variable if not added yet!

*TODO: Portability*

## Why

I just figured out the `awk` is hard to learn and disappears
quickly from my mind. at the other side, `grep` and `sed` are not flexible
enough to quickly transform and process text files at the terminal.

Linebyline is just an one-liner text processor to:

- Split by any delimiter, combine columns and etc ...
- Specify a pattern to omit or keep the line.
- Specify a pattern to break the process.


## Anatomy

```
lbl [opts] -- expression A :: expression B`
```


Linebyline script consists of one or more expressions separated by `::`, In 
the example above, every line of the standard input will be injected into 
`expression A`, then result will pass into the `expression B`.


Let's another example, in the script:

```bash
echo "foo bar" | lbl split :: join ','

``` 

detailed instruction will be:

1. Split the input line by `space`:

```
split ' ' [input-line] -> [foo, bar]
```

2. The split result passes as the positional arguments into the `join ','`:

```
join ',' [foo, bar] -> foo,bar
```

3. Print the result and process next line.

```
foo bar
```

## Usage examples

Split by space and join with `-`:

```bash
echo "foo bar" | lbl split :: join '-'
```

Result: 

```
foo-bar
```

Split by comma and join with space:

```bash
echo "foo,bar" | lbl split ',' :: join
```

Result: 

```
foo bar
```

Ignore lines by regular expression

```bash
lbl ignore '^foo' << EOF
foo
bar
baz
EOF
```

Result: 

```
bar
baz
```

Suppress execution by regular expression

```bash
lbl break '^baz' << EOF
foo
bar
baz
qux
EOF
```

Result: 

```
foo
bar
```


## Operators

- `:0` -> **Argument**: get input arguments by index.
- `:0~3` -> **Argument Range**: get third input arguments.
- A `::` B -> **Pipe**: pass output of the first expression into the next one.

- A `:::` B -> **After**: Run expression `B` after `A` without passing the
    output, instead the whole line will be used as the input for the expression
    `B`

## Functions

- `join [sep] ...`
- `split [sep] ...`
- `grep pattern`
- `grab pattern`
- `ignore pattern`
- `break pattern`
- `replace pattern replacement ...`

## Special variables

- `:~`: get all input arguments
- `:2~`: get input arguments from index 2 to the end.
- `:~5`: take 5 input arguments from the first.
- `:l`: get the input lien.
- `:n`: get the line number.
