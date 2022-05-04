# Hackline

## Why

I just figured out the `awk` is hard to learn and disappears
quickly from my mind. at the other side, `grep` and `sed` are not flexible
enough to quickly transform and process text files at the terminal.

Hackline is just an one-liner text processor to:

- Split by any delimiter, combine columns and etc ...
- Specify a pattern to omit or keep the line.
- Specify a pattern to break the process.


## Anatomy

```
hackline [opts] -- expression A :: expression B`
```


Hackline script consists of one or more expressions separated by `::`, In 
the example above, every line of the standard input will be injected into 
`expression A`, then result will pass into the `expression B`.


## Install

?Portability

## Usage examples

Split by space and join with `-`:

```bash
echo "foo bar" | hackline split :: join '-'
```

Result: 

```
foo-bar
```

Split by comma and join with space:

```bash
echo "foo,bar" | hackline split ',' :: join
```

Result: 

```
foo bar
```

Ignore lines by regular expression

```bash
hackline ignore '^foo' << EOF
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
hackline break '^baz' << EOF
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

- A `:::` B -> **After** Run expression `B` after `A` without passing the
    output, instead the whole line will be used as the input for the expression
    `B`

## Functions

TODO:

## Special variables

TODO:

- `:l`
- `:n`
