# Create Debian package for linebyline


## Prerequicites

```bash
sudo apt install autoconf automake autotools-dev \
                 debhelper and dh-make \
                 debmake
```

## Build

```bash
cd path/to/linebyline/deb
make
ls linebyline_*.deb
```


