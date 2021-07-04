# Clone

```shell
gti clone https://github.com/SirJosh3917/jssat.git
git submodule update --init --recursive --remote
```

# Build

**Debug**

```shell
cargo make dev
```

**Release**

```shell
cargo make release
```

# Guidelines

- When zipping two iterators, always assert that the length of both iterators
  are equal.
