[![Tests](https://github.com/AldanTanneo/bigints/actions/workflows/run_tests.yml/badge.svg)](https://github.com/AldanTanneo/bigints/actions/workflows/run_tests.yml)

# SPARK Constant Time Big Integer library

Implementation of a constant time big integer library, heavily inspired by [crypto-bigint](https://github.com/RustCrypto/crypto-bigint).

## Usage

The implementations are generic over the size of the integer

```ada
package U256 is new Bigints.Uint (256);
package U1024 is new Bigints.Uint (1024);
```

There is also a generic package to deal with modular integers (ie, over a prime field)

```ada
P : U256 := ...--  a big prime, like 2^255 - 19
package GF_P is new Bigints.Modular (U256, P);
```

All functions are implemented in constant time, except those with an explicit `_Vartime` suffix. Overloaded operators are also constant time.

> ⚠️ The constant time choice primitives like `Choice_From_Condition`, `Cond_Select`, `CSwap` rely on best-effort optimisation barriers.

## Tests

The library is formally checked using `gnatprove`. When contracts are respected, there cannot be any runtime errors.

Some functional contracts try to go a bit further and prove more advanced behaviours (WIP).

Tests are implemented in `./tests/src/`. Run them (UNIX only) with

```sh
cd tests
./run.sh
```