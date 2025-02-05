# Table of Contents
- [Table of Contents](#table-of-contents)
- [Setup](#setup)
- [About these exercises](#about-these-exercises)
- [On Feistel networks](#on-feistel-networks)
- [Exercises](#exercises)
  - [Exercise 1: A pure Feistel Network](#exercise-1-a-pure-feistel-network)
    - [1a](#1a)
    - [1b](#1b)
    - [1c](#1c)
    - [1d](#1d)
  - [Exercise 2: Pipelining](#exercise-2-pipelining)
    - [2a](#2a)
    - [2b](#2b)
    - [2c. (optional)](#2c-optional)
    - [2d](#2d)
    - [2e. Questions](#2e-questions)
  - [Exercise 3: Instruction machines](#exercise-3-instruction-machines)
    - [3a](#3a)
    - [3b](#3b)
    - [3c](#3c)
    - [3d](#3d)

# Setup
You need a recent Stack on your PATH. To synthesize the designs, we're using Clash 1.8. To build the project, run:

```
stack build
```

To run the tests, run:

```
stack test
```

To see the various options Tasty offers, run:

```
stack test --test-arguments '--help'
```

# About these exercises
During this set of exercises we are going to build a [Feistel Cipher](https://en.wikipedia.org/wiki/Feistel_cipher) in Clash. The goal is of course not to build properly vetted crypto hardware, but to illustrate and practice Clash concepts. Most exercises will merely point to existing documentation at [hackage.haskell.org/package/clash-prelude](https://hackage.haskell.org/package/clash-prelude) instead of providing it here. We appreciate feedback on how to improve these documents.

After these exercises you should:

* ..be able to reason about the HDL Clash will produce
* ..feel confident applying higher-order function such as `foldl`
* ..be able to use basic data structures to express the core of your problems
* ..know how to use _type parameters_ and _static arguments_ to create parameterizable hardware designs
* ..(_optional_) seen how you can use Clash's `DSignal` to prevent common _timing_ errors in your hardware designs

All exercises have solutions. See `src/Clash/Feistel/Impl*.hs`.

# On Feistel networks
A _Feistel Network_ is a structure used to develop symmetric block ciphers, originally developed by _Horst Feistel_ at IBM. By their nature, Feistel Networks have exactly the same structure for _encryption_ and _decryption_. In the worst case, only a key reschedule has to be staged to change from one to the other. Although some variation exists in existing Feistel Networks, most ciphers distinguish themselves by implementing a different _round function_ (displayed as _F_ in the image below). Thanks to its clever arrangement, round functions do not have to be reversible.

For these exercises we will use an (insecure!) pre-made round function. We will focus on generating the structure and scheduling round keys instead. The following image captures the essence of a Feistel Network:

![Feistel Network](imgs/800px-Feistel_cipher_diagram_en.svg.png)

About the image: [Feistel_cipher_diagram.svg: Amirki derivative work: Amirki / CC BY-SA](https://commons.wikimedia.org/wiki/File:Feistel_cipher_diagram_en.svg)

# Exercises
Scaffolding for exercises (and their solutions) can be found in `src/Clash/Feistel`.

## Exercise 1: A pure Feistel Network
In this exercise we are going to build a _pure_ Feistel Network. I.e., a Feistel network that does _not_ have any internal state and does all its work in a single clock cycle. Take a moment to read and understand `src/Clash/Feistel/Ex1.hs`. Also feel free to look around in `clash-feistel.cabal` to get a feel for how to setup a Clash/Haskell project.

### 1a
Provide a definition for `feistel`. Look at the various functions in [Clash.Sized.Vector](https://hackage.haskell.org/package/clash-prelude-1.8.2/docs/Clash-Sized-Vector.html) to get an idea on how to compose a number of functions.

### 1b
Parameterize `feistel` in its round function.

### 1c
Create a type alias for the (function-)type of a _round function_ and use it in the type signatures of `round` and `feistel`.

### 1d
Implement a number of (property) tests for the Feistel network. Extend `tests/Tests/Clash/Feistel/Ex1.hs`. You can run your tests with `stack test --test-arguments '-p Ex1'`.

<!-- Note: the solutions to this exercise are stored in `tests/Tests/Clash/Feistel/Impl1.hs`. -->

## Exercise 2: Pipelining
You have hopefully found it straightforward enough to implement a pure Feistel Network. Such a pure network cannot be expected to yield efficient hardware, due to its (potentially very) long critical path. In other words, you can only expect your target platform to run at very low clock speeds. To mitigate this, we need to pipeline the design.

### 2a
Implement your network in terms of `Signal`s.

### 2b
Add a `register` after every round.

### 2c. (optional)
Clash provides an API to annotate _delays_ on `Signal`s, in the form of `DSignal`. See the documentation [over here](https://hackage.haskell.org/package/clash-prelude-1.8.2/docs/Clash-Signal-Delayed.html). When used, Clash will provide errors if users try to combine two signals with different delays. Besides an additional level of safety, this can be used to automatically insert an appropriate number of registers to synchronize two signals. For this exercise, do two things:

1. Convert your design to use `DSignal` instead of `Signal`. Hint: inspect and use `Clash.Feistel.Util`. (_If you like the challenge, implement the functions in there yourself._)

2. Do not add pipelining-registers in the function assembling the Feistel Network. Instead, convert your round function to annotate its delay using `DSignal`s and use that type information to insert the appropriate number of registers in the parts feeding the keys/words to the round function.

Hint: Check out the definition of `RoundFunction` in the solution `src/Clash/Feistel/Impl2c.hs`.

### 2d
Like `1d`, write property tests for your pipelined Feistel Network. Use [sampleN](https://hackage.haskell.org/package/clash-prelude-1.8.2/docs/Clash-Signal-Internal.html#v:sampleN) to _sample_ a `Signal`.

Hint: if you use `sampleN` on a function with a hidden reset, `sampleN` will generate a reset signal. This reset signal is asserted for a single cycle.

### 2e. Questions
(To be answered in the session discussing the solutions to the exercises / the next few exercises.)

1. How many registers do you need to represent your Feistel Network?

2. Where is your network's critical path?

3. How could you further shorten the critical path?

## Exercise 3: Instruction machines
After implementing exercises 2{a,b,c} we can expect our pipelined design to run at reasonable clock speeds. We can still do a lot better though. As it currently stands, our users can do a full key reschedule every clock cycle. To facilitate this, we insert ~_½n²_ registers, where _n_ is the number of rounds our network uses. That is a lot of resources! In reality, a user is probably not interested in a full reschedule every cycle. A much more likely scenario is that of a single reschedule followed by a large number of encryption/decryption operations.

### 3a
Implement a function wrapping the round function. It should either store a given key, or mangle data with a previously stored key.

1. Define a _Sum of Product_ data type that can store one of two instructions: _store_ or _mangle_.

2. Change the type of your top level function and "follow the type errors".

Note: the solutions of this exercise build on the result of `2b` _not_ `2c`. You might want to building from `2c` if you want to have ticked off `3d` too.

Hint: if your round function is _pure_, you might want to consider using `mealy` to implement the instruction machine.

Hint: your wrapper functions likely need to be addressable.

### 3b
Add a _mangle mode_: encryption or decryption. Figure out a way to switch between encrypting and decrypting without a key reschedule.

Hint: the minimum and maximum bound of a number type can be requested with `minBound` and `maxBound` respectively.

### 3c
It is usually a good idea to add an instruction "doing nothing" to an instruction machine. Add it to your data structure, and use the warnings Clash/GHC produces to find the places in your code that need an update.

### 3d
Like `2c`, make your round function parameterizable in its delay.
