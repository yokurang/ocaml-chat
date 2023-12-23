# Ahrefs Coding Challenge: One on one chat application in OCaml

## Installation

To run the project locally, please follow the following steps:

First, update your opam packages. Note that this dune project requires dune 3.1.2 or higher.

```bash
opam update
```

or

```bash
opam update && opam upgrade
```

Then, install the dependencies via opam:

```bash
opam install core async sexplib ppx_jane
```

or

```bash
opam install . --deps-only
```

Finally, build the project:

```bash
dune clean && dune build
```

## Usage

To run the server, run the following command:

```bash
dune exec -- one_on_one_chat server -port [port]
```

As an example, to start the server on port 8765, run the following command:

```bash
dune exec -- one_on_one_chat server -port 8765
```

To run the client, run the following command:

```bash
dune exec -- one_on_one_chat client -host [hostname] -port [port]
```

As an example, to start the client on host 127.0.0.1 and port 8765, run the following command:

```bash
dune exec -- one_on_one_chat client -host 127.0.0.1 -port 8765
```

## System Design

