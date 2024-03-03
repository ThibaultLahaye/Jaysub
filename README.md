# J< Project
*pronounced as 'jay-sub'*
## Project Description
This project implements a simple exoteric programming language called J< (pronounced "Jay sub") using [OCaml]() as its metalanguage. J< is unique as it operates as a *time-reversible* programming language, allowing computation to not only progress from input to output but also reverse, deriving inputs from outputs.

J< is inplemented using a [interpretation](https://en.wikipedia.org/wiki/Programming_language_implementation) approach and is heavily based on the design of the [Janus](https://en.wikipedia.org/wiki/Janus_(time-reversible_computing_programming_language)#:~:text=Janus%20is%20a%20time%2Dreversible,Tetsuo%20Yokoyama%20and%20Robert%20Gl%C3%BCck.) language. Though, this implementation only focuses on a subset of its features (hence the name "jay-_sub_"). 

The grammar of J< is outlinded below. It facilitates program- and procedures declarations, and various other statements. 
```
Prog ::= Decl∗; Proc+
Decl ::= id
Proc ::= procedure id { Stat }
Stat ::= Stat ; Stat
       | id Modi Expr
       | id <=> id
       | if Cond then { Stat } else { Stat } fi Cond
       | call id
       | uncall id
       | skip
Modi ::= += | −= | ∗= | /=
Expr ::= num | id | Expr + Expr | Expr − Expr | Expr ∗ Expr | Expr/Expr
Cond ::= Expr == Expr | Expr != Expr | Expr
```
For a more detailed look into the formal grammar and semantics of Janus, refer to the provided [resource](https://dl.acm.org/doi/10.1145/1244381.1244404).

<u>Language Features Supported:</u>
- Declaration of global program variables.
- Imperative assignment using "swap" and "modification" statements.
- Conditional structures including augmented if-statements.
- Looping constructs.
- Procedure definitions with corresponding calls and "uncalls".

<u>The interpreter offers various modes of operation:</u>
- Forward Evaluator (feval): Evaluates the program in a typical forward manner.
- Backward Evaluator (beval): Facilitates "uncomputing", deriving inputs from outputs.
- Program Inverter (invert): Reverses the program's logic.
- Optimizing Source-to-Source Compiler (optimizer): Enhances
    - program efficiency through:
    - Constant folding and peephole optimizations.
    - Dead code elimination.
    - Procedure inlining and removal of uncalls.

These interpreter modes can be accessed using the command desribed in the [CLI usage](#CLI-usage) section.

## How to Run
### Tools & Dependencies
This project requires a relativley minimum installation of [OCaml](https://ocaml.org/) (≥ 4.14.0). That includes installing the [Opam](https://opam.ocaml.org/) package manager, the compiler itself and a platform build system called [Dune](). Though, some developer comfort tools like [Utop](https://github.com/ocaml-community/utop), [Ocaml-lsp-server](https://github.com/ocaml/ocaml-lsp), [Odoc](https://github.com/ocaml/odoc) and [Ocamlformat](https://github.com/ocaml-ppx/ocamlformat) are nice to haves. The codebase was only tested on Arch linux. Check out my dotfiles how NeoVim was configures for OCaml development (ref. [Blogpost](https://nyinyithan.com/nvim-setup-ocaml/))

A detailed explanaition for getting up and running with Ocaml can be found [here](https://ocaml.org/docs/installing-ocaml). 

___TL;DR___ [READ THIS]
Ocaml and opam should be installed system wide using pacman. All other tools can either be installed globally as packages from the AUR, but can also be installed locally using opam switches (Recomended -> section COMMANDS). 

___TL;DR___ [READ THIS]
All the codebase specific dependencies, managed by an opam switch, are installed using `opam install dune menhir ppx_deriving` (more on this in the following section). As stated earlier, best not to install Dune using Pacman.

### Commands
To build the codebase use, first navigate into the jaysub directory. Then execute the following commands. 
The last command wil be further explained in the [CLI usage]() section.
```shell
# Installing the dependencies using an opam switch
opam install dune menhir ppx_deriving
# Set up the shell environment to work with the OCaml compiler and any installed packages managed by OPAM
eval $(opam config env)
# Build the codebase
dune build
# Run the build codebase (interpreter)
dune exec jaysub forward examples/fib.jsub
```
## CLI usage
The previous paragraph showed how the codebaes could be build. The last command `dune exec jaysub forward examples/fib.jsub` executes the built codebase (interpreter), passing a command `jaysub` to the interpreter. This command instructs the interpreter on the mode of operation and the file to interpret, with the output displayed in the terminal. The command structure is as follows:
```
jaysub [forward|backward|invert|optimize] <filename.jsub>
```
This command is implemented within the interpreter, dictating its mode of operation and specifying the file for interpretation.
