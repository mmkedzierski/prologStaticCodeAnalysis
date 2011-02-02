# Goal of this program

This program (hereinafter referred to as the **metaprogram**) is
designed to analyze programs written in Prolog.
From the assumption, the analyzed program should be a syntactically correct
program in Prolog - otherwise an error will be printed
and the metaprogram will terminate.

From the program provided for analysis, the metaprogram extracts the following information:

- list of constants appearing in the program
- list of defined predicates
- information about the program's layering:
  - division of predicates into layers if the program is layered or else
  - information about the reason for the non-layeness of the program
- standard formatted content of the program
The headers of the above information pieces are logged with '***' prefix (without quotes).

In addition, the metaprogram detects the following (potential) errors in the analyzed one
program or method of calling:

- incorrectly calling the program (incorrect arguments)
- inability to open the input file
- empty programs (no declarations)
- syntax error (with the indication of the violating line)
- so-called singleton variables - variables appearing only once in a clause
- non-standard predicates appearing in clauses, and undefined ones
  in the content of the program

Errors (warnings) are signaled by messages starting with from '###' characters (without quotes).


# Compilation

The program should be compiled using the `make` command issued in the current directory.
It creates a `metaprogram` file, which is an executable file.

# Usage

    ./metaprogram <in> [out]

where:

- `in` - the name of the Prolog program to analyze (mandatory argument)
- `out` - the name of the output file (optional argument; stdout is chosen if missing)
