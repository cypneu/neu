# Neu Programming Language

Neu is a new programming language under active development, currently focused on learning language design and Rust implementation practices. It serves as a personal educational project intended primarily for gaining a deeper understanding of interpreters, compilers, and Rust programming.
It is a simple, dynamically-typed programming language, currently interpreted through a tree-walk interpreter approach inspired by the excellent resource ["Crafting Interpreters"](https://craftinginterpreters.com/) by Robert Nystrom.

## Running Neu

Neu is built using Rust, so you'll need the Rust toolchain (including `cargo`) installed. If you don't have it, you can get it from [rustup.rs](https://rustup.rs/).

1. **Clone the repository**
2. **Choose how to run Neu:**

    * **Interactive REPL (Read-Eval-Print Loop):**
        To start an interactive session where you can type Neu commands directly and see the results, run:

        ```bash
        cargo run
        ```

        This will compile the project and launch the REPL. You can then enter Neu code line by line.

    * **Interpret a File:**
        To execute a Neu script saved in a file, pass the file path as an argument to `cargo run`. For example, to run a file named `main.neu`:

        ```bash
        cargo run main.neu
        ```

        This will compile the project and then interpret the provided file.

        You could create the `main.neu` file mentioned above with the following content:

        ```neu
        // Arithmetic
        x = 5 * (3 + 4) % 3;
        y = 1234.45;
        z = x + y;

        // String ops
        s1 = "asdf";
        s2 = "fdas";
        s3 = s1 + s2;

        // Boolean
        a = true;
        b = false;

        // Scope
        xouter = 5;
        {
          xinner1 = 4;
          {
            xinner2 = xouter + 1;
            xinner1 = 5;
          }
          x3 = xinner1 + xouter;
        }
        ```

        Running `cargo run main.neu` would then execute this script.
