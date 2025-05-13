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
        x = (10 + 2) * 3 / 4 - 5 % 2;
        greet = "hello " + "world";

        truth = true and false or !false;

        // Block scope
        outer = 1;
        {
          inner = outer + 1;
        }

        // Control flow with if / else if / else
        if x > 10 {
          status = "big";
        } else if x > 0 {
          status = "positive";
        } else {
          status = "non-negative";
        }

        // While loop
        i = 0; sum = 0;
        while i < 5 {
          i = i + 1;
          sum = sum + i;
        }
        ```

        Running `cargo run main.neu` would then execute this script.
