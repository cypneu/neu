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
        // Structs with fields & methods (instance + static)
        struct Counter {
          val,

          fn inc(self) {
            self.val = self.val + 1;
          }

          fn add(a, b) {
            return a + b;
          }
        }

        // Closures capture `x`
        fn make_adder(x) {
          fn add(y) { return x + y; }
          return add;
        }

        count = Counter { val: 0 };
        add5 = make_adder(5);

        sum = 0;
        for i in 0..3 {
          sum = Counter.add(sum, i); // call static method through type
          count.inc();               // mutate instance field via method
        }

        print("Counter(", count.val, "), sum =", add5(sum));
        ```

        Running `cargo run main.neu` would then execute this script.
