This repository is my implementation of the C compiler based on [Nora Sandler
posts](https://norasandler.com/). My goal was to get more experience in Rust,
assembly and C.

### Usage

```sh
cargo run -- <C-source-file> > program.s
```

This will print assembly code on stdout.

You can also create binary using gcc and run it with shell script.

```sh
./compile.sh program.s
```

### Example

Print "Hello" using std putchar.

main.c
```c
int main() {
    putchar(72);
    putchar(101);
    putchar(108);
    putchar(108);
    putchar(111);
    putchar(10);
}
```

Run

```sh
$ cargo run -- main.c > main.s && ./compile.sh main.s
Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.01s
Running `target/debug/compiler main.c`
Running gcc on main.s
Executing ./program
Hello
Exit code: 0
```

### Final toughts

As I finished Nora's tutorial, I decided, "Why wouldn't I develop a whole C
compiler insted of just this subset?". 

I have messed around and found out that I should use some kind of intermediate
representation insted of AST. So its began. 💀

I decided that I will write my own parser (not a good idea). This project was
supposed to be a compiler, but I fell into parsing pitfall. This was not
enjoyable (sorry parser enjoyers). You can see my attempt (failed
miserably) on the parser branch. This was the moment when I decided that I had
enough.

Although I failed at my parser, I gained some valuable experience, especially
with Rust's `Iterator`, `Option` and `Result<T, E>` methods and custom `Error`
implementation (code may not ilustrate this 🗿).

Additionally, Rust doesn't have a good tool for parameterized tests (debugging was hell), though this might be
skill issue on my part.
