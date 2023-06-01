
import std::io

public export method main():
    io::println("Fibonacci Test")
    int x = 10
    int f = fib(x)
    io::print("fib(")
    io::print(x)
    io::print(") = ")
    io::println(f)

function fib(int n) -> (int r):
    if n < 2:
        return n
    else:
        return n + fib(n-1)
