
import std::io

public export method main():
    io::println("Fibonacci Test")
    int x = 10
    int f = fib(x)
    io::print("fib(")
    io::print(x)
    io::print(") = ")
    io::println(f)

function fib(int n) -> (int r)
// input must be positive
requires n >= 0
// return value can not be smaller than input
ensures r >= n
// return value is bigger than the input if n >= 2
ensures n >= 2 ==> r > n:
    if n < 2:
        return n
    else:
        return n + fib(n-1)
