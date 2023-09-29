
import std::io

public export method main():
    io::println("Lotto")
    int x = lotto(49, 6)
    io::print("In Lotto 6of49 there are ")
    io::print(x)
    io::println("combintations possible.")

function lotto(int n, int k) -> (int result):
    int a = fac(n)
    int b = fac(k)
    int c = fac(n-k)
    return a / (b*c)

function fac(int n) -> (int result):
    if n <= 2:
        return n
    else:
        return n * fac(n-1)
