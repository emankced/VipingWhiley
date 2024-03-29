
import std::io

public export method main():
    io::println("Commutative")
    int x = 10
    int y = 3
    int z = -5
    int f = comm(x, y, z)
    io::print("comm(")
    io::print(x)
    io::print(", ")
    io::print(y)
    io::print(", ")
    io::print(z)
    io::print(") = ")
    io::println(f)

function comm(int a, int b, int c) -> (int result):
    return mul(a, mul(b,c))

function mul(int a, int b) -> (int result):
    return a * b
