
import std::io

public export method main():
    io::println("Arithmetics Test")
    int x = 13
    int y = 74
    int z = ((x + (y * y)) / x) - x
    io::print("x = ")
    io::println(x)
    io::print("y = ")
    io::println(y)
    io::print("((x + (y * y)) / x) - x = ")
    io::println(z)
