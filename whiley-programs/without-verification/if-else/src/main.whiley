
import std::io

public export method main():
    io::println("If-Else Test")
    int x = 13
    int y = 74
    int z = 3

    io::print("x = ")
    io::println(x)
    io::print("y = ")
    io::println(y)
    io::print("z = ")
    io::println(z)

    io::println("(x + y) + z == x + (y + z)")
    if (x + y) + z == x + (y + z):
        io::println("Right")

    io::println("(x + y) + z == x + (y + z)")
    if (x + y) + z == x + (y + z):
        io::println("Right again")
    else:
        io::println("Wrong")

    io::println("(x + y) * z == x + (y * z)")
    if (x + y) * z == x + (y * z):
        io::println("Right")
    else if (x + y) * z > x + (y * z):
        io::println("Wrong")
        io::println("(x + y) * z > x + (y * z)")
        io::println("Right")
