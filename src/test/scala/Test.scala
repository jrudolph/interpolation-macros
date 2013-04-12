object Test extends App {
  import net.virtualvoid.interpolate.ATest._

  println(a"""
  This is the factorial function ${
    def factorial(n: Int): Int =
      if (n <= 1) n else (n * factorial(n - 1))

    factorial(3) } }" +
  """)

  println(
  a"""
   first
   ${ 1+1 }
   second
   ${ 2+2 }""")
}
