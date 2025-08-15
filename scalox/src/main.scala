package scalox

@main
def main(): Unit =
  val chunk = new Chunk()
  chunk.write(OpCode.Return, 123)
  println(s"chunk = ${chunk}")

  val constant = chunk.addConstant(new Value.Number(1.2))
  chunk.write(OpCode.Constant, 123)
  chunk.writeByte(constant.toByte, 123)

  chunk.printDisasm("test chunk")
