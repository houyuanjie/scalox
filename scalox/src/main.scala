package scalox

@main
def main(): Unit =
  val chunk = new Chunk()

  var index = chunk.addConstant(new Value.Number(1.2))
  chunk.write(OpCode.Constant, line = 123)
  chunk.writeByte(index.toByte, line = 123)

  index = chunk.addConstant(new Value.Number(3.4))
  chunk.write(OpCode.Constant, line = 123)
  chunk.writeByte(index.toByte, line = 123)

  chunk.write(OpCode.Add, line = 123)

  index = chunk.addConstant(new Value.Number(5.6))
  chunk.write(OpCode.Constant, line = 123)
  chunk.writeByte(index.toByte, line = 123)

  chunk.write(OpCode.Divide, line = 123)
  chunk.write(OpCode.Negate, line = 123)

  chunk.write(OpCode.Return, line = 123)

  val vm = new VM(debug = true)
  vm.interpret(chunk)

  chunk.printDisasmChunk("Test")
