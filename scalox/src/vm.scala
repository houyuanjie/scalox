package scalox

import scala.compiletime.uninitialized
import scala.annotation.tailrec
import scala.collection.mutable

enum InterpretResult {
  case Ok, CompileError, RuntimeError
}

final class VM(initStack: Seq[Value] = Seq.empty, var debug: Boolean = false) {
  private val stack: mutable.Stack[Value] = mutable.ArrayStack.from(initStack)

  private var chunk: Chunk = uninitialized
  private var ip: Int      = uninitialized

  def interpret(chunk: Chunk) = {
    this.chunk = chunk
    this.ip = 0

    run()
  }

  def resetStack(): Unit = {
    this.stack.clear()
  }

  def printStack(): Unit = {
    println(s"// stack: ${this.stack.mkString("[", ", ", "]")}")
  }

  @tailrec
  def run(): InterpretResult = {
    if debug then
      printStack()
      this.chunk.printDisasmInstruction(this.ip)

    val opc = OpCode.of(readNextByte())

    opc match
      case OpCode.Constant =>
        val constant = readNextConstant()
        this.stack.push(constant)
      case OpCode.Add =>
        val b = this.stack.pop().asInstanceOf[Value.Number]
        val a = this.stack.pop().asInstanceOf[Value.Number]
        this.stack.push(a + b)
      case OpCode.Subtract =>
        val b = this.stack.pop().asInstanceOf[Value.Number]
        val a = this.stack.pop().asInstanceOf[Value.Number]
        this.stack.push(a - b)
      case OpCode.Multiply =>
        val b = this.stack.pop().asInstanceOf[Value.Number]
        val a = this.stack.pop().asInstanceOf[Value.Number]
        this.stack.push(a * b)
      case OpCode.Divide =>
        val b = this.stack.pop().asInstanceOf[Value.Number]
        val a = this.stack.pop().asInstanceOf[Value.Number]
        this.stack.push(a / b)
      case OpCode.Negate =>
        val number = this.stack.pop().asInstanceOf[Value.Number]
        this.stack.push(-number)
      case OpCode.Return =>
        val retVal = this.stack.pop()
        println(s"return = ${retVal.show}")
        return InterpretResult.Ok

    run()
  }

  def readNextByte() = {
    val byte = this.chunk.readByte(this.ip)

    this.ip += 1

    byte
  }

  def readNextConstant() = {
    val index = readNextByte()

    this.chunk.readConstant(index)
  }
}
