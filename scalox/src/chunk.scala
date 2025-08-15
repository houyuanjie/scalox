package scalox

import scala.collection.mutable
import scala.util.Try

enum OpCode {
  case Constant
  case Return

  val byte = ordinal.toByte
}

object OpCode {

  def of(byte: Byte) = OpCode.fromOrdinal(byte.toInt)

}

final class Chunk(
    initCode: Seq[Byte] = Seq.empty,
    initLines: Seq[Int] = Seq.empty,
    initConstants: Seq[Value] = Seq.empty
) {
  val code: mutable.Buffer[Byte] = mutable.ArrayBuffer.from(initCode)
  val lines: mutable.Buffer[Int] = mutable.ArrayBuffer.from(initLines)
  val constants: mutable.Buffer[Value] = mutable.ArrayBuffer.from(initConstants)

  def count = code.length

  def writeByte(byte: Byte, line: Int): Unit = {
    code += byte
    lines += line
  }

  def write(opc: OpCode, line: Int): Unit = {
    writeByte(opc.byte, line)
  }

  def addConstant(value: Value) = {
    constants += value
    constants.length - 1
  }

  final case class Disasm(offset: Int, line: Int, op: Either[String, OpCode])

  def disasm = {
    val ops = new mutable.ArrayBuffer[Disasm](count)

    var offset = 0

    while offset < count do
      val line = lines(offset)
      val byte = code(offset)
      val op = Try(Right(OpCode.of(byte)))
        .getOrElse(Left(s"Unknown opcode ${byte}"))

      ops += new Disasm(offset, line, op)

      op match
        case Left(_) =>
          offset += 1
        case Right(OpCode.Constant) =>
          offset += 2
        case Right(OpCode.Return) =>
          offset += 1

    ops.toSeq
  }

  def printDisasm(name: String): Unit = {
    println(s"====== ${name}")

    disasm.foreach { case Disasm(offset, line, op) =>
      val string = op match
        case Left(msg) => msg
        case Right(OpCode.Constant) =>
          val index = code(offset + 1)
          f"set constants[${index}%d] = ${constants(index).show}"
        case Right(opc) => opc

      println(f"${offset}%04d (line ${line}%4d) ${string}")
    }
  }

}
