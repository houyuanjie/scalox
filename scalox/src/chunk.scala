package scalox

import scala.collection.mutable
import scala.util.Try

enum OpCode(val length: Int) {
  case Constant extends OpCode(OpCode.CONSTANT_INSTRUCTION_LENGTH)
  case Add      extends OpCode(OpCode.SIMPLE_INSTRUCTION_LENGTH)
  case Subtract extends OpCode(OpCode.SIMPLE_INSTRUCTION_LENGTH)
  case Multiply extends OpCode(OpCode.SIMPLE_INSTRUCTION_LENGTH)
  case Divide   extends OpCode(OpCode.SIMPLE_INSTRUCTION_LENGTH)
  case Negate   extends OpCode(OpCode.SIMPLE_INSTRUCTION_LENGTH)
  case Return   extends OpCode(OpCode.SIMPLE_INSTRUCTION_LENGTH)

  val byte = ordinal.toByte
}

object OpCode {
  final val CONSTANT_INSTRUCTION_LENGTH = 2
  final val INVOKE_INSTRUCTION_LENGTH   = 3
  final val SIMPLE_INSTRUCTION_LENGTH   = 1
  final val BYTE_INSTRUCTION_LENGTH     = 2
  final val JUMP_INSTRUCTION_LENGTH     = 3

  def of(byte: Byte) = OpCode.fromOrdinal(byte.toInt)
}

final class Chunk(
    initCode: Seq[Byte] = Seq.empty,
    initLines: Seq[Int] = Seq.empty,
    initConstants: Seq[Value] = Seq.empty
) {
  private val code: mutable.Buffer[Byte]       = mutable.ArrayBuffer.from(initCode)
  private val lines: mutable.Buffer[Int]       = mutable.ArrayBuffer.from(initLines)
  private val constants: mutable.Buffer[Value] = mutable.ArrayBuffer.from(initConstants)

  def count = this.code.length

  def writeByte(byte: Byte, line: Int): Unit = {
    this.code += byte
    this.lines += line
  }

  def write(opc: OpCode, line: Int): Unit = {
    writeByte(opc.byte, line)
  }

  def readByte(ip: Int) = this.code(ip)

  def addConstant(value: Value) = {
    constants += value
    constants.length - 1
  }

  def readConstant(index: Int) = this.constants(index)

  final case class DisasmInstruction(
      offset: Int,
      line: Int,
      op: Either[String, OpCode]
  )

  def disasmInstruction(offset: Int) = {
    val byte = this.code(offset)
    val line = this.lines(offset)
    val op   = Try(Right(OpCode.of(byte)))
      .getOrElse(Left(s"Unknown opcode ${byte}"))

    new DisasmInstruction(offset, line, op)
  }

  def printDisasmInstruction(inst: DisasmInstruction): Unit = {
    val offset = inst.offset

    val string = inst.op match
      case Left(msg)              => msg
      case Right(OpCode.Constant) =>
        val index = this.code(offset + 1)
        f"let cnst_${index}%d = ${constants(index).show}"
      case Right(opc) => opc

    println(f"// ${offset}%04d (line ${inst.line}%4d) ${string}")
  }

  def printDisasmInstruction(offset: Int): Unit = {
    val inst = disasmInstruction(offset)

    printDisasmInstruction(inst)
  }

  def disasmChunk = {
    val ops = new mutable.ArrayBuffer[DisasmInstruction](count)

    var offset = 0

    while offset < count do
      val inst = disasmInstruction(offset)

      ops += inst

      inst.op match
        case Left(_) =>
          offset += 1
        case Right(opc) =>
          offset += opc.length

    ops.toSeq
  }

  def printDisasmChunk(name: String): Unit = {
    println(s"// start <${name}>")

    disasmChunk.foreach(printDisasmInstruction)

    println(s"// end <${name}>")
  }
}
