package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  
  test("orGate test") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(true)
    assert(out.getSignal === true, "or 4")
  }

  test("orGate 2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(true)
    assert(out.getSignal === true, "or 4")
  }
  
  def intToBinaryList(i : Int, listSize : Int) : List[Wire] = {
    val trailing = (i.toBinaryString.map {e => Wire(e == '1')}).toList
    val leading = List.fill(listSize - trailing.length)(Wire(false))
    leading ++ trailing
  }  
  
  test("simpleDemux") {
  	val in, c  = Wire(true)
  	val out0, out1 = Wire(false)
  	simpleDemux(in, c, out0, out1)
  	run
  	assert(out0.getSignal === false, "out 0")
  	assert(out1.getSignal  === true, "out 1")
  	
  }
  
  test("demux in = true") {
  	val inputSize = 5
  	val outputSize = Math.pow(2, inputSize).toInt
  	for( i <- 0 until outputSize) {
  	  val inputWires = intToBinaryList(i, inputSize)
  	  val outputWires = List.fill(outputSize)(Wire(false))
  	  demux(Wire(true), inputWires, outputWires)
  	  run
  	  val expectedOutputWires = List.fill(outputSize)(Wire(false)).updated(outputSize -1 -i, Wire(true))
  	  assert(outputWires === expectedOutputWires)
  	}
  }
  
  test("demux in = false") {
  	val inputSize = 5
  	val outputSize = Math.pow(2, inputSize).toInt
  	for( i <- 0 until outputSize) {
  	  val inputWires = intToBinaryList(i, inputSize)
  	  val outputWires = List.fill(outputSize)(Wire(false))
  	  demux(Wire(false), inputWires, outputWires)
  	  run
  	  val expectedOutputWires = List.fill(outputSize)(Wire(false))
  	  assert(outputWires === expectedOutputWires)
  	}
  }
  
}
