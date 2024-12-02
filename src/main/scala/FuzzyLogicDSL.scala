sealed trait FuzzyClassOperation

case class Class(
                  name: String,
                  superClass: Option[Class] = None,
                  methods: List[Method] = Nil,
                  vars: List[ClassVar] = Nil
                ) extends FuzzyClassOperation

case class ClassVar(name: String, varType: VarType) extends FuzzyClassOperation
case class Method(name: String, params: List[Parameter], body: FuzzyOperation) extends FuzzyClassOperation
case class Parameter(name: String, paramType: String)
case class CreateNew(clazz: Class)

sealed trait VarType
case object StringType extends VarType
case object DoubleType extends VarType
case object SetType extends VarType

sealed trait FuzzyValue
case class FuzzyNumber(value: Double) extends FuzzyValue
case class FuzzyString(value: String) extends FuzzyValue
case class FuzzySetValue(set: FuzzySet) extends FuzzyValue

sealed trait FuzzyOperation

sealed trait FuzzyGateOperation extends FuzzyOperation

case class Input(name: String, value: Option[Double] = None) extends FuzzyGateOperation
case class ADD(a: FuzzyGateOperation, b: FuzzyGateOperation) extends FuzzyGateOperation
case class MULT(a: FuzzyGateOperation, b: FuzzyGateOperation) extends FuzzyGateOperation
case class XOR(a: FuzzyGateOperation, b: FuzzyGateOperation) extends FuzzyGateOperation

case class FuzzyGate(name: String, operation: FuzzyGateOperation)

object FuzzyOperations {
  def round(value: Double, precision: Int = 2): Double = {
    val scale = Math.pow(10, precision)
    Math.round(value * scale) / scale
  }

  def add(a: Double, b: Double): Double = Math.min(1.0, round(a + b))
  def mult(a: Double, b: Double): Double = round(a * b)
  def xor(a: Double, b: Double): Double = round(Math.abs(a - b))
}

object FuzzyGateEvaluator {
  def evaluate(gate: FuzzyGate, inputs: Map[String, Double]): Double = {
    def evalOperation(op: FuzzyGateOperation): Double = op match {
      case Input(name, _) => inputs.getOrElse(name, throw new IllegalArgumentException(s"Input $name is not defined in the scope"))
      case ADD(a, b) => FuzzyOperations.add(evalOperation(a), evalOperation(b))
      case MULT(a, b) => FuzzyOperations.mult(evalOperation(a), evalOperation(b))
      case XOR(a, b) => FuzzyOperations.xor(evalOperation(a), evalOperation(b))
    }
    evalOperation(gate.operation)
  }

  def evaluateMethod(instance: CreateNew, methodName: String, args: Map[String, FuzzyValue]): FuzzyValue = {
    val clazz = instance.clazz
    val method = findMethod(clazz, methodName).getOrElse(throw new IllegalArgumentException(s"Method $methodName not found in class ${clazz.name}"))
    val scope = args
    evaluateMethodBody(method.body, scope)
  }

  private def findMethod(clazz: Class, methodName: String): Option[Method] = {
    clazz.methods.find(_.name == methodName).orElse(clazz.superClass.flatMap(findMethod(_, methodName)))
  }

  private def evaluateMethodBody(body: FuzzyOperation, scope: Map[String, FuzzyValue]): FuzzyValue = {
    body match {
      case op: FuzzyGateOperation => FuzzyNumber(evaluateGateOperation(op, scope))
      case op: FuzzySetOperation => FuzzySetValue(evaluateSetOperation(op, scope))
      case AlphaCut(setOp, alphaOp) =>
        val setA = evaluateSetOperation(setOp, scope)
        val alphaValue = evaluateGateOperation(alphaOp, scope)
        val elements = FuzzySetOperations.alphaCut(setA, alphaValue)
        FuzzyString(elements.mkString(","))
    }
  }

  private def evaluateGateOperation(op: FuzzyGateOperation, scope: Map[String, FuzzyValue]): Double = op match {
    case Input(name, _) =>
      scope.get(name) match {
        case Some(FuzzyNumber(value)) => value
        case _ => throw new IllegalArgumentException(s"Input $name is not defined as a number in the scope")
      }
    case ADD(a, b) => FuzzyOperations.add(evaluateGateOperation(a, scope), evaluateGateOperation(b, scope))
    case MULT(a, b) => FuzzyOperations.mult(evaluateGateOperation(a, scope), evaluateGateOperation(b, scope))
    case XOR(a, b) => FuzzyOperations.xor(evaluateGateOperation(a, scope), evaluateGateOperation(b, scope))
  }

  private def evaluateSetOperation(op: FuzzySetOperation, scope: Map[String, FuzzyValue]): FuzzySet = {
    op match {
      case SetInput(name) =>
        scope.get(name) match {
          case Some(FuzzySetValue(set)) => set
          case _ => throw new IllegalArgumentException(s"Set $name is not defined as a set in the scope")
        }
      case Union(a, b) =>
        val setA = evaluateSetOperation(a, scope)
        val setB = evaluateSetOperation(b, scope)
        FuzzySetOperations.union(setA, setB)
      case Intersection(a, b) =>
        val setA = evaluateSetOperation(a, scope)
        val setB = evaluateSetOperation(b, scope)
        FuzzySetOperations.intersection(setA, setB)
      case Complement(a) =>
        val setA = evaluateSetOperation(a, scope)
        FuzzySetOperations.complement(setA)
      case AddSets(a, b) =>
        val setA = evaluateSetOperation(a, scope)
        val setB = evaluateSetOperation(b, scope)
        FuzzySetOperations.add(setA, setB)
      case MultSets(a, b) =>
        val setA = evaluateSetOperation(a, scope)
        val setB = evaluateSetOperation(b, scope)
        FuzzySetOperations.mult(setA, setB)
      case XorSets(a, b) =>
        val setA = evaluateSetOperation(a, scope)
        val setB = evaluateSetOperation(b, scope)
        FuzzySetOperations.xor(setA, setB)
    }
  }
}

sealed trait FuzzySetOperation extends FuzzyOperation

case class SetInput(name: String) extends FuzzySetOperation
case class AddSets(setA: FuzzySetOperation, setB: FuzzySetOperation) extends FuzzySetOperation
case class MultSets(setA: FuzzySetOperation, setB: FuzzySetOperation) extends FuzzySetOperation
case class XorSets(setA: FuzzySetOperation, setB: FuzzySetOperation) extends FuzzySetOperation
case class AlphaCut(setA: FuzzySetOperation, alpha: FuzzyGateOperation) extends FuzzyOperation
case class Union(setA: FuzzySetOperation, setB: FuzzySetOperation) extends FuzzySetOperation
case class Intersection(setA: FuzzySetOperation, setB: FuzzySetOperation) extends FuzzySetOperation
case class Complement(setA: FuzzySetOperation) extends FuzzySetOperation

// Set Operations
case class Element(name: String, value: Double)

case class FuzzySet(name: String, elements: List[Element]) {
  def eval(): List[Element] = elements
}

object FuzzySetOperations {

  def round(value: Double, precision: Int = 2): Double = {
    val scale = Math.pow(10, precision)
    Math.round(value * scale) / scale
  }

  def elementMap(set: FuzzySet): Map[String, Double] = {
    set.elements.map(e => e.name -> e.value).toMap
  }

  def union(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.max(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_UNION_${setB.name}", newElements)
  }

  def intersection(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.min(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_INTERSECTION_${setB.name}", newElements)
  }

  def complement(setA: FuzzySet): FuzzySet = {
    val newElements = setA.elements.map {
      case Element(nameA, valueA) =>
        Element(nameA, round(1 - valueA))
    }
    FuzzySet(s"${setA.name}_COMPLEMENT", newElements)
  }

  def add(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.min(1.0, valueA + valueB)))
    }.toList

    FuzzySet(s"${setA.name}_ADD_${setB.name}", newElements)
  }

  def mult(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(valueA * valueB))
    }.toList

    FuzzySet(s"${setA.name}_MULT_${setB.name}", newElements)
  }

  def xor(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.max(valueA, valueB) - Math.min(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_XOR_${setB.name}", newElements)
  }

  def alphaCut(setA: FuzzySet, alpha: Double): List[String] = {
    setA.elements.collect {
      case Element(nameA, valueA) if valueA >= alpha => nameA
    }
  }
}

case class FuzzySetClass(
                          name: String,
                          superClass: Option[Class] = None,
                          methods: List[Method] = Nil,
                          vars: List[ClassVar] = Nil
                        ) extends FuzzyClassOperation

case class FuzzySetVar(name: String, varType: VarType, elements: List[Element]) extends FuzzyClassOperation

import scala.collection.mutable
object FuzzyLogicDSL {
  val gates: mutable.Map[String, FuzzyGate] = mutable.Map()
  val inputScope: mutable.Map[String, mutable.Map[String, Double]] = mutable.Map()
  val classInstances: mutable.Map[String, CreateNew] = mutable.Map()
  val instanceScope: mutable.Map[String, mutable.Map[String, FuzzyValue]] = mutable.Map() // Scope for class instances

  def Assign(gate: FuzzyGate): Unit = {
    gates += (gate.name -> gate)
  }

  def AssignInput(gateName: String, input: Input, value: Double): Unit = {
    val currentScope = inputScope.getOrElseUpdate(gateName, mutable.Map())
    currentScope += (input.name -> value)
  }

  def Scope(gate: FuzzyGate, inputAssignment: (Input, Double)): Unit = {
    AssignInput(gate.name, inputAssignment._1, inputAssignment._2)
  }

  def ScopeInstance(instance: CreateNew, variable: String, value: FuzzyValue): Unit = {
    val instanceName = instance.clazz.name
    val currentScope = instanceScope.getOrElseUpdate(instanceName, mutable.Map())
    currentScope += (variable -> value)
  }

  def TestGate(gateName: String, expectedResult: Double): Boolean = {
    val gate = gates.getOrElse(gateName, throw new IllegalArgumentException(s"Gate $gateName not found"))
    val inputValues = inputScope.getOrElse(gateName, throw new IllegalArgumentException(s"Input values for $gateName not found"))
    val result = FuzzyGateEvaluator.evaluate(gate, inputValues.toMap)
    result == expectedResult
  }

  def CreateInstance(clazz: Class): CreateNew = {
    val instance = CreateNew(clazz)
    classInstances += (clazz.name -> instance)
    instance
  }

  def Invoke(instance: CreateNew, methodName: String, argNames: List[String]): FuzzyValue = {
    val scope = instanceScope.getOrElse(instance.clazz.name, throw new IllegalArgumentException(s"Instance scope for ${instance.clazz.name} not found"))
    val args: Map[String, FuzzyValue] = argNames.map { argName =>
      scope.get(argName) match {
        case Some(value) => argName -> value
        case None => throw new IllegalArgumentException(s"Input $argName is not defined in the scope for instance ${instance.clazz.name}")
      }
    }.toMap

    FuzzyGateEvaluator.evaluateMethod(instance, methodName, args)
  }
}

object Main extends App {
  import FuzzyLogicDSL._
  val baseSetClass = Class(
    name = "BaseSet",
    methods = List(
      Method(
        name = "unionMethod",
        params = List(Parameter("setA", "set"), Parameter("setB", "set")),
        body = Union(SetInput("setA"), SetInput("setB"))
      ),
      Method(
        name = "intersectionMethod",
        params = List(Parameter("setA", "set"), Parameter("setB", "set")),
        body = Intersection(SetInput("setA"), SetInput("setB"))
      )
    ),
    vars = List(ClassVar("v1", SetType))
  )

  // Create an instance of the base set class
  val baseSetInstance = CreateInstance(baseSetClass)

  // Using ScopeInstance for the instance variables
  ScopeInstance(baseSetInstance, "setA", FuzzySetValue(FuzzySet("setA", List(Element("x1", 0.5), Element("x2", 0.7)))))
  ScopeInstance(baseSetInstance, "setB", FuzzySetValue(FuzzySet("setB", List(Element("x1", 0.3), Element("x3", 0.8)))))

  // Invoke unionMethod on the baseSetInstance
  val unionResult = Invoke(baseSetInstance, methodName = "unionMethod", argNames = List("setA", "setB"))
  unionResult match {
    case FuzzySetValue(set) => println(s"Result of invoking unionMethod on BaseSet instance: ${set.elements}")
    case _ => println("Unexpected result type")
  }

  // Invoke intersectionMethod on the baseSetInstance
  val intersectionResult = Invoke(baseSetInstance, methodName = "intersectionMethod", argNames = List("setA", "setB"))
  intersectionResult match {
    case FuzzySetValue(set) => println(s"Result of invoking intersectionMethod on BaseSet instance: ${set.elements}")
    case _ => println("Unexpected result type")
  }
}
