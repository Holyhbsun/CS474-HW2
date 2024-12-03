import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicDSL._
import FuzzySetOperations._

class FuzzyLogicDSLTest extends AnyFlatSpec with Matchers {

  behavior of "FuzzyLogicDSL"

  it should "correctly evaluate ADD operation in a class method" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "addMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = ADD(Input("p1"), Input("p2")) // This is a FuzzyGateOperation, which is acceptable
        )
      ),
      vars = List(ClassVar("v1", DoubleType))
    )
    val instance = CreateInstance(baseClass, "instanceAdd")

    ScopeInstance(instance, "p1", FuzzyNumber(0.4))
    ScopeInstance(instance, "p2", FuzzyNumber(0.5))

    val result = Invoke(instance, methodName = "addMethod", argNames = List("p1", "p2")) // 0.4+0.5
    result shouldEqual FuzzyNumber(0.9) // Since Invoke now returns FuzzyValue
  }

  it should "correctly evaluate MULT operation in a class method" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "multMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = MULT(Input("p1"), Input("p2"))
        )
      )
    )
    val instance = CreateInstance(baseClass, "instanceMult")

    ScopeInstance(instance, "p1", FuzzyNumber(0.6))
    ScopeInstance(instance, "p2", FuzzyNumber(0.5))

    val result = Invoke(instance, methodName = "multMethod", argNames = List("p1", "p2")) //0.6*0.5
    result shouldEqual FuzzyNumber(0.3)
  }

  it should "correctly evaluate XOR operation in a class method" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "xorMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = XOR(Input("p1"), Input("p2"))
        )
      )
    )
    val instance = CreateInstance(baseClass, "instanceXor")

    ScopeInstance(instance, "p1", FuzzyNumber(0.7))
    ScopeInstance(instance, "p2", FuzzyNumber(0.4))

    val result = Invoke(instance, methodName = "xorMethod", argNames = List("p1", "p2")) //0.7-0.4
    result shouldEqual FuzzyNumber(0.3)
  }

  it should "correctly invoke methods from nested superclasses" in {
    val nestedClass = Class(
      name = "Nested",
      methods = List(
        Method(
          name = "nestedMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = MULT(Input("p1"), Input("p2"))
        )
      )
    )

    val baseClass = Class(
      name = "Base",
      superClass = Some(nestedClass),
      methods = List(
        Method(
          name = "baseMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double"), Parameter("p3", "double")),
          body = ADD(ADD(Input("p1"), Input("p2")), Input("p3")) // p1 + p2 + p3
        )
      )
    )

    val derivedClass = Class(
      name = "Derived",
      superClass = Some(baseClass),
      methods = List(
        Method(
          name = "derivedMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = XOR(Input("p1"), Input("p2"))
        ),
        Method(
          name = "nestedMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = ADD(Input("p1"), Input("p2"))
        )
      )
    )

    val instance = CreateInstance(derivedClass, "instanceNested")

    ScopeInstance(instance, "p1", FuzzyNumber(0.6))
    ScopeInstance(instance, "p2", FuzzyNumber(0.5))
    ScopeInstance(instance, "p3", FuzzyNumber(0.4))

    val overriddenNestedResult = Invoke(instance, methodName = "nestedMethod", argNames = List("p1", "p2"))
    overriddenNestedResult shouldEqual FuzzyNumber(1.0) // 0.6 + 0.5 = 1.1, capped at 1.0

    val baseResult = Invoke(instance, methodName = "baseMethod", argNames = List("p1", "p2", "p3"))
    baseResult shouldEqual FuzzyNumber(1.0) // 0.6 + 0.5 + 0.4 = 1.5, capped at 1.0

    val derivedResult = Invoke(instance, methodName = "derivedMethod", argNames = List("p1", "p2"))
    derivedResult shouldEqual FuzzyNumber(0.1) // abs(0.6 - 0.5) = 0.1

    val baseInstance = CreateInstance(baseClass, "instanceBase")

    ScopeInstance(baseInstance, "p1", FuzzyNumber(0.6))
    ScopeInstance(baseInstance, "p2", FuzzyNumber(0.5))

    val originalNestedResult = Invoke(baseInstance, methodName = "nestedMethod", argNames = List("p1", "p2"))
    originalNestedResult shouldEqual FuzzyNumber(0.3) // 0.6 * 0.5 = 0.3
  }

  it should "correctly evaluate inherited method in derived class" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "multMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = MULT(Input("p1"), Input("p2"))
        )
      )
    )

    val derivedClass = Class(
      name = "Derived",
      superClass = Some(baseClass),
      methods = List(
        Method(
          name = "xorMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = XOR(Input("p1"), Input("p2"))
        )
      )
    )

    val instance = CreateInstance(derivedClass, "instanceDerived")

    ScopeInstance(instance, "p1", FuzzyNumber(0.7))
    ScopeInstance(instance, "p2", FuzzyNumber(0.3))

    val multResult = Invoke(instance, methodName = "multMethod", argNames = List("p1", "p2"))
    multResult shouldEqual FuzzyNumber(0.21) // 0.7*0.3

    val xorResult = Invoke(instance, methodName = "xorMethod", argNames = List("p1", "p2"))
    xorResult shouldEqual FuzzyNumber(0.4) // 0.7-0.3
  }


  it should "correctly handle FuzzyString parameters" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "stringMethod",
          params = List(Parameter("p1", "string"), Parameter("p2", "double")),
          body = ADD(Input("p2"), Input("p2"))
        )
      )
    )

    val instance = CreateInstance(baseClass, "instanceString")

    ScopeInstance(instance, "p2", FuzzyNumber(0.4))

    ScopeInstance(instance, "p1", FuzzyString("Hello World!"))

    val result = Invoke(instance, methodName = "stringMethod", argNames = List("p1", "p2")) //0.4+0.4, skip the string
    result shouldEqual FuzzyNumber(0.8)
  }

  behavior of "FuzzySet operations in class methods"

  it should "correctly evaluate ADD operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "addMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = AddSets(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceAdd")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.5), Element("x3", 0.4)))))

    val result = Invoke(instance, methodName = "addMethod", argNames = List("setA", "setB"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.7),
          Element("x2", 0.7),
          Element("x3", 0.4)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate MULT operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "multMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = MultSets(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceMult")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.6), Element("x2", 0.8)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.5), Element("x3", 0.4)))))

    val result = Invoke(instance, methodName = "multMethod", argNames = List("setA", "setB"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.3), //0.6*0.5
          Element("x2", 0.0),
          Element("x3", 0.0)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate XOR operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "xorMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = XorSets(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceXor")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.7), Element("x2", 0.3)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.4), Element("x3", 0.5)))))

    val result = Invoke(instance, methodName = "xorMethod", argNames = List("setA", "setB"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.3), // abs(0.7 - 0.4)
          Element("x2", 0.3), // abs(0.3 - 0.0)
          Element("x3", 0.5) // abs(0.0 - 0.5)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate complement operation on a fuzzy set in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "complementMethod",
          params = List(Parameter("setA", "set")),
          body = Complement(SetInput("setA"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceComplement")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.8)))))

    val result = Invoke(instance, methodName = "complementMethod", argNames = List("setA"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.8),
          Element("x2", 0.2)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate alpha-cut operation on a fuzzy set in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "alphaCutMethod",
          params = List(Parameter("setA", "set"), Parameter("alpha", "double")),
          body = AlphaCut(SetInput("setA"), Input("alpha"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceAlphacut")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.5), Element("x2", 0.7), Element("x3", 0.9)))))
    ScopeInstance(instance, "alpha", FuzzyNumber(0.6))

    val result = Invoke(instance, methodName = "alphaCutMethod", argNames = List("setA", "alpha"))

    result match {
      case FuzzyString(value) =>
        value shouldEqual "x2,x3"
      case _ => fail("Expected a FuzzyString")
    }
  }

  it should "correctly evaluate unionMethod in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "unionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Union(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceUnion")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.6), Element("x3", 0.5)))))

    val result = Invoke(instance, methodName = "unionMethod", argNames = List("setA", "setB"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.6), //max(0.2,0.6)
          Element("x2", 0.7),
          Element("x3", 0.5)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate intersectionMethod in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "intersectionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Intersection(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceIntersection")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.5), Element("x2", 0.7)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.3), Element("x3", 0.8)))))

    val result = Invoke(instance, methodName = "intersectionMethod", argNames = List("setA", "setB"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.3), //min(0.5,0.3)
          Element("x2", 0.0),
          Element("x3", 0.0)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate complementMethod in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "complementMethod",
          params = List(Parameter("setA", "set")),
          body = Complement(SetInput("setA"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instanceComplement")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7)))))

    val result = Invoke(instance, methodName = "complementMethod", argNames = List("setA"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.8),
          Element("x2", 0.3)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate nested set operations in a class method" in {
    val nestedSetClass = Class(
      name = "NestedSet",
      methods = List(
        Method(
          name = "nestedMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Complement(Union(SetInput("setA"), SetInput("setB")))
        )
      )
    )

    val instance = CreateInstance(nestedSetClass, "instanceNested")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.5)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.3)))))

    val result = Invoke(instance, methodName = "nestedMethod", argNames = List("setA", "setB"))

    result match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.5)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate inherited fuzzy set method in derived class" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "unionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Union(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val derivedSetClass = Class(
      name = "DerivedSet",
      superClass = Some(baseSetClass),
      methods = List(
        Method(
          name = "intersectionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Intersection(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(derivedSetClass, "instanceDerive")

    ScopeInstance(instance, "setA", FuzzySetValue(FuzzySet("A", List(Element("x1", 0.4), Element("x2", 0.6)))))
    ScopeInstance(instance, "setB", FuzzySetValue(FuzzySet("B", List(Element("x1", 0.5), Element("x3", 0.7)))))

    val unionResult = Invoke(instance, methodName = "unionMethod", argNames = List("setA", "setB"))
    unionResult match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.5),
          Element("x2", 0.6),
          Element("x3", 0.7)
        )
      case _ => fail("Expected a FuzzySetValue")
    }

    val intersectionResult = Invoke(instance, methodName = "intersectionMethod", argNames = List("setA", "setB"))
    intersectionResult match {
      case FuzzySetValue(set) =>
        set.elements shouldEqual List(
          Element("x1", 0.4),
          Element("x2", 0.0),
          Element("x3", 0.0)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

}