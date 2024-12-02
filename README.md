# CS474-HW2
CS474 HW2 fuzzy logic expansion

This project contains the new features required for CS474 HW2. The report.docx include the detail explanation of the implementation and the semantics of this DSL.

## Installation

### Prerequisites

To run this project, you'll need:
- **Scala 2.13.x**
- **SBT (Scala Build Tool)**
- **Git** for version control.

### Steps

1. **Clone the Repository**:
   Open a terminal and run the following command:
   ```bash
   git clone https://github.com/Holyhbsun/CS474-HW2.git
   cd CS474-HW2

2. **Build the Project**:
   You can use SBT to build the project. In the project directory, run:
   ```bash
    sbt compile

3. **Run the Program**:
   To run the program, use:
   ```bash
    sbt run

4. **Run Tests**:
   To run the test, use:
   ```bash
    sbt test

## Usage
The following are the new features implemented in this project.
### Object Design
- **Class**: Represents the blueprint of an object in the DSL.
- **SuperClass**: Allows possible inheritance.
- **Methods**: A list of Method instances defining the name, params used in the class, and the actual operations used in this method.
- **Vars**: A list of ClassVar instances representing the variables used in the class.
- **CreateNew**: Creates a new instance of a class.
- **CreateInstance**: Create instances by getting the contents and the name of the clazz.
- **Invoke**: Invoke a method defined within a class on a specific instance.
- **ClassVar**: Represents a variable or attribute of a class.
- **Vartype**: Defines the types of variables allowed in the DSL.

### Evaluation
- **evaluateMethod**: Finds the method by the method name and then evaluates its body within the given scope.
- **evaluateMethodBody**: Matches the body of the method.
- **evaluateGateOperation**: Evaluates fuzzy gate operations by operands recursively.
- **evaluateSetOperation**: Evaluates fuzzy set operations by operands recursively, similar to evaluateGateOperation.
